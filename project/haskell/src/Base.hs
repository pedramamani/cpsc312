{-# LANGUAGE PatternSynonyms #-}

module Base
  ( Parsable (parse, unparse),
    Activity (Activity, label),
    Line (Line, indent, content),
    Time (Time, hour, minute),
    now,
    start,
    end,
    Date (Date, year, month, day),
    today,
    next,
    isBetween,
    Record (Record, activity, startTime, stopTime, date),
    duration,
    Node (Node, key, children),
    allKeys,
    find,
    visualize,
  )
where

import qualified Config
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Text.Printf as Printf
import qualified Text.Read as Read

class Parsable a where
  parse :: String -> a
  unparse :: a -> String

newtype Activity = Activity_ {label :: String} deriving (Eq, Ord, Show)

pattern Activity :: String -> Activity
pattern Activity l <-
  Activity_ l
  where
    Activity s
      | null l = error "Activity label cannot be empty"
      | not $ all isValidCharacter l = error $ "Activity label " ++ show s ++ " contains an invalid character"
      | otherwise = Activity_ l
      where
        l = unwords $ words s

instance Parsable Activity where
  parse = Activity
  unparse = label

isValidCharacter :: Char -> Bool
isValidCharacter c = Char.isAlphaNum c || c == ' ' || c == '-' || c == '.'

data Line a = Line_ {indent :: Int, content :: a} deriving (Eq, Show)

pattern Line :: (Parsable a) => Int -> a -> Line a
pattern Line i c <-
  Line_ i c
  where
    Line i c
      | i < 0 = error $ "Line indent " ++ show i ++ " is negative"
      | otherwise = Line_ i c

instance (Parsable a) => Parsable (Line a) where
  parse s
    | '\n' `elem` s = error $ "Line " ++ show s ++ " contains multiple lines"
    | rem i Config.tab /= 0 = error $ "Line " ++ show s ++ " is indented incorrectly"
    | otherwise = Line (div i Config.tab) $ parse r
    where
      (l, r) = span (== ' ') s
      i = length l

  unparse (Line i c) = replicate (Config.tab * i) ' ' ++ unparse c

data Time = Time_ {hour :: Int, minute :: Int} deriving (Eq, Ord, Show)

pattern Time :: Int -> Int -> Time
pattern Time h m <-
  Time_ h m
  where
    Time h m
      | h < 0 || h >= 24 = error $ "Hour " ++ show h ++ " is invalid"
      | m < 0 || m >= 60 = error $ "Minute " ++ show m ++ " is invalid"
      | otherwise = Time_ h m

now :: IO Time
now = do
  t <- Clock.getCurrentTime
  tz <- LocalTime.getCurrentTimeZone
  let lt = LocalTime.localTimeOfDay $ LocalTime.utcToLocalTime tz t
  return $ Time (LocalTime.todHour lt) (LocalTime.todMin lt)

start :: Time
start = Time 0 0

end :: Time
end = Time 23 59

instance Parsable Time where
  parse s
    | length st /= 2 = error $ "Time " ++ show s ++ " does not follow hour:minute format"
    | Maybe.isNothing h = error $ "Hour " ++ show sh ++ " is not an integer"
    | Maybe.isNothing m = error $ "Minute " ++ show sm ++ " is not an integer"
    | otherwise = Time (Maybe.fromJust h) (Maybe.fromJust m)
    where
      st = Split.splitOn ":" s
      [sh, sm] = st
      h = Read.readMaybe sh
      m = Read.readMaybe sm

  unparse (Time h m) = Printf.printf "%02d" h ++ ":" ++ Printf.printf "%02d" m

data Date = Date_ {year :: Int, month :: Int, day :: Int} deriving (Eq, Ord, Show)

pattern Date :: Int -> Int -> Int -> Date
pattern Date y m d <-
  Date_ y m d
  where
    Date y m d
      | y < 2000 || y >= 2100 = error $ "Year " ++ show y ++ " is invalid"
      | m < 1 || m > 12 = error $ "Month " ++ show m ++ " is invalid"
      | d < 1 || d > 31 = error $ "Day " ++ show m ++ " is invalid"
      | otherwise = Date_ y m d

toDate :: Calendar.Day -> Date
toDate cd = Date (fromIntegral y) m d
  where
    (y, m, d) = Calendar.toGregorian cd

today :: IO Date
today = do
  t <- Clock.getCurrentTime
  tz <- LocalTime.getCurrentTimeZone
  return $ toDate $ LocalTime.localDay $ LocalTime.utcToLocalTime tz t

next :: Date -> Date
next (Date y m d) = toDate $ Calendar.addDays 1 $ Calendar.fromGregorian (toInteger y) m d

isBefore :: Date -> Date -> Bool
isBefore (Date y1 m1 d1) (Date y2 m2 d2)
  | y1 < y2 = True
  | y1 == y2 && m1 < m2 = True
  | y1 == y2 && m1 == m2 && d1 <= d2 = True
  | otherwise = False

isBetween :: Date -> Date -> Date -> Bool
isBetween d1 d2 d = isBefore d1 d && isBefore d d2

instance Parsable Date where
  parse s
    | length ds /= 3 = error $ "Date " ++ show s ++ " does not follow year-month-day format"
    | Maybe.isNothing y = error $ "Year " ++ show sy ++ " is not an integer"
    | Maybe.isNothing m = error $ "Month " ++ show sm ++ " is not an integer"
    | Maybe.isNothing d = error $ "Day " ++ show sd ++ " is not an integer"
    | otherwise = Date (Maybe.fromJust y) (Maybe.fromJust m) (Maybe.fromJust d)
    where
      ds = Split.splitOn "-" s
      [sy, sm, sd] = ds
      y = Read.readMaybe sy
      m = Read.readMaybe sm
      d = Read.readMaybe sd

  unparse (Date y m d) = show y ++ "-" ++ Printf.printf "%02d" m ++ "-" ++ Printf.printf "%02d" d

data Record = Record_ {startTime :: Time, stopTime :: Time, date :: Date, activity :: Activity} deriving (Eq, Ord, Show)

pattern Record :: Time -> Time -> Date -> Activity -> Record
pattern Record st et d a <-
  Record_ st et d a
  where
    Record st et d a
      | et < st = error $ "Activity start time " ++ unparse st ++ " must precede its end time " ++ unparse et
      | otherwise = Record_ st et d a

duration :: Record -> Int
duration r = 60 * (hour et - hour st) + minute et - minute st
  where
    st = startTime r
    et = stopTime r

instance Parsable Record where
  parse s
    | length sr /= 4 = error $ "Record " ++ show s ++ " has invalid format"
    | otherwise = Record st et d a
    where
      sr = Split.splitOn Config.sep s
      [sst, set, sd, sa] = sr
      st = parse sst
      et = parse set
      d = parse sd
      a = parse sa

  unparse (Record st et d a) = unparse st ++ Config.sep ++ unparse et ++ Config.sep ++ unparse d ++ Config.sep ++ unparse a

data Node a = Node {key :: a, children :: [Node a]} deriving (Eq, Show)

instance (Parsable a, Ord a) => Parsable (Node a) where
  parse s
    | nr /= 1 = error $ "Tree must have 1 root node, not " ++ show nr
    | length ls /= length (allKeys t) = error "Tree has repeated keys"
    | otherwise = t
    where
      ls = map parse $ lines s
      nr = length $ filter ((== 0) . indent) ls
      t = parseHelper ls

  unparse = unparseHelper 0

parseHelper :: (Parsable a) => [Line a] -> Node a
parseHelper ((Line i l) : cs)
  | not (any fi cs) && not (null cs) = error "Tree has invalid indent"
  | otherwise = Node l $ map parseHelper $ branchBy fi cs
  where
    fi = (== i + 1) . indent

unparseHelper :: (Parsable a) => Int -> Node a -> String
unparseHelper i (Node a cs) = List.intercalate "\n" $ unparse (Line i a) : ct
  where
    ct = flip map cs $ unparseHelper $ i + 1

allKeys :: (Ord a) => Node a -> Set.Set a
allKeys (Node a cs) = foldr (Set.union . allKeys) (Set.singleton a) cs

find :: (Eq a) => a -> Node a -> Maybe (Node a)
find a t
  | a == key t = Just t
  | otherwise = Maybe.listToMaybe $ Maybe.mapMaybe (find a) (children t)

branchBy :: (a -> Bool) -> [a] -> [[a]]
branchBy = branchByHelper []

branchByHelper :: [[a]] -> (a -> Bool) -> [a] -> [[a]]
branchByHelper bs _ [] = reverse bs
branchByHelper bs f (a : as)
  | not $ f a = branchByHelper bs f as
  | otherwise = branchByHelper ((a : ls) : bs) f rs
  where
    (ls, rs) = break f as

visualize :: Map.Map String Int -> IO ()
visualize m = putStr $ unlines $ zipWith (++) isf bs
  where
    tt = fromIntegral $ sum $ Map.elems m
    bs = map (formatLine . (/ (tt + 1)) . fromIntegral) (Map.elems m)
    is = map (\(s, n) -> s ++ ": " ++ formatDuration n) (Map.toList m)
    lm = maximum (map length is) + 1
    isf = map (\s -> s ++ replicate (lm - length s) ' ') is

formatDuration :: Int -> String
formatDuration t
  | t <= 59 = show t ++ " minutes"
  | ms == 0 = show hs ++ " hours"
  | otherwise = show hs ++ " hours and " ++ show ms ++ " minutes"
  where
    hs = t `div` 60
    ms = t `rem` 60

formatLine :: Double -> String
formatLine v = "|" ++ replicate n Config.barChar ++ replicate m ' ' ++ "|"
  where
    n = round $ fromIntegral Config.barLength * v
    m = Config.barLength - n
