{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Interface (load, reload, start, stop, cancel, status, reset, view) where

import qualified Base
import qualified Config
import qualified Control.Monad as Monad
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Base

load :: FilePath -> IO ()
load f = do
  initFromFile f
  writeFile Config.inputFile f
  putStrLn "Successfully loaded activity tree"

reload :: IO ()
reload = do
  checkInit
  c <- readFile Config.inputFile
  initFromFile c
  putStrLn $ "Successfully reloaded activity tree from " ++ show c

start :: Base.Activity -> IO ()
start a = do
  checkInit
  as <- readFile Config.activitiesFile <&> Set.fromAscList . map Base.parse . Split.splitOn Config.sep
  mr <- getCurrentRecord
  if Maybe.isJust mr
    then do
      let r = Maybe.fromJust mr
      putStrLn $ "Cannot start new activity while " ++ show (Base.label $ Base.activity r) ++ " is active"
    else
      if a `notElem` as
        then do
          putStrLn $ "Activity " ++ show (Base.label a) ++ " does not exist in your activity tree"
          Exit.exitSuccess
        else do
          ct <- Base.now
          cd <- Base.today
          writeFile Config.activeFile $ Base.unparse $ Base.Record ct Base.end cd a
          putStrLn $ "Started " ++ show (Base.unparse a) ++ " at " ++ Base.unparse ct

stop :: IO ()
stop = do
  checkInit
  ct <- Base.now
  mr <- getCurrentRecord
  if Maybe.isNothing mr
    then putStrLn "There is no current activity to stop"
    else do
      let r = Maybe.fromJust mr
      addRecord r
      putStrLn $ "Stopped " ++ show (Base.label $ Base.activity r) ++ " at " ++ Base.unparse ct
      writeFile Config.activeFile ""

cancel :: IO ()
cancel = do
  checkInit
  mr <- getCurrentRecord
  if Maybe.isNothing mr
    then putStrLn "There is no current activity to cancel"
    else do
      let r = Maybe.fromJust mr
      putStrLn $ "Cancelled " ++ show (Base.label $ Base.activity r)
      writeFile Config.activeFile ""

status :: IO ()
status = do
  checkInit
  mr <- getCurrentRecord
  if Maybe.isNothing mr
    then putStrLn "Currently not busy with anything"
    else do
      let r = Maybe.fromJust mr
      putStrLn $ "Busy with " ++ show (Base.label $ Base.activity r) ++ " since " ++ Base.unparse (Base.startTime r)

reset :: IO ()
reset = do
  putStrLn "All saved data will be erased. Do you want to proceed? (y/n)"
  s <- getLine
  if s == "y"
    then do
      removeIfExists Config.treeFile
      removeIfExists Config.activitiesFile
      removeIfExists Config.activeFile
      removeIfExists Config.recordsFile
      removeIfExists Config.inputFile
      putStrLn "Reset complete"
    else putStrLn "Aborted"

view :: Base.Activity -> Base.Date -> Base.Date -> IO ()
view a d1 d2 = do
  checkInit
  as <- readFile Config.activitiesFile <&> Set.fromAscList . map Base.parse . Split.splitOn Config.sep
  if a `notElem` as
    then do
      putStrLn $ "Activity " ++ show (Base.label a) ++ " does not exist in your activity tree"
      Exit.exitSuccess
    else do
      t <- readFile Config.treeFile <&> Base.parse
      rs :: [Base.Record] <- readFile Config.recordsFile <&> map Base.parse . lines
      let md = aggregate (Maybe.fromJust $ Base.find a t) (filter (Base.isBetween d1 d2 . Base.date) rs)
      Base.visualize $ Map.mapKeys Base.label md

aggregate :: Base.Node Base.Activity -> [Base.Record] -> Map.Map Base.Activity Int
aggregate ns = aggregateHelper (Map.insert (Base.Activity "Uncategorized") 0 (Map.fromList $ map ((,0) . Base.key) (Base.children ns))) ns

aggregateHelper :: Map.Map Base.Activity Int -> Base.Node Base.Activity -> [Base.Record] -> Map.Map Base.Activity Int
aggregateHelper m n (r : rs)
  | a == Base.key n = aggregateHelper mi n rs
  | null pas = aggregateHelper m n rs
  | otherwise = aggregateHelper ma n rs
  where
    a = Base.activity r
    pas = filter (Set.member a . Base.allKeys) (Base.children n)
    ma = Map.adjust (+ Base.duration r) (Base.key $ head pas) m
    mi = Map.adjust (+ Base.duration r) (Base.Activity "Uncategorized") m
aggregateHelper m _ [] = m

initFromFile :: FilePath -> IO ()
initFromFile f = do
  fe <- Directory.doesFileExist f
  if fe
    then do
      c <- readFile f
      let t = Base.parse c
      writeFile Config.treeFile $ Base.unparse t
      writeFile Config.activitiesFile $ List.intercalate Config.sep $ map Base.label $ Set.toList $ Base.allKeys t
      createIfNone Config.activeFile
      createIfNone Config.recordsFile
    else do
      putStrLn $ "File " ++ show f ++ " does not exists"
      Exit.exitSuccess

addRecord :: Base.Record -> IO ()
addRecord r@(Base.Record st et d a) = do
  cd <- Base.today
  if d == cd
    then do
      ct <- Base.now
      appendFile Config.recordsFile $ (Base.unparse $ r {Base.stopTime = ct}) ++ "\n"
    else do
      appendFile Config.recordsFile $ Base.unparse r ++ "\n"
      addRecord r {Base.startTime = Base.start, Base.date = Base.next d}

getCurrentRecord :: IO (Maybe Base.Record)
getCurrentRecord = do
  rc <- readFile Config.activeFile
  if null rc then return Nothing else return $ Just $ Base.parse rc

createIfNone :: FilePath -> IO ()
createIfNone f = do
  fe <- Directory.doesFileExist f
  Monad.unless fe $ writeFile f ""

removeIfExists :: FilePath -> IO ()
removeIfExists f = do
  fe <- Directory.doesFileExist f
  Monad.when fe $ Directory.removeFile f

checkInit :: IO ()
checkInit = do
  fe <- Directory.doesFileExist Config.treeFile
  Monad.unless fe $ do
    putStrLn "You need to load an activity tree first"
    Exit.exitSuccess
