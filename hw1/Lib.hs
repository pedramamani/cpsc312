-- | @getFirstDigit n@ evaluates to the first digit of @n@, which is
-- 0 for the number 0 and one of [1, ..., 9] otherwise.
--
-- getFirstDigit 6 == 6
-- getFirstDigit 112 == 1
-- getFirstDigit 0 == 0
-- getFirstDigit -4 == 4

getFirstDigit :: Integer -> Integer
getFirstDigit n | abs(n) < 10 = abs(n)
getFirstDigit n = getFirstDigit (abs(n) `div` 10)



-- | @getFirstDigits ns@ evaluates to a list of the first digits of
-- @ns@, using 'getFirstDigit', described above.
--
-- getFirstDigits [] == []
-- getFirstDigits [15, 0, -4, 728] == [1, 0, 4, 7]
-- getFirstDigits [-0, 10, -999] == [0, 1, 9]

getFirstDigits :: [Integer] -> [Integer]
getFirstDigits [] = []
getFirstDigits (first:rest) = getFirstDigit first : getFirstDigits rest



-- | @addLists l l'@ returns the element-wise sum of lists @l@ and @l'@.
-- For lists of unequal lengths, the longer list is shortened.
--
-- addLists [1, 2, -1] [0, 9, 3] == [1, 11, 2]
-- addLists [] [] == []
-- addLists [1] [1, 2] == [2]

addLists :: [Integer] -> [Integer] -> [Integer]
addLists [] _ = []
addLists _ [] = []
addLists (n:ns) (n':ns') = n + n' : addLists ns ns'



-- | @digitToCount n@ evaluates to a 9-long list where the @n@th
-- entry is 1 and the other entries are 0s.
--
-- digitToCount 8 == [0, 0, 0, 0, 0, 0, 0, 1, 0]
-- digitToCount 23 == [0, 0, 0, 0, 0, 0, 0, 0, 0]

digitToCount :: Integer -> [Integer]
digitToCount n | 1 <= n && n <= 9 = replicate (fromIntegral (n - 1)) 0 ++ [1] ++ replicate (fromIntegral (9 - n)) 0
digitToCount _ = replicate 9 0


-- | @countDigits ns@ returns a list of 9 numbers representing how
-- often each digit [1, ..., 9] occurs as the first digit of a number in `ns`.
--
-- countDigits [] == [0, 0, 0, 0, 0, 0, 0, 0, 0]
-- countDigits [-12, 19, 3, 922] == [2, 0, 1, 0, 0, 0, 0, 0, 1]

countDigits :: [Integer] -> [Integer]
countDigits [] = digitToCount 0
countDigits (n:ns) = addLists (digitToCount (getFirstDigit n)) (countDigits ns)



-- | @barChart c ns@ produces a list of one string per number @n@ in 
-- @ns@. Each string is as long as the corresponding number @n@ and
-- made up of copies of @c@. (The string is empty if @n < 0@.)
--
-- barChart '#' [] == []
-- barChart '#' [3, 0, 5, 1] == ["###","","#####","#"]
-- barChart '-' [2] == ["--"]

barChart :: Char -> [Integer] -> [String]
barChart _ [] = []
barChart c (n:ns) = (replicate (max 0 (fromIntegral n)) c) : barChart c ns
 


-- | @genBenChart ns@ produces a list of strings that is a bar
-- chart of '#' characters with 9 bars showing the frequency of 
-- first digits 1 to 9 in @ns@.
--
-- genBenChart [] = ["", "", "", "", "", "", "", "", ""]
-- genBenChart [22, 7, 13, 18, 3, 6, 35] == ["##", "#", "##", "", "", "#", "#", "", ""]

genBenChart :: [Integer] -> [String]
genBenChart ns = barChart '#' (countDigits ns)



-- | @valueOfLine l@ requires that @l@ be exactly: A single line of data
-- starting with an integer followed optionally by a space and any
-- additional text.
-- It evaluates to the value of the integer. 
--
-- valueOfLine "1" = 1
-- valueOfLine "-32 is my favorite number" == -32

valueOfLine s = read (head (words s))



-- | @extractData s@ requires that @s@ be exactly: a single line (of 
-- any text), followed by zero or more lines of data. A line of data
-- starts with an integer followed optionally by a space and any
-- additional text.
-- It evaluates to a tuple of the text of the first line and a list
-- of the integers from the subsequent lines.
--
-- extractData "\n" == ("", [])
-- extractData "Test\n12\n1 is a small number!\n345 45 5\n" == ("Test", [12, 1, 345])
-- extractData "123\n-9\n" == ("123", [-9])

extractData :: String -> (String, [Integer])
extractData d = (head (lines d), map valueOfLine (tail (lines d)))



-- | @main@ reads from standard input (and expects data in 
-- the format described for 'extractData' above), and prints
-- a bar chart showing frequencies of first digits in the data
-- to standard output.

main :: IO ()
main = 
  getContents >>= 
    pure . extractData >>=
    pure . (\(title, numLines) -> 
      unlines (title : genBenChart numLines)) >>=
    putStr
