module Config where

tab :: Int
tab = 4

barLength :: Int 
barLength = 20

barChar :: Char
barChar = '-'

sep :: String
sep = " | "

assets, treeFile, activitiesFile, activeFile, recordsFile, inputFile :: FilePath
assets = "./assets/"
treeFile = assets ++ "tree.txt"
activitiesFile = assets ++ "activities.txt"
activeFile = assets ++ "active.txt"
recordsFile = assets ++ "records.txt"
inputFile = assets ++ "input.txt"
