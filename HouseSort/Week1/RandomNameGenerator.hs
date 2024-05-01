module Week1.HouseSort.RandomNameGenerator where

import System.IO
import System.Random (randomRIO)

import Data.Maybe (catMaybes)

type StudentName = String

data Student = Student {
    lastName :: String,
    name     :: StudentName
} deriving (Show, Eq, Ord)

data SchoolHouse = Griffindor
                 | Hufflepuff
                 | Ravenclaw
                 | Slytherin
                 deriving (Eq, Show, Ord, Read)

assignSchoolHouse :: IO SchoolHouse
assignSchoolHouse = do
    house <- pickRandom [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    return house

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs!! idx)

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = []
split' d s = x : split' d (drop 1 y)
    where (x, y) = span (/= d) s

parseLine :: String -> Maybe Student
parseLine line = case split' ',' line of
    [lastName, name] -> Just $ Student {lastName = lastName, name = name}
    _               -> Nothing

distributeStudents :: [Student] -> Int -> Int -> [SchoolHouse] -> [(SchoolHouse, [Student])]
distributeStudents students studentsPerHouse extraStudents houses =
    let housesWithExtraStudents = take extraStudents houses
        housesWithoutExtraStudents = drop extraStudents houses
        studentsByHouse = zip houses (chunks studentsPerHouse students)
        extraStudentsByHouse = zip housesWithExtraStudents (chunks 1 (take extraStudents students))
        studentsByHouse' = combine studentsByHouse extraStudentsByHouse
    in studentsByHouse'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

combine :: [(a, [b])] -> [(a, [b])] -> [(a, [b])]
combine [] ys = ys
combine xs [] = xs
combine ((x, xs'):xss) ((y, ys'):yss) = (x, xs' ++ ys') : combine xss yss

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = catMaybes $ map parseLine (lines contents)
    return students

main :: IO ()
main = do
    records <- readFileToList "./Students.txt"
    let houses = [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    let studentsPerHouse = length records `div` length houses
    let extraStudents = length records `mod` length houses
    let studentsByHouse = distributeStudents records studentsPerHouse extraStudents houses
    mapM_ (\(house, students) -> do
        putStrLn $ "House: " ++ show house
        mapM_ (\student -> putStrLn $ "  " ++ name student ++ " " ++ lastName student) students
        putStrLn "") studentsByHouse