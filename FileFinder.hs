module FileFinder (findFiles) where

import Control.Monad (filterM)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Time (addDays)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
import Prompt (Prompt(..), Condition(..), Conjunction(..))
import System.Directory (
    doesDirectoryExist, listDirectory, doesFileExist, 
    getModificationTime, canonicalizePath)
import System.FilePath ((</>), takeFileName)
import System.IO (hFileSize, withFile, IOMode(..))



findFiles :: FilePath -> Prompt -> IO [FilePath]
findFiles folder (Prompt conj conditions) = do
    allContents <- listDirectoryRecursive folder
    filterM (matchesConditions conditions conj) allContents

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    contents <- listDirectory dir
    foldl (\accIO name -> do
        acc <- accIO
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then do
                subDirContents <- listDirectoryRecursive path
                return (acc ++ subDirContents)
            else return (acc ++ [path])
     ) (return []) contents

matchesConditions :: [Condition] -> Conjunction -> FilePath -> IO Bool
matchesConditions conditions conj file = do
    results <- mapM (matchesCondition file) conditions
    return $ (if conj == And then all else any) id results

matchesCondition :: FilePath -> Condition -> IO Bool
matchesCondition file (NameCondition pos name) = 
    let filename = map toLower $ takeFileName file
    in
      return $ case pos of
        "beginning" -> take (length name) filename == name
        "starting" -> take (length name) filename == name
        "containing" -> name `isInfixOf` filename
        "ending" -> reverse (take (length name) (reverse filename)) == name
        "named" -> filename == name

matchesCondition file (DateCondition date) = do
    modTime <- getModificationTime file
    curTime <- getCurrentTime
    localModTime <- utcToLocalZonedTime modTime
    localCurTime <- utcToLocalZonedTime curTime
    let localModDay = localDay (zonedTimeToLocalTime localModTime)
        localCurDay = localDay (zonedTimeToLocalTime localCurTime)
        (modYear, modMonth, modDay) = toGregorian localModDay 
        (curYear, curMonth, curDay) = toGregorian localCurDay 
        (_, modWeek, _) = toWeekDate localModDay
        (_, curWeek, _) = toWeekDate localCurDay
        (yearOfLastWeek, lastWeek, _ ) = toWeekDate (addDays (-7) localCurDay) 
        dayOfWeek = map toLower $ formatTime defaultTimeLocale "%A" localModTime
        modMonthDate = YearMonth modYear modMonth
        curMonthDate = YearMonth curYear curMonth
        modDayDate = YearDay modYear modDay
        curDayDate = YearDay curYear curDay
        daysOfWeek = ["monday", "tuesday", "wednesday", 
            "thursday", "friday", "saturday", "sunday"]
        curDate = map toLower date
        in
        return $ case date of
            "today" -> curDayDate == modDayDate
            "yesterday" -> modDayDate == addDays (-1) curDayDate 
            "this week" -> modYear == curYear && modWeek == curWeek
            "last week" -> modYear == yearOfLastWeek && modWeek == lastWeek
            "this month" -> modMonthDate == curMonthDate
            "last month" -> modMonthDate == addMonths (-1) curMonthDate
            "this year" -> modYear == curYear
            "last year" -> modYear == curYear - 1
            _ | curDate `elem` daysOfWeek -> dayOfWeek == map toLower date
            _ -> error $ "Can't parse date expression: " ++ date
    
matchesCondition file (SizeCondition comp size unit) = do
    fileSize <- withFile file ReadMode hFileSize
    let fileSizeInBytes = convertToBytes size unit
    return $ case comp of
        "larger than " -> fileSize > fileSizeInBytes
        "smaller than " -> fileSize < fileSizeInBytes

convertToBytes :: Integer -> String -> Integer
convertToBytes size unit = case unit of
    "kb" -> size * 1024
    "mb" -> size * 1024 * 1024
    "gb" -> size * 1024 * 1024 * 1024
