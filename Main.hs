module Main where

import Data.Char
import FileFinder (findFiles)
import Parser (parsePrompt)
import Prompt (Prompt)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Enter your file search prompt: "
    prompt <- getLine
    case parsePrompt prompt of
        
        Left err -> putStrLn $ "Error parsing prompt: " ++ show err
        Right (folder, query) -> do
            results <- findFiles folder query
            mapM_ putStrLn results

