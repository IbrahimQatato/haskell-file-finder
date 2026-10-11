{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser (parsePrompt) where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Text.Parsec(
    ParseError, digit, letter,
    noneOf, string, choice,
    many1, sepBy1, (<|>),
    parse, try, string' , option)
import Text.Parsec.String
import Prompt (Prompt(..), Condition(..), Conjunction(..))

parsePrompt :: String -> Either ParseError (FilePath, Prompt)
parsePrompt input = 
    if " and " `isInfixOf` input && " or " `isInfixOf` input
        then error "Can only have one type of conjunction: <and>/<or>"
        else parse (promptParser input) "" input

promptParser :: String -> Parser (FilePath, Prompt)
promptParser input = do
    _ <- string "find files "
    conditions <- conditionParser `sepBy1` conjunctionParser
    let conjunction = if " and " `isInfixOf` input then And else Or
    folderPath <- option "." (try folderParser)
    return (folderPath, (Prompt conjunction )conditions)

conjunctionParser :: Parser String
conjunctionParser = string' " and " <|> string' " or "

conditionParser :: Parser Condition
conditionParser = try parseName <|> try parseDate <|> parseSize 

parseName :: Parser Condition
parseName = do
    pos <- choice [
        string' "beginning with ", string' "starting with ",
        string' "containing ", string' "ending with ",
        string' "named "]
    intermediateName <- many1 (noneOf " ")
    let name = map toLower intermediateName 
    return $ case pos of
        "beginning with " -> NameCondition "beginning" name
        "starting with " -> NameCondition "starting" name
        "containing " -> NameCondition "containing" name
        "ending with " -> NameCondition "ending" name
        "named " -> NameCondition "named" name
        _ -> error "Unexpected case in parseName"

parseDate :: Parser Condition
parseDate = do
    _ <- string "changed "
    date <- choice [
        string' "today", string' "yesterday", string' "this week", string' "last week",
        string' "this month", string' "last month", string' "this year", string' "last year",
        string' "on " >> many1 letter]
    return (DateCondition date)

parseSize :: Parser Condition
parseSize = do
    comp <- choice [string' "larger than ", string' "smaller than "]
    size <- many1 digit
    unit <- choice [string "kb", string "mb", string "gb"]
    return (SizeCondition comp (read size) unit)

folderParser :: Parser FilePath
folderParser = do
    _ <- string " in "
    path <- many1 (noneOf " ")
    return path
