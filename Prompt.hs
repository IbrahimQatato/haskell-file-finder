module Prompt (Prompt(..), Condition(..), Conjunction(..)) where

data Prompt = Prompt Conjunction [Condition]
    deriving (Show, Eq)

data Condition = NameCondition String String
               | DateCondition String
               | SizeCondition String Integer String
               | PathCondition String
    deriving (Show, Eq)

data Conjunction = And | Or
    deriving (Show, Eq)
