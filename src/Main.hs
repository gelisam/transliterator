module Main where

import Data.Char
import Data.Either
import Test.DocTest

-- $setup
-- >>> let jsForeachSource = "for (let i of [1,2,3]) {console.log(i);}"
-- >>> let jsForeachPattern = "for (let VAR of LIST) {...}"
-- >>> let scalaForeachPattern = "LIST.foreach { VAR => ... }"
-- 
-- >>> let jsForeachSourceW = parseLexemeW jsForeachSource
-- 
-- >>> let jsForeachPatternV = parseLexemeV jsForeachPattern
-- >>> let jsForeachPatternVM = parseLexemeVW jsForeachPattern
-- 
-- >>> let scalaForeachPatternV = parseLexemeV scalaForeachPattern
-- >>> let scalaForeachPatternVM = parseLexemeVW scalaForeachPattern


class Unparse a where
    unparse :: a -> String

instance Unparse a => Unparse [a] where
    unparse = concatMap unparse

instance (Unparse a, Unparse b) => Unparse (Either a b) where
    unparse = either unparse unparse


data Lexeme
  = Alphanum   String
  | Symbol     String
  | Open       String
  | Close      String
  deriving (Show, Eq)

instance Unparse Lexeme where
    unparse (Alphanum s) = s
    unparse (Symbol   s) = s
    unparse (Open     s) = s
    unparse (Close    s) = s


data Whitespace
  = Newline
  | Blank   String
  | Comment String
  deriving (Show, Eq)

instance Unparse Whitespace where
    unparse Newline     = "\n"
    unparse (Blank   s) = s
    unparse (Comment s) = "//" ++ s

type LexemeW = Either Whitespace Lexeme

-- |
-- >>> mapM_ print jsForeachSourceW
-- Right (Alphanum "for")
-- Left (Blank " ")
-- Right (Open "(")
-- Right (Alphanum "let")
-- Left (Blank " ")
-- Right (Alphanum "i")
-- Left (Blank " ")
-- Right (Alphanum "of")
-- Left (Blank " ")
-- Right (Open "[")
-- Right (Alphanum "1")
-- Right (Symbol ",")
-- Right (Alphanum "2")
-- Right (Symbol ",")
-- Right (Alphanum "3")
-- Right (Close "]")
-- Right (Close ")")
-- Left (Blank " ")
-- Right (Open "{")
-- Right (Alphanum "console")
-- Right (Symbol ".")
-- Right (Alphanum "log")
-- Right (Open "(")
-- Right (Alphanum "i")
-- Right (Close ")")
-- Right (Symbol ";")
-- Right (Close "}")
parseLexemeW :: String -> [LexemeW]
parseLexemeW [] = []
parseLexemeW (c:cs) | isAlphaNum c = case parseLexemeW cs of
    Right (Alphanum s) : ls -> Right (Alphanum (c:s)) : ls
    ls                      -> Right (Alphanum [c]  ) : ls
parseLexemeW ('\n':cs) = Left Newline : parseLexemeW cs
parseLexemeW (c:cs) | isSpace c = case parseLexemeW cs of
    Left (Blank s) : ls -> Left (Blank (c:s)) : ls
    ls                  -> Left (Blank [c]  ) : ls
parseLexemeW ('/':'/':cs) = Left (Comment comment) : parseLexemeW cs'
  where
    (comment, cs') = break (/= '\n') cs
parseLexemeW ('{':cs) = Right (Open  "{") : parseLexemeW cs
parseLexemeW ('[':cs) = Right (Open  "[") : parseLexemeW cs
parseLexemeW ('(':cs) = Right (Open  "(") : parseLexemeW cs
parseLexemeW (')':cs) = Right (Close ")") : parseLexemeW cs
parseLexemeW (']':cs) = Right (Close "]") : parseLexemeW cs
parseLexemeW ('}':cs) = Right (Close "}") : parseLexemeW cs
parseLexemeW (c:cs) = case parseLexemeW cs of
    Right (Symbol s) : ls -> Right (Symbol (c:s)) : ls
    ls                    -> Right (Symbol [c]  ) : ls


newtype Var = Var String
  deriving (Show, Eq)

instance Unparse Var where
    unparse (Var s) = s

isVar :: Lexeme -> Maybe Var
isVar (Symbol "...") = Just (Var "...")
isVar (Alphanum s) | all isUpper s = Just (Var s)
isVar _ = Nothing


type LexemeV = Either Var Lexeme

-- |
-- >>> mapM_ print jsForeachPatternV
-- Right (Alphanum "for")
-- Right (Open "(")
-- Right (Alphanum "let")
-- Left (Var "VAR")
-- Right (Alphanum "of")
-- Left (Var "LIST")
-- Right (Close ")")
-- Right (Open "{")
-- Left (Var "...")
-- Right (Close "}")
parseLexemeV :: String -> [LexemeV]
parseLexemeV = fmap go . rights . parseLexemeW
  where
    go :: Lexeme -> LexemeV
    go x = case isVar x of
        Just v  -> Left v
        Nothing -> Right x


type LexemeVW = Either Var LexemeW

-- |
-- >>> mapM_ print scalaForeachPatternVM
-- Left (Var "LIST")
-- Right (Right (Symbol "."))
-- Right (Right (Alphanum "foreach"))
-- Right (Left (Blank " "))
-- Right (Right (Open "{"))
-- Right (Left (Blank " "))
-- Left (Var "VAR")
-- Right (Left (Blank " "))
-- Right (Right (Symbol "=>"))
-- Right (Left (Blank " "))
-- Left (Var "...")
-- Right (Left (Blank " "))
-- Right (Right (Close "}"))
parseLexemeVW :: String -> [LexemeVW]
parseLexemeVW = fmap go . parseLexemeW
  where
    go :: LexemeW -> LexemeVW
    go (Left  w) = Right (Left w)
    go (Right x) = case isVar x of
        Just v  -> Left v
        Nothing -> Right (Right x)



main :: IO ()
main = doctest ["src/Main.hs"]
