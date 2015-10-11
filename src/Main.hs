module Main where

import Data.Char
import Data.Either
import Test.DocTest


data Lexeme
  = Alphanum   String
  | Symbol     String
  | Open       String
  | Close      String
  deriving (Show, Eq)


data Whitespace
  = Newline
  | Blank   String
  | Comment String
  deriving (Show, Eq)

type LexemeW = Either Whitespace Lexeme

-- |
-- >>> mapM_ print $ parseLexemeW "for (let i of [1,2,3]) {...}"
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
-- Right (Symbol "...")
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


-- Nothing stands for a variable
type LexemeV = Maybe Lexeme

-- |
-- >>> mapM_ print $ parseLexemeV "for (let VAR of LIST) {...}"
-- Right (Alphanum "for")
-- Left (Blank " ")
-- Right (Open "(")
-- Right (Alphanum "let")
-- Left (Blank " ")
-- Nothing
-- Left (Blank " ")
-- Right (Alphanum "of")
-- Left (Blank " ")
-- Nothing
-- Right (Close ")")
-- Left (Blank " ")
-- Right (Open "{")
-- Nothing
-- Right (Close "}")
parseLexemeV :: String -> [LexemeV]
parseLexemeV = fmap isVar . rights . parseLexemeW
  where
    isVar :: Lexeme -> LexemeV
    isVar (Alphanum "...") = Nothing
    isVar (Alphanum s) | all isUpper s = Nothing
    isVar x = Just x


main :: IO ()
main = doctest ["src/Main.hs"]
