module Main where

import Data.Char
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
-- >>> mapM_ print $ lexer "for (let i of [1,2,3]) {...}"
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
lexer :: String -> [LexemeW]
lexer [] = []
lexer (c:cs) | isAlphaNum c = case lexer cs of
    Right (Alphanum s) : ls -> Right (Alphanum (c:s)) : ls
    ls                      -> Right (Alphanum [c]  ) : ls
lexer ('\n':cs) = Left Newline : lexer cs
lexer (c:cs) | isSpace c = case lexer cs of
    Left (Blank s) : ls -> Left (Blank (c:s)) : ls
    ls                  -> Left (Blank [c]  ) : ls
lexer ('/':'/':cs) = Left (Comment comment) : lexer cs'
  where
    (comment, cs') = break (/= '\n') cs
lexer ('{':cs) = Right (Open  "{") : lexer cs
lexer ('[':cs) = Right (Open  "[") : lexer cs
lexer ('(':cs) = Right (Open  "(") : lexer cs
lexer (')':cs) = Right (Close ")") : lexer cs
lexer (']':cs) = Right (Close "]") : lexer cs
lexer ('}':cs) = Right (Close "}") : lexer cs
lexer (c:cs) = case lexer cs of
    Right (Symbol s) : ls -> Right (Symbol (c:s)) : ls
    ls                    -> Right (Symbol [c]  ) : ls


main :: IO ()
main = doctest ["src/Main.hs"]
