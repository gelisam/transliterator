module Main where

import Data.Char
import Test.DocTest


data Lexeme
  = Alphanum   String
  | Symbol     String
  | Whitespace String
  | Newline
  | Comment    String
  | Open       String
  | Close      String
  deriving (Show, Eq)

-- |
-- >>> mapM_ print $ lexer "for (let i of [1,2,3]) {...}"
-- Alphanum "for"
-- Whitespace " "
-- Open "("
-- Alphanum "let"
-- Whitespace " "
-- Alphanum "i"
-- Whitespace " "
-- Alphanum "of"
-- Whitespace " "
-- Open "["
-- Alphanum "1"
-- Symbol ","
-- Alphanum "2"
-- Symbol ","
-- Alphanum "3"
-- Close "]"
-- Close ")"
-- Whitespace " "
-- Open "{"
-- Symbol "..."
-- Close "}"
lexer :: String -> [Lexeme]
lexer [] = []
lexer (c:cs) | isAlphaNum c = case lexer cs of
    Alphanum s : ls -> Alphanum (c:s) : ls
    ls              -> Alphanum [c]   : ls
lexer ('\n':cs) = Newline : lexer cs
lexer (c:cs) | isSpace c = case lexer cs of
    Whitespace s : ls -> Whitespace (c:s) : ls
    ls                -> Whitespace [c]   : ls
lexer ('/':'/':cs) = Comment comment : lexer cs'
  where
    (comment, cs') = break (/= '\n') cs
lexer ('{':cs) = Open "{" : lexer cs
lexer ('[':cs) = Open "[" : lexer cs
lexer ('(':cs) = Open "(" : lexer cs
lexer (')':cs) = Close ")" : lexer cs
lexer (']':cs) = Close "]" : lexer cs
lexer ('}':cs) = Close "}" : lexer cs
lexer (c:cs) = case lexer cs of
    Symbol s : ls -> Symbol (c:s) : ls
    ls            -> Symbol [c]   : ls


main :: IO ()
main = doctest ["src/Main.hs"]
