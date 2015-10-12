module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.State
import Data.Char
import Data.Either
import Test.DocTest
import Text.Printf

-- $setup
-- >>> let testParser p = fmap unparse . evalParser p . parseLexemeW
-- 
-- >>> let jsForeachSource = "for (let i of [1,2,3]) {console.log(i);}"
-- >>> let jsForeachPattern = "for (let VAR of LIST) {...}"
-- >>> let cppForeachPattern = "for (... VAR = LIST.begin(); VAR != LIST.end(); ++VAR) {...}"
-- >>> let scalaForeachPattern = "LIST.foreach { VAR => ... }"
-- 
-- >>> let jsForeachSourceW = parseLexemeW jsForeachSource
-- 
-- >>> let jsForeachPatternV = parseLexemeV jsForeachPattern
-- >>> let jsForeachPatternVM = parseLexemeVW jsForeachPattern
-- 
-- >>> let cppForeachPatternV = parseLexemeV cppForeachPattern
-- >>> let cppForeachPatternVM = parseLexemeVW cppForeachPattern
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

matchingParen :: String -> String
matchingParen "{" = "}"
matchingParen "[" = "]"
matchingParen "(" = ")"
matchingParen s   = error $ printf "unrecognized paren '%s'" s


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


type Subst1 = (Var,[LexemeW])
type Subst = [Subst1]

-- | only substitute the first occurrence of one variable.
-- >>> unparse $ substitute11 (Var "VAR", parseLexemeW "i") cppForeachPatternVM
-- "for (... i = LIST.begin(); VAR != LIST.end(); ++VAR) {...}"
substitute11 :: Subst1 -> [LexemeVW] -> [LexemeVW]
substitute11 (var,replacement) = go
  where
    go :: [LexemeVW] -> [LexemeVW]
    go []                     = []
    go (Left v:xs) | v == var = fmap Right replacement ++ xs
    go (x     :xs)            = x : go xs 

-- | substitute all occurrences of one variable.
-- >>> unparse $ substitute1 (Var "VAR", parseLexemeW "i") cppForeachPatternVM
-- "for (... i = LIST.begin(); i != LIST.end(); ++i) {...}"
substitute1 :: Subst1 -> [LexemeVW] -> [LexemeVW]
substitute1 (var,replacement) = go
  where
    go :: [LexemeVW] -> [LexemeVW]
    go []                     = []
    go (Left v:xs) | v == var = fmap Right replacement ++ go xs
    go (x     :xs)            = x : go xs 

-- | Substitute all occurrences of all variables.
-- 
-- The same variable (usually "...") can appear more than once in the
-- replacements, in which case each occurrence of that variable in the input
-- will be replaced with the corresponding replacement.
-- A variable can also appear once in the replacements, in which case all the
-- occurrences of that variable in the input will become that replacement.
-- 
-- >>> :{
-- unparse $ substitute [ (Var "...", parseLexemeW "map<int,string>::key_iterator")
--                      , (Var "VAR", parseLexemeW "i")
--                      , (Var "LIST", parseLexemeW "myMap.keys")
--                      , (Var "...", parseLexemeW "cout << i << endl;")
--                      ] cppForeachPatternVM
-- :}
-- "for (map<int,string>::key_iterator i = myMap.keys.begin(); i != myMap.keys.end(); ++i) {cout << i << endl;}"
substitute :: Subst -> [LexemeVW] -> [LexemeW]
substitute replacements = fmap assertR
                        . substituteAll replacements
                        . substituteOnce replacements
  where
    compose :: [a -> a] -> (a -> a)
    compose = foldr (>>>) id
    
    substituteOnce :: Subst -> [LexemeVW] -> [LexemeVW]
    substituteOnce = compose . fmap substitute11
    
    substituteAll :: Subst -> [LexemeVW] -> [LexemeVW]
    substituteAll = compose . fmap substitute1
    
    assertR :: LexemeVW -> LexemeW
    assertR (Left (Var var)) = error $ printf "'%s' not in scope" var
    assertR (Right x) = x


type Parser a = StateT [LexemeW] [] a

runParser :: Parser a -> [LexemeW] -> Maybe (a, [LexemeW])
runParser parser input = case runStateT parser input of
    []    -> Nothing
    (x:_) -> Just x

evalParser :: Parser a -> [LexemeW] -> Maybe a
evalParser parser = fmap fst . runParser parser

-- |
-- >>> evalParser (pMaybeToken Just) $ parseLexemeW "..."
-- Just (Right (Symbol "..."))
-- >>> evalParser (pMaybeToken (const Nothing)) $ parseLexemeW "..."
-- Nothing
-- >>> evalParser (pMaybeToken Just) $ parseLexemeW ""
-- Nothing
pMaybeToken :: (LexemeW -> Maybe a) -> Parser a
pMaybeToken p = do
    (x:xs) <- get
    Just y <- return (p x)
    put xs
    return y

pWhitespace :: Parser Whitespace
pWhitespace = pMaybeToken go
  where
    go :: LexemeW -> Maybe Whitespace
    go (Left w) = Just w
    go _ = Nothing

pLexeme :: Parser Lexeme
pLexeme = pMaybeToken go
  where
    go :: LexemeW -> Maybe Lexeme
    go (Right x) = Just x
    go _ = Nothing

pLexemeW :: Parser LexemeW
pLexemeW = pMaybeToken Just

pOpen :: Parser String
pOpen = pMaybeToken go
  where
    go :: LexemeW -> Maybe String
    go (Right (Open s)) = Just s
    go _ = Nothing

pClose :: Parser String
pClose = pMaybeToken go
  where
    go :: LexemeW -> Maybe String
    go (Right (Close s)) = Just s
    go _ = Nothing

-- not an Open nor a Close
pFlatLexemeW :: Parser LexemeW
pFlatLexemeW = pMaybeToken go
  where
    go :: LexemeW -> Maybe LexemeW
    go (Right (Open  _)) = Nothing
    go (Right (Close _)) = Nothing
    go x = Just x

-- not an Open nor a Close
pFlatLexeme :: Parser Lexeme
pFlatLexeme = pMaybeToken go
  where
    go :: LexemeW -> Maybe Lexeme
    go (Right (Open  _)) = Nothing
    go (Right (Close _)) = Nothing
    go (Right x) = Just x
    go _ = Nothing

-- match as little as possible, possibly nothing.
pWildcard0 :: Parser [LexemeW]
pWildcard0 = return []
         <|> ((++) <$> pNesting     <*> pWildcard0)
         <|> ((:)  <$> pFlatLexemeW <*> pWildcard0)

-- | match as little as possible, but at least one lexeme.
-- >>> testParser pWildcard "foo.bar(baz)"
-- Just "foo"
-- >>> testParser pWildcard " foo.bar(baz)"
-- Just " foo"
-- >>> testParser pWildcard "(foo ~ bar).apply(baz)"
-- Just "(foo ~ bar)"
-- >>> testParser pWildcard "(foo ~ bar].apply(baz)"
-- *** Exception: mismatched parens: '(' and ']'
pWildcard :: Parser [LexemeW]
pWildcard = ((++) <$> pNesting                <*> pWildcard0)
        <|> ((:)  <$> (Right <$> pFlatLexeme) <*> pWildcard0)
        <|> ((:)  <$> (Left  <$> pWhitespace) <*> pWildcard )

pNesting :: Parser [LexemeW]
pNesting = do
    sOpen <- pOpen
    xs <- pWildcard0
    sClose <- pClose
    if matchingParen sOpen == sClose
    then return $ [Right (Open sOpen)]
               ++ xs
               ++ [Right (Close sClose)]
    else error $ printf "mismatched parens: '%s' and '%s'" sOpen sClose


main :: IO ()
main = doctest ["src/Main.hs"]
