module Main where

import Test.DocTest


-- |
-- >>> 2+2
-- 4
main :: IO ()
main = doctest ["src/Main.hs"]
