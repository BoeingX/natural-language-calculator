module Main where

import Control.Monad
import Data.Char
import System.IO
import Text.Trifecta

import Lib

main :: IO ()
main = forever $ do
    putStr ">> "
    hFlush stdout
    line <- map toLower <$> getLine  
    let result = eval <$> parseString parse mempty line
    case result of
      Success n -> print n
      _ -> putStrLn "parse error"
