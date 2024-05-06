{-# LANGUAGE OverloadedStrings #-}
module PrettyPrint where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import System.Console.Pretty (style, Style(..), color, Color(..))

prettyPrint :: Map String String -> IO ()
prettyPrint mp = putStrLn
  $ intercalate "\n"
  $ map (\(k, v) -> (color Cyan . style Bold $ k) ++ " = " ++ color Magenta v)
  $ Map.assocs mp

prettyTitle :: String -> IO ()
prettyTitle title = putStrLn $ "[" ++ style Underline (style Bold title) ++ "]"
