module Main where

import Desbot

main :: IO ()
main =
  do c <- readConfig
     connect'' c
