module Main where

import Desbot

main :: IO ()
main =
  do c <- readPrivateConf
     connect'' c
     
