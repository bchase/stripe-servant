module Main where

import Lib

main :: IO ()
main =
  -- stripe WithoutConnect getCharges >>= print
  stripe WithoutConnect getCustomers >>= print
