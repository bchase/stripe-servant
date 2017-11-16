module Main where

import Lib

main :: IO ()
main =
  -- stripe WithoutConnect [ PaginateBy 10 ] getCharges >>= print
  stripe WithoutConnect [ PaginateBy 10 ] getCustomers >>= print
