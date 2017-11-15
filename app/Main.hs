module Main where

import Lib

main :: IO ()
main = stripe Nothing getCharges >>= print
