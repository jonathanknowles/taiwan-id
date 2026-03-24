module Main (main) where

import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "taiwan-id" []
