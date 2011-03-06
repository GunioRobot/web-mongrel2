
module Main where

import Web.Mongrel2


main :: IO ()
main = do
  connect $ def { m2_publish = "tcp://127.0.0.1:9996"
                , m2_pull = "tcp://127.0.0.1:9997" }
    