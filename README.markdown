
* web-mongrel2

This library attempts to abstract away some the details of connecting Haskell web frameworks to Mongrel2.

* Status

Gearing up for a usable release.

* Examples


    module Main where
    
    import Web.Mongrel2
    
    import Control.Monad (forever)
    import Text.Groom (groom)
    import Data.Default (def)
    
    main :: IO ()
    main = do
      let b = def { m2_publish = "tcp://127.0.0.1:9996"
                  , m2_pull = "tcp://127.0.0.1:9997" }
      bx <- connect b
      case m2_pull_socket bx of
        Nothing -> error "Pull socket didn't connect.  Is Mongrel2 running?"
        Just sock ->
          forever $ poll sock >>=
                      recv dummy bx >>
                      return ()
     where
       dummy :: Request -> IO Response
       dummy req = do
         putStrLn $ groom req
         return def
    