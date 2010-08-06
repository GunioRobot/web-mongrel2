{-# LANGUAGE QuasiQuotes #-}

module Web.Mongrel2.QQ where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

qq :: QuasiQuoter
qq = QuasiQuoter (litE . stringL) (litP . stringL)
