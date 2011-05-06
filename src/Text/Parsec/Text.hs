
{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parsec.Text ( module Text.Parsec.Char, Parser, GenParser ) where

import Text.Parsec.Prim
import Text.Parsec.Char
import qualified Data.Text as T

instance (Monad m) => Stream T.Text m Char where
  uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st

