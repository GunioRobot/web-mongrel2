{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Web.Mongrel2
-- Copyright: (c) 2011 Clint Moore
-- License: BSD-style
-- Maintainer: cmoore@wamboli.com
-- Stability: experimental
-- Portability: GHC
--
--
-- A simple abstraction for applications to use Mongrel2.
-- Mongrel2 is simple and easy to use, and hopefully others
-- find this module almost as easy.
--
-- > require Web.Mongrel2
-- > require Control.Monad (forever)
-- >
-- > main :: IO ()
-- > main = do
-- >   conn <- connect $ def { m2_publish = "tcp://127.0.0.1:9996
-- >                         , m2_pull = "tcp://127.0.0.1:9997"
-- >                         , m2_uuid = "my-awesome-webapp" }
-- >   case m2_pull_socket conn of
-- >     Nothing -> error "Didn't connect to Mongrel2!"
-- >     Just sock ->
-- >       forever $ poll sock >>=
-- >                   recv dumper conn >>
-- >                   return ()
-- >
-- >  where
-- >    dumper :: Request -> IO Response
-- >    dumper req = do
-- >      putStrLn req
-- >      return $ defaultr req
-- >

module Web.Mongrel2 (
  M2(..)
  , Request(..)
  , Response(..)
    -- * Connection
  , connect
  , poll
  , recv
    -- * Utilities
  , defaultr
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import System.Time (getClockTime)
import qualified System.ZMQ as Z
import Text.StringTemplate
import qualified Data.List as L

import Web.Mongrel2.Parsing
import Web.Mongrel2.Types
import Data.FileEmbed (embedFile)

import Data.Default (def)

-- | Generate a 'Response' from the 'Request' copying sane defaults.
defaultr :: Request -> Response
defaultr req =
  def { response_uuid = request_uuid req
      , response_id = request_id req
      , response_path = request_path req }

-- Request
-- UUID ID PATH SIZE:HEADERS,SIZE:BODY

-- Response
-- UUID SIZE:ID ID ID, BODY

send_response :: Z.Socket a -> Response -> IO ()
send_response sock resp = do
  now <- getClockTime
  -- TODO: Why the hell do I have to do this?
  -- TODO: Otherwise, every response is missing one character.
  let leng = (L.length $ response_body resp) + 1
  let res = BS.pack $
             render $
             setAttribute "headers" (response_headers resp) $
             setManyAttrib [("id", (response_id resp)),
                            ("uuid", (response_uuid resp)),
                            ("idl", (show $ length $ response_id resp)),
                            ("now", (show now)),
                            ("clen", show leng),
                            ("sep", "\r\n"),
                            ("status", response_status resp),
                            ("contenttype", response_content_type resp),
                            ("charset", response_charset resp),
                            ("body",(response_body resp))] $
             newSTMP response_template
  Z.send sock res []

-- | The receive action.
--
recv :: (Request -> IO Response) -> M2 -> [Z.Poll] -> IO ()
recv handle pub ((Z.S s _):_ss) = do
  req <- Z.receive s []
  case m2_parse (BS.unpack req) of
    Left err -> error err
    Right rq -> do
      rsp <- handle rq
      case m2_publish_socket pub of
        Nothing -> error "Publish socket not connected?!"
        Just so -> send_response so rsp
recv _ _ _ = return ()

-- | Shortcut for @system-zeromq@'s poll function.
-- Interval is hardcoded to 1000000 ms.
poll :: Z.Socket a -> IO [Z.Poll]
poll sock =
  Z.poll [Z.S sock Z.InOut] 1000000

-- | Connects the internal sockets.
connect :: M2 -> IO M2
connect mong = do
  case ((,)
        <$> m2_publish_socket mong
        <*> m2_pull_socket mong ) of
    Just _ -> return mong
    Nothing -> do
      ctx <- Z.init 1
      pub <- Z.socket ctx Z.Pub
      pull <- Z.socket ctx Z.Pull

      let uid = case m2_uuid mong of
            Just v -> v
            Nothing -> "82209006-86GF-4982-B5EA-D1E29E55D481"

      Z.connect pull $ m2_pull mong
      Z.setOption pull $ Z.Identity uid

      Z.connect pub $ m2_publish mong

      return $
        mong { m2_publish_socket = Just pub
             , m2_pull_socket = Just pull
             , m2_context = Just ctx
             , m2_uuid = Just uid
             }

response_template :: String
response_template = BS.unpack $(embedFile "response_template.st")
