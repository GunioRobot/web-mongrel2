
{-# LANGUAGE QuasiQuotes #-}

module Web.Mongrel2 (
  M2(..)
  , Request(..)
  , Response(..)
  , MongrelHeaders(..)
  , connect
  , poll
  , recv
  , defaultr
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (lookup)
import System.Time (getClockTime)
import qualified System.ZMQ as Z
import Text.StringTemplate

import Web.Mongrel2.Parsing
import Web.Mongrel2.QQ (qq)
import Web.Mongrel2.Types

import Data.Default (def)

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
  Z.send sock (BS.pack $ render $
    setAttribute "headers" (response_headers resp) $
    setManyAttrib [("id", (response_id resp)),
                   ("uuid", (response_uuid resp)),
                   ("idl", (show $ length $ response_id resp)),
                   ("now", (show now)),
                   ("clen", (show $ length $ response_body resp)),
                   ("sep", "\r\n"),
                   ("status", response_status resp),
                   ("contenttype", response_content_type resp),
                   ("charset", response_charset resp),
                   ("body",(response_body resp))] $
    newSTMP response_template) []

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

poll :: Z.Socket a -> IO [Z.Poll]
poll sock =
  Z.poll [Z.S sock Z.InOut] 1000000

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
response_template = [$qq|$uuid$ $idl$:$id$, HTTP/1.1 $status$ OK
Content-Type: $contenttype$; charset=$charset$
Connection: close
Content-Length: $clen$
Server: Mongrel2
Date: $now$
$headers:{a|$a.0$:$a.1$
}$

$body$

|]
