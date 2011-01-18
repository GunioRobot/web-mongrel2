{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Mongrel2 (
  M2(..)
  , Request(..)
  , Response(..)
  , MongrelHeaders(..)
  , connect
  , poll
  , recv
  ) where

import Char (toLower)
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.String.Utils (split,join)
import Prelude hiding (lookup)
import System.Time (getClockTime)
import qualified System.ZMQ as Z
import Text.JSON
import qualified Text.JSON as JS
import qualified Text.ParserCombinators.Parsec as P
import Text.StringTemplate
import Web.Mongrel2.QQ (qq)

-- | The Mongrel2 specific request headers.
data MongrelHeaders = MongrelHeaders {
  header_uuid :: String,
  header_id :: String,
  header_path :: String
  } deriving (Show)
                        
-- | An incoming request from the server.
data Request = Request {
  request_mongrel_headers :: MongrelHeaders,
  request_headers :: [(String,String)],
  request_path :: String,
  request_method :: String,
  request_version :: String,
  request_uri :: String,
  request_pattern :: String,
  request_accept :: String,
  request_host :: String,
  request_query_string :: String,
  request_user_agent :: String
  } deriving (Show)

-- | The response to send back.
data Response = Response {
  response_uuid :: String,
  response_id :: String,
  response_body :: String,
  response_headers :: [(String,String)],
  response_status :: String
  } deriving(Show)

-- | Internal connection data.
data M2 = M2 {
  m2_publish :: String,
  m2_publish_socket :: Maybe (Z.Socket Z.Pub),
  m2_pull :: String,
  m2_pull_socket :: Maybe (Z.Socket Z.Pull),
  m2_context :: Maybe Z.Context,
  m2_uuid :: Maybe String
  }

instance Default M2 where
  def = M2 {
    m2_publish = def,
    m2_publish_socket = Nothing,
    m2_pull = def,
    m2_pull_socket = Nothing,
    m2_context = Nothing,
    m2_uuid = Nothing
    }

instance Default MongrelHeaders where
  def = MongrelHeaders { 
    header_uuid = def,
    header_id = def,
    header_path = def
    }

instance Default Request where
  def = Request {
    request_mongrel_headers = def,
    request_headers = def,
    request_path = def,
    request_method = def,
    request_version = def,
    request_uri = def,
    request_pattern = def,
    request_accept = def,
    request_host = def,
    request_query_string = def,
    request_user_agent = def
    }

instance Default Response where
  def = Response {
    response_body = def,
    response_id = def,
    response_uuid = def,
    response_headers = def,
    response_status = def
    }

-- | Lookup a key from the JSON-encoded request from Mongrel2

mlookup :: String -> JS.JSObject JS.JSValue -> Maybe String
mlookup key bndl =
  mlookup' key bndl <|> mlookup' (map toLower key) bndl
 where
   mlookup' :: String -> JS.JSObject JS.JSValue -> Maybe String
   mlookup' k b = 
     case JS.valFromObj k b of
       JS.Ok v -> Just $ JS.fromJSString v
       _ -> Nothing

parse :: String -> Either String Request
parse request =
  case split " " request of
    (nam:seqq:pat:blk) ->
      case request_env $ join " " blk of
        Left e -> Left e
        Right req ->
          Right req { request_mongrel_headers =
                         def { header_id = seqq
                             , header_uuid = nam
                             , header_path = pat } }
    _ -> Left "Unsupported or mis-parsed request."

request_env :: String -> Either String Request
request_env request_body =
  case P.parse qstr "" request_body of
    Left x -> Left $ show x
    Right (headers_,query_string_) ->
      case JS.decode headers_ of
        JS.Ok (JS.JSObject json) -> do
          let unjs = concat $
                map (\(x,JSString y) -> do
                      [(x,fromJSString y)]
                    ) $ fromJSObject json
          
          Right $ def { request_path = ml "PATH" json
                      , request_method = ml "METHOD" json
                      , request_version = ml "VERSION" json
                      , request_uri = ml "URI" json
                      , request_headers = unjs
                      , request_pattern = ml "PATTERN" json
                      , request_accept = ml "Accept" json
                      , request_host = ml "Host" json
                      , request_user_agent = ml "User-Agent" json
                      , request_query_string = query_string_
                      }
            
        _ -> Left "error parsing the headers."
 where
  ml :: String -> JS.JSObject JS.JSValue -> String
  ml k b = maybe "" id $ mlookup k b

qstr :: P.Parser (String,String)
qstr = do
  n <- number
  _ <- P.char ':'
  x <- P.count n P.anyChar
  _ <- P.char ','
  nx <- number
  _ <- P.char ':'
  xy <- P.count nx P.anyChar
  
  return (x,xy)

number :: P.Parser Int
number = do
  b <- P.many1 P.digit
  return $ read b

send_response :: Z.Socket a -> Response -> IO ()
send_response sock resp = do
  now <- getClockTime
  let okfine = BS.pack $
               render $
               setAttribute "headers" (response_headers resp) $
               setManyAttrib [("uuid",(response_uuid resp)),
                              ("size",(show $ length $ response_id resp)),
                              ("id", (response_id resp)),
                              ("now", (show now)),
                              ("clen", (show $ length $ response_body resp)),
                              ("sep", "\r\n"),
                              ("body",(response_body resp))] $
               newSTMP response_template
  Z.send sock okfine []

recv :: (Request -> IO Response) -> M2 -> [Z.Poll] -> IO ()
recv handle pub ((Z.S s _):_ss) = do
     req <- Z.receive s []
     case parse (BS.unpack req) of
       Left err -> error err
       Right rq -> do
         rsp <- handle rq
         case m2_publish_socket pub of
           Nothing -> error "Publish socket not connected?!"
           Just so -> send_response so rsp
{-
         now <- getClockTime
         let st = newSTMP $ BS.unpack respTemplate
         let okfine = BS.pack $ render $
                       setManyAttrib [("uuid",(respUUID rsp)),
                                      ("size",(show $ length $ respID rsp)),
                                      ("id", (respID rsp)),
                                      ("now", (show now)),
                                      ("clen", (show $ length $ respBody rsp)),
                                      ("sep", "\r\n"),
                                      ("body",(respBody rsp))] st
         case mPublishS pub of
           Nothing ->
             error "Need to connect the publish socket first!"
           Just so -> do
             Z.send so okfine []
-}


recv _ _ _ = return ()

poll :: Z.Socket a -> IO [Z.Poll]
poll sock = Z.poll [Z.S sock Z.InOut] 1000000

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

      return $ mong {
                   m2_publish_socket = Just pub,
                   m2_pull_socket = Just pull,
                   m2_context = Just ctx,
                   m2_uuid = Just uid
                 }

response_template :: String
response_template = 
  [$qq|$uuid$ $size$:$id$, HTTP/1.1 $status$ OK
Content-Type: text/html; charset=UTF-8
Connection: close
Content-Length: $clen$
Server: Mongrel2
Date: $now$
$headers:{a|$a.0$:$a.1$
}$

$body$

|]
