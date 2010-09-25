{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Mongrel2 (
  M2(..)
  , Request(..)
  , Response(..)
  , MongrelHeaders(..)
  , connect
  , mpoll
  , getRequest
  , parse
  , sendResponse
  , recv
  ) where
import Web.Mongrel2.QQ

import qualified Text.ParserCombinators.Parsec as P
import Data.String.Utils (join,split,splitWs)
import qualified Data.ByteString.Char8 as BS
import System.Time (getClockTime)
import qualified Text.JSON as JS
import qualified System.ZMQ as Z
import Prelude hiding (lookup)
import Text.StringTemplate
import Control.Applicative
import Data.Default

-- | The Mongrel2 specific request headers.
data MongrelHeaders = MongrelHeaders {
      rhUUID :: String,
      rhID :: String,
      rhPath :: String
    } deriving (Show)

-- | An incoming request from the server.
data Request = Request {
      rMongrelHeaders :: MongrelHeaders,
      rHeaders :: String,
      rPath :: String,
      rMethod :: String,
      rVersion :: String,
      rURI :: String,
      rPattern :: String,
      rAccept :: String,
      rHost :: String,
      rQueryString :: String,
      rUserAgent :: String
    } deriving (Show)
  
-- | The response to send back.
data Response = Response {
      respUUID :: String,
      respID :: String,
      respBody :: String,
      respHeaders :: [(String,String)]
    } deriving(Show)

-- | The handlers internal data.
data M2 = M2 {
      mPublish :: String,
      mPublishS :: Maybe (Z.Socket Z.Pub),
      mSubscribe :: String,
      mSubscribeS :: Maybe (Z.Socket Z.Pull),
      mContext :: Maybe Z.Context,
      mUUID :: Maybe String
      }

instance Default M2 where
  def = M2 {
          mPublish = def,
          mPublishS = Nothing,
          mSubscribe = def,
          mSubscribeS = Nothing,
          mContext = Nothing,
          mUUID = Nothing
        }

instance Default MongrelHeaders where
  def = MongrelHeaders {
          rhUUID = def,
          rhID = def,
          rhPath = def
        }

instance Default Request where
  def = Request {
          rMongrelHeaders = def,
          rHeaders = def,
          rPath = def,
          rMethod = def,
          rVersion = def,
          rURI = def,
          rPattern = def,
          rAccept = def,
          rHost = def,
          rQueryString = def,
          rUserAgent = def
        }

instance Default Response where
  def = Response {
          respBody = def,
          respID = def,
          respUUID = def,
          respHeaders = def
        }

-- | Lookup a key from the JSON-encoded request from Mongrel2
lookup :: String -> (JS.JSObject JS.JSValue) -> Maybe String
lookup k bndl =
  case JS.valFromObj k bndl of
    JS.Ok v -> Just $ JS.fromJSString v
    _ -> Nothing

-- | Attempt to parse the JSON-encoded request body from Mongrel2
decode :: String -> Either String ( JS.JSObject JS.JSValue )
decode inc =
  case JS.decode inc of
    JS.Ok (JS.JSObject b) -> Right b
    _ -> Left "failed on json decode."

-- | Attempts to parse the entire request from Mongrel.
request_env :: String -> Either String Request
request_env request_body =
  case P.parse netStrings "" request_body of
    Left _ -> Left "Failed P.parse"
    Right (a,rst) ->
      case decode a of
        Left c -> Left c
        Right json ->
          case ((,,,,,,,) <$> lookup "PATH" json
                <*> lookup "METHOD" json
                <*> lookup "VERSION" json
                <*> lookup "URI" json
                <*> lookup "PATTERN" json
                <*> lookup "Accept" json
                <*> lookup "Host" json
                <*> lookup "User-Agent" json) of
            Nothing -> Left "Failed an applicative lookup."
            Just ( path',method',version',uri',
                   pattern',accept',host',user_agent') -> do
              let b = maybe "" id $ lookup "Cookie" json

              Right $ def { rPath = path'
                          , rMethod = method'
                          , rVersion = version' 
                          , rURI = uri'
                          , rHeaders = b
                          , rPattern = pattern'
                          , rAccept = accept'
                          , rHost = host'
                          , rUserAgent = user_agent'
                          , rQueryString = qstring rst
                          }
    where
      qstr :: P.Parser String
      qstr = do
        n <- number
        _ <- P.char ':'
        x <- P.count n P.anyChar
        return x
      qstring :: String -> String
      qstring fx = do
        case P.parse qstr "" fx of
          Left _ -> ""
          Right y -> y

parse :: String -> Either String Request
parse request = do
  case msplit request of
    Left a -> Left a
    Right (b,c) ->
      case preamble b of
        Left d -> Left d
        Right request_headers ->
          case request_env c of
            Left e -> Left e
            Right req -> Right req { rMongrelHeaders = request_headers }

-- Love to http://www.weavejester.com/node/7
netStrings :: P.Parser (String,String)
netStrings = do
  n <- number
  _ <- P.char ':'
  s <- P.count n P.anyChar
  _ <- P.char ','
  rst <- P.many P.anyChar
  return (s,rst)

number :: P.Parser Int
number = do
  b <- P.many1 P.digit
  return $ read b

preamble :: String -> Either String MongrelHeaders
preamble b =
  case splitWs b of
    [uid,rid,path] -> Right $ def { rhUUID = uid,
                                    rhID = rid,
                                    rhPath = path
                                  }
    _ -> Left "splitWs failed."
  
msplit :: String -> Either String (String,String)
msplit a = case split " " a of
             [] -> Left "failed on split."
             b -> Right ((join " " $ take 3 b),(join " " $ drop 3 b))

getRequest :: Z.Socket a -> IO BS.ByteString
getRequest s = Z.receive s []

sendResponse :: Z.Socket a -> Response -> IO ()
sendResponse sock resp = do
  now <- getClockTime
  let okfine = BS.pack $
               render $
               setAttribute "headers" (respHeaders resp) $
               setManyAttrib [("uuid",(respUUID resp)),
                              ("size",(show $ length $ respID resp)),
                              ("id", (respID resp)),
                              ("now", (show now)),
                              ("clen", (show $ length $ respBody resp)),
                              ("sep", "\r\n"),
                              ("body",(respBody resp))] $ newSTMP respTemplate 
  Z.send sock okfine []

recv :: (Request -> IO Response) -> Z.Socket a -> [Z.Poll] -> IO ()
recv handle pub ((Z.S s _):_ss) = do
     req <- Z.receive s []
     case parse (BS.unpack req) of
       Left _err -> return ()
       Right rq -> do
         rsp <- handle rq
         now <- getClockTime
         let st = newSTMP respTemplate
         let okfine = BS.pack $ render $
                       setManyAttrib [("uuid",(respUUID rsp)),
                                      ("size",(show $ length $ respID rsp)),
                                      ("id", (respID rsp)),
                                      ("now", (show now)),
                                      ("clen", (show $ length $ respBody rsp)),
                                      ("sep", "\r\n"),
                                      ("body",(respBody rsp))] st
         Z.send pub okfine []
recv _ _ _ = return ()

mpoll :: Z.Socket a -> IO [Z.Poll]
mpoll sock = Z.poll [Z.S sock Z.In] 1000000

connect :: M2 -> IO M2
connect mong = do
  case ((,) <$> mPublishS mong
        <*> mSubscribeS mong ) of
    Just _ -> return mong
    Nothing -> do
      ctx <- Z.init 1
      pub <- Z.socket ctx Z.Pub
      sub <- Z.socket ctx Z.Pull
      
      let uid = case mUUID mong of
            Just v -> v
            Nothing ->  "82209006-86FF-4982-B5EA-D1E29E55D481"
      Z.connect sub $ mSubscribe mong
      Z.setOption sub $ Z.Identity uid
      
      Z.connect pub $ mPublish mong
      Z.setOption pub $ Z.Identity uid

      return $ mong {
                   mPublishS = Just pub,
                   mSubscribeS = Just sub,
                   mContext = Just ctx,
                   mUUID = Just uid
                 }

respTemplate :: String
respTemplate = [$qq|$uuid$ $size$:$id$, HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8
Connection: close
Content-Length: $clen$
Server: Mongrel2
Date: $now$
$headers:{a|$a.0$:$a.1$
}$

$body$
|]
