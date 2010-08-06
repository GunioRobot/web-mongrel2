
module Web.Mongrel2 ( connect,
                          parse,
                          Mongrel2(..),
                          Request(..),
                          Response(..),
                          RequestHeaders(..) 
                        ) where

import qualified System.ZMQ as Z
import Control.Applicative
import qualified System.UUID.V4 as UU
import Data.Maybe
import Data.Default
import qualified Text.JSON as JS
import Control.Monad (liftM)

import Prelude hiding (lookup)
import qualified Text.ParserCombinators.Parsec as P
import Data.String.Utils (join,split,splitWs)

{-  TYPES   -}

data RequestHeaders = RequestHeaders {
      r_uuid :: String,
      r_id :: String,
      r_path :: String
    } deriving (Show)
                    
data Request = Request {
      m_request_headers :: RequestHeaders,
      m_path :: String,
      m_method :: String,
      m_version :: String,
      m_uri :: String,
      m_pattern :: String,
      m_accept :: String,
      m_host :: String,
      m_query_string :: String,
      m_user_agent :: String
    } deriving (Show)

{-
UUID SIZE:ID, BODY
-}
data Response = Response {
      rUUID :: String,
      rID :: String,
      rBody :: String
    }

data Mongrel2 = Mongrel2 {
      mpublish :: String,
      mpublishs :: Maybe (Z.Socket Z.Pub),
      msubscribe :: String,
      msubscribes :: Maybe (Z.Socket Z.Up),
      mcontext :: Maybe Z.Context,
      uuid :: Maybe String
    }

instance Default Mongrel2 where
  def = Mongrel2 {
          mpublish = def,
          mpublishs = Nothing,
          msubscribe = def,
          msubscribes = Nothing,
          mcontext = Nothing,
          uuid = Nothing
        }

instance Default RequestHeaders where
  def = RequestHeaders {
          r_uuid = def,
          r_id = def,
          r_path = def
        }

instance Default Request where
  def = Request {
          m_request_headers = def,
          m_path = def,
          m_method = def,
          m_version = def,
          m_uri = def,
          m_pattern = def,
          m_accept = def,
          m_host = def,
          m_query_string = def,
          m_user_agent = def
        }

instance Default Response where
  def = Response {
          rBody = def,
          rID = def,
          rUUID = def
        }


{- Parser -}

lookup :: String -> (JS.JSObject JS.JSValue) -> Maybe String
lookup k bndl =
  case JS.valFromObj k bndl of
    JS.Ok v -> Just $ JS.fromJSString v
    _ -> Nothing

decode :: String -> Either String ( JS.JSObject JS.JSValue )
decode inc =
  case JS.decode inc of
    JS.Ok (JS.JSObject b) -> Right b
    _ -> Left "failed on json decode."

request_env :: String -> Either String Request
request_env request_body =
  case P.parse netStrings "" request_body of
    Left _ -> Left "Failed P.parse"
    Right (a,_rst) ->
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
                                            
              let rq = def { m_path = path'
                           , m_method = method'
                           , m_version = version' 
                           , m_uri = uri'
                           , m_pattern = pattern'
                           , m_accept = accept'
                           , m_host = host'
                           , m_user_agent = user_agent' }
              case lookup "QUERY" json of
                Nothing -> Right rq
                Just v -> Right rq { m_query_string = v }

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
            Right req -> Right req { m_request_headers = request_headers }

-- Love to http://www.weavejester.com/node/7
netStrings :: P.Parser (String,String)
netStrings = do
  n <- number
  _ <- P.char ':'
  s <- P.count n P.anyChar
  _ <- P.char ','
  rst <- P.many P.anyChar
  return (s,rst)
 where      
   number :: P.Parser Int
   number = do
     b <- P.many1 P.digit
     return $ read b

preamble :: String -> Either String RequestHeaders
preamble b =
  case splitWs b of
    [uid,rid,path] -> Right $ def { r_uuid = uid,
                                    r_id = rid,
                                    r_path = path
                                  }
    _ -> Left "splitWs failed."
  
msplit :: String -> Either String (String,String)
msplit a =
     case split " " a of
       [] -> Left "failed on split."
       b -> Right ((join " " $ take 3 b),(join " " $ drop 3 b))


{-  Functions  -}


connect :: Mongrel2 -> IO Mongrel2
connect mong = do
  case ((,) <$> mpublishs mong
        <*> msubscribes mong) of
    Just _ -> return mong
    Nothing -> do
      -- Trash the context?  Not sure if this is a good idea.
      -- Nor do I know if building both sockets off of same
      -- context is a bad thing or not.  We'll soon see, I'm sure.
      ctx <- Z.init 1
      pub <- Z.socket ctx Z.Pub
      sub <- Z.socket ctx Z.Up
      
      uid <- liftM show UU.uuid

      Z.connect sub $ msubscribe mong
      Z.setOption sub $ Z.Identity uid

      Z.connect pub $ mpublish mong
      Z.setOption pub $ Z.Identity uid

      return $ mong {
                   mpublishs = Just pub,
                   msubscribes = Just sub,
                   mcontext = Just ctx, 
                   uuid = Just (show uid)
                 }
        

