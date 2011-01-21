
module Web.Mongrel2.Types where

import Data.Default
import System.ZMQ

-- | An incoming request from the server.
data Request = Request {
  request_uuid :: String,
  request_path :: String,
  request_id :: String,
  
  request_headers :: [(String,String)],
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
  response_path :: String,
  
  response_body :: String,
  response_headers :: [(String,String)],
  response_status :: String,
  response_charset :: String,
  response_content_type :: String,
  response_target :: Maybe String
  } deriving(Show)

-- | Internal connection data.
data M2 = M2 {
  m2_publish :: String,
  m2_publish_socket :: Maybe (Socket Pub),
  m2_pull :: String,
  m2_pull_socket :: Maybe (Socket Pull),
  m2_context :: Maybe Context,
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

instance Default Request where
  def = Request {
    request_uuid = def,
    request_id = def,
    request_path = "/",
    
    request_headers = def,
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
    response_id = def,
    response_uuid = def,
    response_path = "/",
    
    response_charset = "UTF-8",
    response_content_type = "text/plain",
    response_body = def,
    response_headers = def,
    response_status = def,
    response_target = Nothing
    }
