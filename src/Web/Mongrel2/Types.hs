
module Web.Mongrel2.Types where

import Data.Default
import System.ZMQ

-- | An incoming request from the server.
data Request = Request {
  -- | The uuid of the server.
  request_uuid :: String,
  -- | The path as passed in from Mongrel2.
  request_path :: String,
  -- | The individual request id.
  request_id :: String,
 
  -- | Any headers passed in from the server are copied to here.
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
-- 'response_uuid', 'response_id', and 'response_path' are passed from the request and are needed for the response back to Mongrel2.
data Response = Response {
  -- | The uuid of the server.
  response_uuid :: String,
  -- | The request id.
  response_id :: String,
  -- | The request path.
  response_path :: String,

  -- | This is for, for example, cookies.
  --
  -- > def {
  -- >   response_headers = [("cookies","uid=10239120192")]
  -- > }
  --
  -- Of course, they will need to be encoded, etc.
  -- May I be so bold as to suggest @Web.Encodings@ ? :) 
  --
  response_headers :: [(String,String)],

  -- | 404, 302, etc.
  response_status :: String,
  -- | Defaults to UTF-8
  response_charset :: String,
  -- | Defaults to text/plain
  response_content_type :: String,
  response_body :: String
  } deriving(Show)

-- | Internal connection data.
-- 'm2_publish' and 'm2_pull' can be any ZeroMQ type that Mongrel2 supports.
data M2 = M2 {
  -- | The address to connect for replies back to Mongrel2.
  m2_publish :: String,
  m2_publish_socket :: Maybe (Socket Pub),
  -- | The address to poll for requests.
  m2_pull :: String,
  m2_pull_socket :: Maybe (Socket Pull),
  m2_context :: Maybe Context,
  -- | The application identifier.
  -- This defaults to a standard uuid, but you probably want to supply your own.
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
    response_headers = def,
    response_status = def,
    response_body = def
    }
