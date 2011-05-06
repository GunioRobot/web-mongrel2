{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Mongrel2.Types where

import Data.Default
import System.ZMQ
import qualified Data.Text as T

data RequestMethod = POST
                   | GET
                   | HEAD
                   | PUT
                   | DELETE

instance Show RequestMethod where
  show POST = "POST"
  show GET = "GET"
  show HEAD = "HEAD"
  show PUT = "PUT"
  show DELETE = "DELETE"

instance Default RequestMethod where
  def = GET
  
instance Default T.Text where
  def = ""
  
-- | An incoming request from the server.
data Request = Request {
  -- | The uuid of the server.
  request_uuid :: T.Text,
  -- | The path as passed in from Mongrel2.
  request_path :: T.Text,
  -- | The individual request id.
  request_id :: T.Text,
 
  -- | Any headers passed in from the server are copied to here.
  request_headers :: [(T.Text,T.Text)],
  request_method :: RequestMethod,
  request_version :: T.Text,
  request_uri :: T.Text,
  request_pattern :: T.Text,
  request_accept :: T.Text,
  request_host :: T.Text,
  request_query_string :: T.Text,
  request_user_agent :: T.Text
  } deriving (Show)

-- | The response to send back.
-- 'response_uuid', 'response_id', and 'response_path' are passed from the request and are needed for the response back to Mongrel2.
data Response = Response {
  -- | The uuid of the server.
  response_uuid :: T.Text,
  -- | The request id.
  response_id :: T.Text,
  -- | The request path.
  response_path :: T.Text,

  -- | This is for, for example, cookies.
  --
  -- > def {
  -- >   response_headers = [("cookies","uid=10239120192")]
  -- > }
  --
  -- Of course, they will need to be encoded, etc.
  -- May I be so bold as to suggest @Web.Encodings@ ? :) 
  --
  response_headers :: [(T.Text,T.Text)],

  -- | 404, 302, etc.
  response_status :: T.Text,
  -- | Defaults to UTF-8
  response_charset :: T.Text,
  -- | Defaults to text/plain
  response_content_type :: T.Text,
  response_body :: T.Text
  } deriving (Show)

-- | Internal connection data.
-- 'm2_publish' and 'm2_pull' can be any ZeroMQ type that Mongrel2 supports.
data M2 = M2 {
  -- | The address to connect for replies back to Mongrel2.
  m2_publish :: T.Text,
  m2_publish_socket :: Maybe (Socket Pub),
  -- | The address to poll for requests.
  m2_pull :: T.Text,
  m2_pull_socket :: Maybe (Socket Pull),
  m2_context :: Maybe Context,
  -- | The application identifier.
  -- This defaults to a standard uuid, but you probably want to supply your own.
  m2_uuid :: Maybe T.Text
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
