{-# LANGUAGE OverloadedStrings #-}
module Web.Mongrel2.Parsing (m2_parse) where

import qualified Data.Text.Lazy as T
import Control.Applicative hiding (many)
import Text.Parsec.Text
import Text.Parsec hiding ((<|>))
import Data.Default
import qualified Text.JSON as JS

import Web.Mongrel2.Types

m2_parse :: T.Text -> Either T.Text Request
m2_parse request =
  case parse request_split "" request of
    Right (uui,seqq,pat,blk) ->
      case request_env blk of
        Left e -> Left e
        Right req ->
          Right req { request_uuid = uui
                    , request_id = seqq
                    , request_path = pat }
    Left a -> Left $ T.pack $ show a
 where   
   request_split :: Parser (T.Text,T.Text,T.Text,T.Text)
   request_split = do
     uui <- many $ noneOf " "
     _ <- space
     iid <- many $ noneOf " "
     _ <- space
     path <- many $ noneOf " "
     _ <- space
     rest <- many anyToken
             
     return (T.pack uui,T.pack iid,T.pack path,T.pack rest)
     
   request_env :: T.Text -> Either T.Text Request
   request_env request_body =
     case parse qstr "" request_body of
       Left x -> Left $ T.pack $ show x
       Right (headers_,query_string_) ->
         case JS.decode (T.unpack headers_) of
           JS.Ok (JS.JSObject json) -> do
             let unjs =
                   concat $
                   map (\(x,y') ->
                         case y' of
                           JS.JSString y -> [(T.pack x,T.pack $ JS.fromJSString y)]
                           _ -> []
                       ) $ JS.fromJSObject json
          
             Right $ def { request_path = ml "PATH" json
                         , request_method = string_to_method $ ml "METHOD" json
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
   ml :: T.Text -> JS.JSObject JS.JSValue -> T.Text
   ml k b = maybe "" id $ mlookup k b
  
   qstr :: Parser (T.Text,T.Text)
   qstr = do
    n <- number
    _ <- char ':'
    x <- count n anyChar
    _ <- char ','
    nx <- number
    _ <- char ':'
    xy <- count nx anyChar
    
    return (T.pack x,T.pack xy)

number :: Parser Int
number = many1 digit >>= (return . read)

mlookup :: T.Text -> JS.JSObject JS.JSValue -> Maybe T.Text
mlookup key bndl =
  -- TODO: Not so sure that the alternate toLower is needed.
  mlookup' key bndl <|> mlookup' (T.toLower key) bndl
 where
   mlookup' :: T.Text -> JS.JSObject JS.JSValue -> Maybe T.Text
   mlookup' k b = 
     case JS.valFromObj (T.unpack k) b of
       JS.Ok v -> Just $ T.pack $ JS.fromJSString v
       _ -> Nothing

string_to_method :: T.Text -> RequestMethod
string_to_method "GET" = GET
string_to_method "POST" = POST
string_to_method "PUT" = PUT
string_to_method "DELETE" = DELETE
string_to_method "HEAD" = HEAD
string_to_method lx = error $ "Unknown method: " ++ T.unpack lx

