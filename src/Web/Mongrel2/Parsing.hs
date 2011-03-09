
module Web.Mongrel2.Parsing (m2_parse) where

import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Data.Default
import Text.JSON
import Char (toLower)

import Web.Mongrel2.Types

m2_parse :: String -> Either String Request
m2_parse request =
  case parse request_split "" request of
    Right (uui,seqq,pat,blk) ->
      case request_env blk of
        Left e -> Left e
        Right req ->
          Right req { request_uuid = uui
                    , request_id = seqq
                    , request_path = pat }
    Left a -> Left $ show a
 where   
   request_split :: Parser (String,String,String,String)
   request_split = do
     uui <- many $ noneOf " "
     _ <- space
     iid <- many $ noneOf " "
     _ <- space
     path <- many $ noneOf " "
     _ <- space
     rest <- many anyToken
     
     return (uui,iid,path,rest)
     
   request_env :: String -> Either String Request
   request_env request_body =
     case parse qstr "" request_body of
       Left x -> Left $ show x
       Right (headers_,query_string_) ->
         case decode headers_ of
           Ok (JSObject json) -> do
             let unjs =
                   concat $
                   map (\(x,y') ->
                         case y' of
                           JSString y -> [(x,fromJSString y)]
                           _ -> []
                       ) $ fromJSObject json
          
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
   ml :: String -> JSObject JSValue -> String
   ml k b = maybe "" id $ mlookup k b
  
   qstr :: Parser (String,String)
   qstr = do
    n <- number
    _ <- char ':'
    x <- count n anyChar
    _ <- char ','
    nx <- number
    _ <- char ':'
    xy <- count nx anyChar
    
    return (x,xy)

number :: Parser Int
number = many1 digit >>= (return . read)

mlookup :: String -> JSObject JSValue -> Maybe String
mlookup key bndl =
  -- TODO: Not so sure that the alternate toLower is needed.
  mlookup' key bndl <|> mlookup' (map toLower key) bndl
 where
   mlookup' :: String -> JSObject JSValue -> Maybe String
   mlookup' k b = 
     case valFromObj k b of
       Ok v -> Just $ fromJSString v
       _ -> Nothing

string_to_method :: String -> RequestMethod
string_to_method "GET" = GET
string_to_method "POST" = POST
string_to_method "PUT" = PUT
string_to_method "DELETE" = DELETE
string_to_method "HEAD" = HEAD
string_to_method lx = error $ "Unknown method: " ++ lx

