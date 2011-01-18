
module Web.Mongrel2.Parsing where

import Text.ParserCombinators.Parsec
import Data.Default
import Text.JSON
import Char (toLower)
import Web.Mongrel2.Types

m2_parse :: String -> Either String Request
m2_parse request =
  case parse request_split "" request of
    Right (nam,seqq,pat,blk) ->
      case request_env blk of
        Left e -> Left e
        Right req ->
          Right req { request_mongrel_headers =
                         def { header_id = seqq
                             , header_uuid = nam
                             , header_path = pat } }
    _ -> Left "Unsupported or mis-parsed request."
 where
   request_split :: Parser (String,String,String,String)
   request_split = do
     name <- string $ noneOf " "
     char ' '
     sequence <- string $ noneOf " "
     char ' '
     path <- string $ noneOf " "
     char ' '
     rest <- string
     return (name,sequence,path,rest)
     
   request_env :: String -> Either String Request
   request_env request_body =
     case parse qstr "" request_body of
       Left x -> Left $ show x
       Right (headers_,query_string_) ->
         case decode headers_ of
           Ok (JSObject json) -> do
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
number = do
  b <- many1 digit
  return $ read b

mlookup :: String -> JSObject JSValue -> Maybe String
mlookup key bndl =
  mlookup' key bndl <|> mlookup' (map toLower key) bndl
 where
   mlookup' :: String -> JSObject JSValue -> Maybe String
   mlookup' k b = 
     case valFromObj k b of
       Ok v -> Just $ fromJSString v
       _ -> Nothing
