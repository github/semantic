module Text.JSON where

import Data.List
import Data.Ratio

data JSValue
  = JSNull
  | JSBool !Bool
  | JSRational Bool !Rational
  | JSString JSString
  | JSArray [JSValue]
  | JSObject (JSObject JSValue)
  deriving (Eq)

instance Show JSValue where
  show JSNull = "null"
  show (JSBool isTrue) = if isTrue then "true" else "false"
  show (JSRational True rational) = show (fromRational rational :: Double)
  show (JSRational False rational) = show (numerator rational :: Integer)
  show (JSString s) = show s
  show (JSArray v) = "[" ++ intercalate "," (show <$> v) ++ "]"
  show (JSObject o) = show o

newtype JSString = JSONString { fromJSString :: String }
  deriving (Eq)

instance Show JSString where
  show (JSONString s) =  "\"" ++ (s >>= escape) ++ "\""
    where escape '\\' = "\\"
          escape '\n' = "\\n"
          escape '\r' = "\\r"
          escape '\t' = "\\t"
          escape '\b' = "\\b"
          escape '\"' = "\\\""
          escape '\'' = "\\'"
          escape c = pure c

toJSString :: String -> JSString
toJSString = JSONString

newtype JSObject value = JSONObject { fromJSObject :: [(String, value)] }
  deriving (Eq)

instance Show value => Show (JSObject value) where
  show (JSONObject pairs) = "{" ++ intercalate "," (showPair <$> pairs) ++ "}"
    where showPair (key, value) = show (toJSString key) ++ ":" ++ show value

toJSObject :: [(String, value)] -> JSObject value
toJSObject = JSONObject

class JSON a where
  showJSON :: a -> JSValue
  showJSONs :: [a] -> JSValue
  showJSONs = JSArray . fmap showJSON

instance JSON Char where
  showJSON = showJSONs . pure
  showJSONs = JSString . toJSString

instance JSON Integer where
  showJSON = JSRational False . fromIntegral

instance JSON Int where
  showJSON = JSRational False . fromIntegral

instance JSON a => JSON [a] where
  showJSON = JSArray . fmap showJSON
