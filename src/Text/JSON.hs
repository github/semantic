module Text.JSON where

data JSValue
  = JSNull
  | JSBool !Bool
  | JSRational Bool !Rational
  | JSString JSString
  | JSArray [JSValue]
  | JSObject (JSObject JSValue)
  deriving (Eq, Show)

newtype JSString = JSONString { fromJSString :: String }
  deriving (Eq, Show)

toJSString :: String -> JSString
toJSString = JSONString

newtype JSObject value = JSONObject { fromJSObject :: [(String, value)] }
  deriving (Eq, Show)

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
