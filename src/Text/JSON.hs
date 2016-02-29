module Text.JSON where

data JSValue
  = JSNull
  | JSBool !Bool
  | JSRational Bool !Rational
  | JSString JSString
  | JSArray [JSValue]
  | JSObject (JSObject JSValue)

newtype JSString = JSONString { fromJSString :: String }

toJSString :: String -> JSString
toJSString = JSONString

newtype JSObject value = JSONObject { fromJSObject :: [(String, value)] }

toJSObject :: [(String, value)] -> JSObject value
toJSObject = JSONObject

class JSON a where
  showJSON :: a -> JSValue
  showJSONs :: [a] -> JSValue
  showJSONs = JSArray . fmap showJSON
