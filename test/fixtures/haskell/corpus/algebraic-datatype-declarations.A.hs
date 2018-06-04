data N
data N a = N a
data N a = N !a
data N a b = N !a b

data B = A
       | A0
       | A9
       | Aa
       | A_
       | Az'

data N = N { a :: Int }
data N = N { a, b :: Int }
data N = N { a :: !Int, b :: Int }
data N = N { a, b :: {-#  UNPACK #-} !Int, c :: String }
data N = N { a :: Int } | O { b :: String }
data N = N { b :: Int } | O { c :: String }
