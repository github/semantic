module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Map

a :: Term String ()
a = () :< (Keyed $ fromList [
  ("hello", () :< Indexed [ () :< Leaf "hi" ]),
  ("goodbye", () :< Leaf "goodbye") ])

b :: Term String ()
b = () :< (Keyed $ fromList [
  ("hello", () :< Indexed []),
  ("goodbye", () :< Indexed []) ])

d :: Diff String ()
d = Free . Annotated ((), ()) . Keyed $ fromList [
  ("hello", Free . Annotated ((), ()) $ Indexed [ Pure . Delete $ () :< Leaf "hi" ]),
  ("goodbye", Pure $ Replace (() :< Leaf "goodbye") (() :< Indexed [])) ]

main :: IO ()
main = putStrLn "hello world"
