g num1 num2 = num2
g (Ex 1) = Ex 1
g n@num1 x@num2 = x
g _ num2 = num2
g 1 2 = 3
g "hello" "world" = "goodbye"
g 'a' 'b' = 'c'
g 1.0 2.0 = 3.0
g Ex { a = 1 } = 1
g Ex { a = 1, b = 2 } = 2
g Ex {} = 2
g Blob{start, end} = [start, end]
g Blob{..} = [start, end]
g Blob{start, end = end', ..} = [start, end', name, path]
g c@Ex { a = 1 } = c
g (1) = 1
g (-(1)) = 1
g (Example a b c) = a
g (a :<: f) = 2
g ([]) = 1
g (1, 2) = 1
g ((Just 1), Just 2) = Just 1
g (Just a, Nothing) = Nothing
g (Left a, Right b, -(1)) = b
g [] = 0
g [a] = a
g (a:bs) = a
g (a:b:cs) = a
g (a:b:c:ds) = a
g ~a = 1
g ~(-(1)) = 1
g ~(~(a, b), c) = c
g x = case x of
  Just a | a < 10 -> True
         | a < 20 -> False
         | a > 19 -> True

f x = case x of
  Just a | a < 10, True, let b :: Int -> Int
                             b x = x -> True
  Nothing -> False

g x = case x of
  Just _ -> x where x = True
  Nothing -> False

g a = case a of
  Just a -> g
    where g = h
          h = 1
  Nothing -> i
    where i = j
          j = 2

g a = case a of
  Just a -> g
  Nothing -> i
  where g = 1
        i = 2

g = (
    \ a ->
      case a of
        A -> b
        )
b = reify tcName >>= \case
      TyConI (DataD _ _ _ _ cons _) -> do
        sigs <- filter (const makeSigs) <$> traverse genSig cons
        decs <- traverse genDecl cons
        pure $ sigs ++ decs

      _ ->
        fail "makeEff expects a type constructor"

g y | y < 10, y > 2 = True
    | otherwise = False

g x = do
  let z = x
  y <- a
  pure y

g x = do
  let z = x
  a
  y
  where y = pure a
        a = 1

g y@Example = y { a = 1, b = 2 }
g y@(Example { a = a', b = b' }) = y { a = a' + 1, b = b' ++ " !" }

g = read @Int
g = read @Int "5"
g = read @Prelude.Int "5"
g = read @Int . Prelude.id
g = read @Prelude.Int . Prelude.id
g = read @Int . id
g = read @Prelude.Int . Prelude.id
g = read @Int .id
g = read @Prelude.Int .Prelude.id
g = read @Int. id
g = read @Prelude.Int. Prelude.id
g = read @(x (Bar a))

g = Data.List.uncons . id
g = Data.List.uncons . Prelude.id
g = Data.List.uncons. id
g = Data.List.uncons. Prelude.id
g = Data.List.uncons .id
g = Data.List.uncons .Prelude.id
g = Data.List.uncons.id
g = Data.List.uncons.Prelude.id

g = Bar ': Baz
g = Bar ':<: Baz

g = (<>)
g = a <>

g = 1 where
h = 2 where

g = a where
  {- comment -}
g = a where
  {-
-}
g = a where
  --
g = a where
  {-# COLUMN 42 #-}

c = 2

eval' (a :< b) (fmap subtermValue -> QualifiedName name' iden) = foo

sort' :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort' = sortBy ?cmp

c (F (u :: Union e b) q) = b

g :: ( IvoryStore a
     , IvoryZero ('Stored a)
     , GetAlloc eff ~ 'Scope s
     ) => IBool
       -> Ivory eff a
       -> Ivory eff a
       -> Ivory eff a

emptyErrorReporter logger = pure reportError
  where
    reportError ErrorReport{..} = let
      msg = takeWhile (/= '\n') (displayException errorReportException)
      in logger msg errorReportContext
