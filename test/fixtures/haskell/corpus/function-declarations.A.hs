f num1 num2 = num2
f (Ex 1) = Ex 1
f n@num1 x@num2 = x
f _ num2 = num2
f 1 2 = 3
f "hello" "world" = "goodbye"
f 'a' 'b' = 'c'
f 1.0 2.0 = 3.0
f Ex { a = 1 } = 1
f Ex { a = 1, b = 2 } = 2
f Ex {} = 2
f Blob{start, end} = [start, end]
f Blob{..} = [start, end]
f Blob{start, end = end', ..} = [start, end', name, path]
f (1) = 1
f (-(1)) = 1
f (Example a b c) = a
f (a :<: f) = 2
f ([]) = 1
f (1, 2) = 1
f ((Just 1), Just 2) = Just 1
f (Just a, Nothing) = Nothing
f (Left a, Right b, -(1)) = b
f [] = 0
f [a] = a
f (a:bs) = a
f (a:b:cs) = a
f (a:b:c:ds) = a
f ~a = 1
f ~(-(1)) = 1
f ~(~(a, b), c) = c
f x = case x of
  Just a | a < 10 -> True
         | a < 20 -> False
         | a > 19 -> True

g x = case x of
  Just a | a < 10, True, let b :: Int -> Int
                             b x = x -> True
  Nothing -> False

f x = case x of
  Just _ -> x where x = True
  Nothing -> False

f a = case a of
  Just a -> g
    where g = h
          h = 1
  Nothing -> i
    where i = j
          j = 2

f a = case a of
  Just a -> g
  Nothing -> i
  where g = 1
        i = 2

f = (
    \ a ->
      case a of
        A -> b
        )
a = reify tcName >>= \case
      TyConI (DataD _ _ _ _ cons _) -> do
        sigs <- filter (const makeSigs) <$> traverse genSig cons
        decs <- traverse genDecl cons
        pure $ sigs ++ decs

      _ ->
        fail "makeEff expects a type constructor"

f x | x < 10, x > 2 = True
    | otherwise = False

f x = do
  let z = x
  y <- a
  pure y

f x = do
  let z = x
  a
  y
  where y = pure a
        a = 1

f y@Example = y { a = 1, b = 2 }
f y@(Example { a = a', b = b' }) = y { a = a' + 1, b = b' ++ " !" }

f = read @Int
f = read @Int "5"
f = read @Prelude.Int "5"
f = read @Int . Prelude.id
f = read @Prelude.Int . Prelude.id
f = read @Int . id
f = read @Prelude.Int . Prelude.id
f = read @Int .id
f = read @Prelude.Int .Prelude.id
f = read @Int. id
f = read @Prelude.Int. Prelude.id
f = read @(x (Bar a))

f = Data.List.uncons . id
f = Data.List.uncons . Prelude.id
f = Data.List.uncons. id
f = Data.List.uncons. Prelude.id
f = Data.List.uncons .id
f = Data.List.uncons .Prelude.id
f = Data.List.uncons.id
f = Data.List.uncons.Prelude.id

f = Bar ': Baz
f = Bar ':<: Baz

f = (<>)
f = a <>

f = 1 where
g = 2 where

f = a where
  {- comment -}
f = a where
  {-
-}
f = a where
  --
f = a where
  {-# COLUMN 42 #-}

b = 2

eval (a :< b) (fmap subtermValue -> QualifiedName name iden) = bar

sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort = sortBy ?cmp

b (E (u :: Union e b) q) = a

ifte :: ( IvoryStore a
        , IvoryZero ('Stored a)
        , GetAlloc eff ~ 'Scope s
        ) => IBool
          -> Ivory eff a
          -> Ivory eff a
          -> Ivory eff a

nullErrorReporter logger = pure reportError
  where
    reportError ErrorReport{..} = let
      msg = takeWhile (/= '\n') (displayException errorReportException)
      in logger msg errorReportContext
