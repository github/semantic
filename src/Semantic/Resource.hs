{-# LANGUAGE GADTs, TypeOperators, RankNTypes #-}

module Semantic.Resource
  ( Resource (..)
  , bracket
  , runResource
  ) where

import           Control.Monad.Effect
import           Control.Monad.IO.Class
import qualified Control.Exception as Exc

data Resource m output where
  Resource :: m res -> (res -> m any) -> (res -> m output) -> Resource m output

instance PureEffect Resource
instance Effect Resource where
  handleState c dist (Request (Resource fore aft go) k)
    = Request (Resource (dist (fore <$ c)) (dist . fmap aft) (dist . fmap go)) (dist . fmap k)

bracket :: (Member Resource effs, Effectful m)
        => m effs res
        -> (res -> m effs any)
        -> (res -> m effs b)
        -> m effs b
bracket fore aft go = send (Resource (lowerEff fore) (lowerEff . aft) (lowerEff . go))

runResource :: (Member (Lift IO) effects, PureEffects effects)
            => (forall x . Eff effects x -> IO x)
            -> Eff (Resource ': effects) a
            -> Eff effects a
runResource handler = interpret (\(Resource fore aft go)
                                 -> liftIO (Exc.bracket
                                            (handler (runResource handler fore))
                                            (handler . runResource handler . aft)
                                            (handler . runResource handler . go)))
