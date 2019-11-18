module FCrossover where

import Data.Bifunctor 
import FunctorCombo.Functor
import FunctorCombo.DHoley
import Control.Comonad.Cofree (Cofree(..))

newtype Fix f = Fix { unFix :: f (Fix f) }

type Context f = [Der f (Fix f)]

type Zipper f = (Context f, Fix f)

down :: Holey f => Zipper f -> f (Zipper f)
down (ds', t) = (fmap.first) (:ds') (extract (unFix t))

crossoverPoints :: Holey f => Fix f -> Cofree f (Zipper f)
crossoverPoints = h [] where h ctx t = let pos = (ctx, t) in pos :< fmap (uncurry h) (down pos)


