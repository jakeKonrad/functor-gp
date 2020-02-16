-- Proof of concept for an idea I'm calling functor crossover. Inspired 
-- by Conal Elliot's blog posts on derivatives of datatypes, functor crossover is
-- a generic implementation of subtree crossover from genetic programming. As described
-- in Elliot's blog, the Holey typeclass captures a class of datatypes that are "tree-like" 
-- or regular, the notion of regular coming from the paper, The Derivitive of 
-- Datatype is the Type of Its One Holed Context. These "tree-like" datatypes are 
-- exactly the class of types used in genetic programming, hence the crossover operator
-- being subtree crossover.
{-# LANGUAGE DeriveFunctor #-}
module FCrossover where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor(first))
import FunctorCombo.DHoley (Holey(..))
import Control.Comonad.Cofree (Cofree(..), coiter)
import Control.Monad.Free (Free(..))

-- Fixed type of functors.
newtype Fix f = Fix { unFix :: f (Fix f) }

-- In this module are some copied definitions from Elliot's blog, these
-- are: 'Context f', 'Zipper f' and 'down'.

type Context f = [Der f (Fix f)]

type Zipper f = (Context f, Fix f)

down :: Holey f => Zipper f -> f (Zipper f)
down (ds', t) = (fmap.first) (:ds') (extract (unFix t))

-- This deviates from Elliot's definition as I need it to, "zip"
-- a zipper all the way back up.
up :: Holey f => Zipper f -> Fix f
up ([], f)      = f
up (df:rest, f) = up (rest, Fix (fillC df f))

-- This function generates all the possible crossover points of a given tree. In this 
-- context a crossover point is simply a zipper. This function then annotates each
-- node of the tree with a zipper that points to that node.
crossoverPoints :: Holey f => Fix f -> Cofree f (Zipper f)
crossoverPoints t = coiter down ([], t)

-- I need the notion of choice.  
newtype Choice a = MkChoice { runChoice :: Free ChoiceF a }

data ChoiceF r = EmptyF | ChooseF r r  
               deriving Functor

instance Functor Choice where
  fmap f = MkChoice . fmap f . runChoice

instance Applicative Choice where
  pure = MkChoice . Pure
  (MkChoice l) <*> (MkChoice r) = MkChoice (l <*> r)

instance Monad Choice where
  x >>= k = MkChoice $ join $ fmap (runChoice . k) $ runChoice x 

instance Semigroup (Choice a) where
  (MkChoice (Free EmptyF)) <> rhs           = rhs
  lhs           <> (MkChoice (Free EmptyF)) = lhs
  lhs           <> rhs                      = MkChoice $ Free (ChooseF (runChoice lhs) (runChoice rhs))

instance Monoid (Choice a) where
  mempty = MkChoice (Free EmptyF)

-- Given a foldable structure, choose one of the elements.
chooseAny :: Foldable t => t a -> Choice a
chooseAny = foldMap (MkChoice . Pure) 

-- Given two parents, choose a crossover point from each. Then, as crossover points here are zippers
-- and zippers are a pair with the path taken to get to the subtree and the subtree, simply swap the
-- two subtrees and zip them up.
fcrossover :: (Foldable f, Holey f) => Fix f -> Fix f -> Choice (Fix f, Fix f)
fcrossover parent1 parent2 = do
    (ctx1, subtree1) <- chooseAny (crossoverPoints parent1)
    (ctx2, subtree2) <- chooseAny (crossoverPoints parent2)
    pure (up (ctx1, subtree2), up (ctx2, subtree1))

