module FunctorGP.Genetic where

-- Given cs and n a's return an a 
data F :: [Type] -> Nat -> Type -> Type where
  F :: HFunc cs (Op n a) -> F cs n a

data ConsTable :: [[Type]] -> [Nat] -> Type -> Type where
  Null :: ConsTable '[] '[] a
  Cell :: F cs n a -> ConsTable css ns a -> ConsTable (cs ': css) (n ': ns) a

type Terminals css ns a = ConsTable (TerminalTys css ns) (TerminalNs css ns) a

partition :: ConsTable css ns a -> (Terminals css ns a, Functions css ns a)

data TermF :: [Type] -> Nat -> Type -> Type where
  TermF :: F cs n a -> Vect n a -> TermF cs n a 


