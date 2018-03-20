module Haskell01 where

-- #TODO: put your macid in the following string
macid = "Replace Me"

{- ------------------------------------------------------------------------------------------------------------
 - Part 1: Either
 - ------------------------------------------------------------------------------------------------------------
 -}
data MyEither a b = MyLeft a | MyRight b
  deriving Show

instance Functor (MyEither a) where
  fmap f (MyRight x) = MyRight $ f x
  fmap _ (MyLeft x) = MyLeft x


instance Applicative (MyEither a) where
  pure val = MyRight val
  MyRight f <*> x = fmap f x
  MyLeft y  <*> _ = MyLeft y

{- Task 1:                          (1 Mark)
 -     Complete the following Monad Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  return a >>= f   ==  f a
 -       Right Identity: m >>= return     ==  m
 -       Associative:    (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
 - ------------------------------------------------------------------------------------------------------------
 -}
instance Monad (MyEither a) where
  return x = MyRight x
  MyLeft a >>= f = MyLeft a
  MyRight x >>= f = f x

{- ------------------------------------------------------------------------------------------------------------
 - Part 2: List
 - ------------------------------------------------------------------------------------------------------------
 -}
data List a = Cons a (List a) | Empty
  deriving Show

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Empty       = Empty

{- Task 2:                            (1 Mark)
 -     Complete the following Monoid Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  mempty `mappend` x  == x
 -       Right Identity: x `mappend` mempty  == x
 -       Associative:    (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: mappend corresponds to the (++) function on built-in Haskell lists
 -}
instance Monoid (List a) where
  mempty = Empty
  mempty      `mappend` ys     = ys
  xs          `mappend` mempty = xs
  (Cons x xs) `mappend` ys     = Cons x (xs `mappend` ys)

{- Task 3:                                 (1 Mark)
 -     Complete the following Applicative Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Identity:       pure id <*> xs    == xs
 -       Homomorphism:   pure f <*> pure x == pure (f x)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: [f1,...fn] <*> [x1,...,xi] == f1 x1 `mappend` ... f1 xi `mappend` ... fn x1 `mappend` ... fn xi
 -}
instance Applicative List where
      pure x = Cons x Empty
      (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)
      Empty      <*> _ = Empty




{- Task 4:                          (2 Marks)
 -     Complete the following Monad Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  return a >>= f   ==  f a
 -       Right Identity: m >>= return     ==  m
 -       Associative:    (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: [x1,..,xn] >>= f == f x1 `mappend` ... `mappend` f xn
 -}
instance Monad List where
    return x = Cons x Empty
    (Cons x (xs)) >>= f = mappend (f x) (xs >>= f)
    Empty >>= _ = Empty
