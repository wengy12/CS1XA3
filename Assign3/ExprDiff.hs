{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
-}

module ExprDiff where

import           ExprType
import           Data.List
import           Data.Fixed
import           Data.Maybe

import qualified Data.Map.Strict as Map

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) (Add e1 e2)

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) (Mult e1 e2)

  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) (Pow e1 e2)

  cos :: Expr a -> Expr a
  cos e1 = simplify (Map.fromList []) (Cos e1)

  sin :: Expr a -> Expr a
  sin e1 = simplify (Map.fromList []) (Sin e1)

  tg :: Expr a -> Expr a
  tg e1 = simplify (Map.fromList []) (Tan e1)

  ln :: Expr a -> Expr a
  ln e1 = simplify (Map.fromList []) (Ln e1)

  e :: Expr a -> Expr a
  e e1 = simplify (Map.fromList []) (Exp e1)

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x

instance (Num a, Eq a, Ord a, Floating a) => DiffExpr a where
-----------------eval---------------------
  -- ^ Just constant
  eval vrs (Const x) = x
  -- ^ Recursive Add, Mult and Power
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  -- ^ This functions are built-in
  eval vrs (Sin x) = Prelude.sin(eval vrs x)
  eval vrs (Cos x) = Prelude.cos(eval vrs x)
  eval vrs (Tan x) = Prelude.tan(eval vrs x)
  eval vrs (Exp x) = Prelude.exp(eval vrs x)
  eval vrs (Ln x) = if eval vrs x > 0 then
                        Prelude.log(eval vrs x)
                     else error "Damain error"
  eval vrs (Pow e1 e2) = eval vrs e1 ** eval vrs e2
  -- ^ Substitution
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "The given variables are not in the expression!"

--------------simplify---------------------------
  -- ^ Just const
  simplify vrs (Const x) = (Const x)

  -- ^ Substitute what we have
  simplify vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> (Const v)
                       Nothing -> (Var x)

  -- ^ Simplify addition
  simplify vrs (Add a b) =
    case (simplify vrs a, simplify vrs b) of
      -- ^ killing zeros
      (Const 0, x) -> simplify vrs x
      (x, Const 0) -> simplify vrs x

      -- ^ add two numbers
      (Const x, Const y) -> Const (x+y)

      -- ^ move all const to the right
      (Const x, y) -> (Add (simplify vrs y) (Const x))

      -- ^ adds all consts together
      (Add z (Const y), Const x) -> simplify vrs (Add (simplify vrs z) (Const (x+y)))

      -- ^ case we dont have any consts
      (x, y) -> (Add x y)


  -- Simplify moltiplications
  simplify vrs (Mult a b) =
    case (simplify vrs a, simplify vrs b) of
      -- ^ multipy by zero
      (Const 0, y) -> (Const 0)
      (y, Const 0) -> (Const 0)

      -- ^ multipy by 1
      (Const 1, y) -> (simplify vrs y)
      (y, Const 1) -> (simplify vrs y)

      -- ^ multipy two numbers
      (Const x, Const y) -> Const (x*y)

      -- ^ get numbers to the right
      (Const x, y) -> (Mult (simplify vrs y) (Const x))

      -- ^ multiply the numbers
      (Mult z (Const y), Const x) -> simplify vrs (Mult (simplify vrs z) (Const (x*y)))

      -- ^ if we left no constants
      (x, y) -> (Mult x y)

----------------------functions-------------------------

  --------------cos----------------
  simplify vrs (Cos (Const a)) = (Const (Prelude.cos(a)))
  simplify vrs (Cos (Var a)) = (Cos (Var a))
  simplify vrs (Cos a) = (Cos (simplify vrs a))

  --------------sin----------------
  simplify vrs (Sin (Const a)) = (Const (Prelude.sin(a)))
  simplify vrs (Sin (Var a)) = (Sin (Var a))
  simplify vrs (Sin a) = (Sin (simplify vrs a))

  --------------tan----------------

  simplify vrs (Tan (Const a)) = (Const (Prelude.tan(a)))
  simplify vrs (Tan (Var a)) = (Tan (Var a))
  simplify vrs (Tan a) = (Tan (simplify vrs a))

  --------------ln-----------------
  simplify vrs (Ln (Const a)) = if a > 0 then (Const (Prelude.log(a))) else error "Domain error"
  simplify vrs (Ln (Var a)) = (Ln (Var a))
  simplify vrs (Ln (Exp a)) = simplify vrs (a)
  simplify vrs (Ln a) = (Ln (simplify vrs a))

  --------------exp----------------
  simplify vrs (Exp (Const a)) = (Const (exp(a)))
  simplify vrs (Exp (Var a)) = (Exp (Var a))
  simplify vrs (Exp (Ln a)) = simplify vrs (a)
  simplify vrs (Exp a) = (Exp (simplify vrs a))

  --------------parens----------------
  simplify vrs (Par a) = simplify vrs a

  --------------power----------------
  simplify vrs (Pow a (Const 0)) = (Const 1)
  simplify vrs (Pow a (Const 1)) = simplify (Map.fromList []) (simplify vrs a)
  simplify vrs (Pow a (Const k)) = (Pow a (Const k))

  -----------partial diff------------
  -- ^ consts goet to zero
  partDiff x (Const a) = (Const 0)

  -- ^ all other vars are just consts
  partDiff x (Var a) = if x == a then
                          (Const 1)
                          else
                          (Const 0)

  -- ^ normal addition
  partDiff x (Add a b) = (partDiff x a) !+ (partDiff x b)

  -- ^ Power
  partDiff x (Pow (Var a) (Const b)) = if x == a then
                                          (Mult (Const b) (Pow (Var a) (Const (b-1))))
                                       else
                                          (Const 0)
  -- ^ Product rule
  partDiff x (Mult a b) = simplify (Map.fromList []) (((partDiff x a) !* (simplify (Map.fromList []) (b))) !+ ((simplify (Map.fromList []) (a)) !* (partDiff x b)))

  -- ^ Diff of other funcs in simple case, dont have division so cant find part diff for ln
  partDiff x (Cos (Var a)) = if x == a then
                                (Const (-1)) !* (Sin (Var a))
                             else
                                (Const 0)
  partDiff x (Sin (Var a)) = if x == a then
                                (Cos (Var a))
                             else
                                (Const 0)
  partDiff x (Exp (Var a)) = if x == a then
                                (Exp (Var a))
                             else
                                (Const 0)

  -- ^ Chain rule
  partDiff x (Cos a) = (Const (-1)) !* (Sin (simplify (Map.fromList []) (a))) !* (partDiff x a)
  partDiff x (Sin a) = ((Cos (simplify (Map.fromList []) (a))) !* (partDiff x a))
  partDiff x (Exp a) = ((Exp (simplify (Map.fromList []) (a))) !* (partDiff x a))

  -- ^ Simplify first
  partDiff x y = partDiff x (simplify (Map.fromList []) (y))
