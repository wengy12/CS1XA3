module ExprType where

import           Data.List

data Expr a =     Add (Expr a) (Expr a)
                | Mult (Expr a) (Expr a)
                | Pow (Expr a) (Expr a)
                | Cos (Expr a)
                | Sin (Expr a)
                | Tan (Expr a)
                | Ln (Expr a)
                | Exp (Expr a)
                | Const a
                | Var String
                | Par (Expr a)

  deriving Eq

-- Returns a list of variables using sets
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Pow e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e1) = getVars e1
getVars (Sin e1) = getVars e1
getVars (Tan e1) = getVars e1
getVars (Ln e1) = getVars e1
getVars (Exp e1) = getVars e1
getVars (Par e1) = getVars e1
getVars (Const _)    = []
getVars (Var ident)  = [ident]
