module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- This is how things are displayed. It makes things easier to read
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Pow e1 e2)= parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Sin e1)     = parens $ "sin " ++ show e1
  show (Cos e1)     = parens $ "cos " ++ show e1
  show (Tan e1)     = parens $ "tg " ++ show e1
  show (Ln e1)      = parens $ "ln " ++ show e1
  show (Exp e1)     = parens $ "e^" ++ show e1
  show (Par e1)     = parens (show e1)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
