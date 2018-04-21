{-|
Module : ExprParser
Description : Contains a type class and instances for
differentiable expressions
-}

module ExprParser (parseExprD,parseExprF) where

import ExprType
import ExprPretty
import Text.Parsec
import Text.Parsec.String

-- | Parses a string into an Expr Double type
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Parses a string into an Expr Float type
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | The actual parser
exprD :: Parser (Expr Double)
exprD =  exprFuncD <|> exprVar <|> exprConstD <|> exprOpD <|> exprParD

-- | The actual parser
exprVar :: Parser (Expr a) -- Abstracted and available for use whether for float values or double
exprVar = do {
               symbol "Var";
               ss <- many1 letter;
               return (Var ss);
             }


exprConstD :: Parser (Expr Double)
exprConstD = do {
                  symbol "Const";
                  ss <- double;
                  return (Const ss);
                }

exprFuncD :: Parser (Expr Double)
exprFuncD = do {
               s <- symbol "Cos" <|> symbol "Sin" <|> symbol "Tan" <|> symbol "Ln" <|> symbol "Exp";
               ss <- exprD;
               if s == "Cos" then
                 return (Cos ss);
               else if s == "Sin" then
                 return (Sin ss);
               else if s == "Tan" then
                 return (Tan ss)
               else if s == "Ln" then
                 return (Ln ss)
               else if s == "Exp" then
                 return (Exp ss)
               else
                 error "Check your grammar!"
             }

exprOpD :: Parser (Expr Double)
exprOpD = do {
               s <- symbol "Add" <|> symbol "Mult" <|> symbol "Pow";
               ss <- between (symbol "(") (symbol ")") exprD;
               ss' <- between (symbol "(") (symbol ")") exprD;
               if s == "Add" then
                 return (Add ss ss');
               else if s == "Mult" then
                 return (Mult ss ss');
               else if s == "Pow" then
                 return (Pow ss ss');
               else
                 error "Check your Grammar!"
             }

exprParD :: Parser (Expr Double)
exprParD = ( between (symbol "(") (symbol ")") exprD )

---------------------------------------------------------------------

exprF :: Parser (Expr Float)
exprF = exprFuncF <|> exprVar <|> exprConstF <|> exprOpF <|> exprParF

exprConstF :: Parser (Expr Float)
exprConstF = do {
                  symbol "Const";
                  ss <- float;
                  return (Const ss);
                }

exprFuncF :: Parser (Expr Float)
exprFuncF = do {
               s <- symbol "Cos" <|> symbol "Sin" <|> symbol "Tan" <|> symbol "Ln" <|> symbol "Exp";
               ss <- exprF;
               if s == "Cos" then
                 return (Cos ss);
               else if s == "Sin" then
                 return (Sin ss);
               else if s == "Tan" then
                 return (Tan ss)
               else if s == "Ln" then
                 return (Ln ss)
               else if s == "Exp" then
                 return (Exp ss)
               else
                 error "Check your grammar!"
             }

exprOpF :: Parser (Expr Float)
exprOpF = do {
               s <- symbol "Add" <|> symbol "Mult" <|> symbol "Pow";
               ss <- between (symbol "(") (symbol ")") (exprF);
               ss' <- between (symbol "(") (symbol ")") (exprF);
               if s == "Add" then
                 return (Add ss ss');
               else if s == "Mult" then
                 return (Mult ss ss');
               else if s == "Pow" then
                 return (Pow ss ss');
               else
                 error "Check your Grammar!"
             }

exprParF :: Parser (Expr Float)
exprParF = ( between (symbol "(") (symbol ")") exprF )

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do {spaces;
                ss' <- string ss;
                spaces;
                return ss'
              }
  in try symbol'

digits :: Parser String
digits = many1 digit

integer :: Parser Integer
integer = fmap read (digits)

decimalDigits :: Parser String
decimalDigits = do { d <- char '.';
                     rm <- digits;
                     return  (d:rm)
                   }

decimalDigits' :: Parser String
decimalDigits' = do { ds <- digits;
                      rs <- try decimalDigits <|> return "";
                      return (ds ++ rs)
                    }

double :: Parser Double
double = fmap read $ decimalDigits'

float :: Parser Float
float = fmap read $ decimalDigits'
