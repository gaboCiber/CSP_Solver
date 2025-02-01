module CSP where

import qualified Data.Map as Map

-- Tipos básicos
type Variable = String
type Assignment a = Map.Map Variable a
type Domain a = [a]

-- Definición de expresiones
data Expression a
  = Var Variable                  -- Variable simbólica
  | Val a                         -- Valor constante
  | BinOp (a -> a -> Maybe a) (Expression a) (Expression a)  -- Operación binaria

data BoolExpression a
  = RelOp (a -> a -> Bool) (Expression a) (Expression a) -- Operador relacional
  | LogOp (Bool -> Bool -> Bool) (BoolExpression a) (BoolExpression a) -- Operador lógico genérico
  | Not (BoolExpression a)                              -- Operador lógico NOT

showOP :: p -> String
showOP op = case op of
  (==) -> "=="
  (/=) -> "/="
  (<) -> "<"
  (<=) -> "<="
  (>) -> ">"
  (>=) -> ">="
  (+) -> "+"
  (-) -> "-"
  (*) -> "*"
  (/) -> "/"
  (&&) -> "&&"
  (||) -> "||"
instance Show a => Show (Expression a) where
  show (Var v) = v
  show (Val x) = show x
  show (BinOp op e1 e2) = "( BinOp (" ++ showOP op ++ ") " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Show a => Show (BoolExpression a) where
  show (RelOp op e1 e2) = "(" ++ show e1 ++ showOP op ++ show e2 ++ ")"
  show (LogOp op e1 e2) = "(" ++ show e1 ++ showOP op ++ show e2 ++ ")"
  show (Not e) = "NOT " ++ show e

-- Restricciones basadas en expresiones
type Constraint a = Assignment a -> Bool

eitherOp :: (Int -> Int -> Int) -> (String -> String -> String) -> Either Int String -> Either Int String -> Maybe (Either Int String)
eitherOp intOp _ (Left a) (Left b) = Just (Left (intOp a b))  -- Operación con enteros
eitherOp _ strOp (Right a) (Right b) = Just (Right (strOp a b))  -- Operación con strings
eitherOp _ _ _ _ = Nothing  -- Si son tipos distintos, la operación falla

-- Evaluación de expresiones
evaluateExpr :: (Ord a) => Assignment a -> Expression a -> Maybe a
evaluateExpr _ (Val x) = Just x
evaluateExpr assignment (Var v) = Map.lookup v assignment
evaluateExpr assignment (BinOp op e1 e2) = do
  v1 <- evaluateExpr assignment e1
  v2 <- evaluateExpr assignment e2
  op v1 v2 

-- Evaluación de expresiones booleanas
evaluateBool :: (Ord a) => Map.Map Variable a -> BoolExpression a -> Maybe Bool
evaluateBool assignment (RelOp op e1 e2) = do
  v1 <- evaluateExpr assignment e1
  v2 <- evaluateExpr assignment e2
  return (op v1 v2)
evaluateBool assignment (LogOp op e1 e2) = do
  v1 <- evaluateBool assignment e1
  v2 <- evaluateBool assignment e2
  return (op v1 v2)
evaluateBool assignment (Not e) = do
  v <- evaluateBool assignment e
  return (not v)

-- Conversión de expresiones a restricciones
expressionConstraint :: (Ord a) => BoolExpression a -> Constraint a
expressionConstraint expr assignment =
  case evaluateBool assignment expr of
    Just True -> True
    _         -> False

-- Comprueba si una asignación satisface todas las restricciones
satisfiesAllConstraints :: [Constraint a] -> Assignment a -> Bool
satisfiesAllConstraints consts assignment =
  all (\constraint -> constraint assignment) consts

-- Definición del CSP
data CSP a = CSP
  { variables   :: [Variable]      -- Variables
  , domains     :: Map.Map Variable (Domain a)  -- Dominios
  , constraints :: [Constraint a]  -- Restricciones
  }