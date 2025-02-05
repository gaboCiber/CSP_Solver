{-# LANGUAGE InstanceSigs #-}
module CSP where

import Data.List (intercalate)
import Data.Either (fromLeft, fromRight)
import qualified Data.Map as Map

-- Tipos básicos
type Variable = String
type Assignment = Map.Map Variable (Either Int String)
type Domain = [Either Int String]
type Constraint = Assignment -> Bool


-- Definición de operadores relacionales
data BoolOpType = EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
  deriving (Eq)

instance Show BoolOpType where
  show :: BoolOpType -> String
  show EqOp = "=="
  show NeqOp = "/="
  show LtOp = "<"
  show LeOp = "<="
  show GtOp = ">"
  show GeOp = ">"

-- Definición de operadores aritméticos
data BinOpType = AddOp | SubOp | MulOp | DivOp | ConcOp
  deriving (Eq)

instance Show BinOpType where
  show :: BinOpType -> String
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"
  show DivOp = "/"
  show ConcOp = "++"

-- Definición de operadores aritméticos

-- Definición de operadores unarios
data UnaryOpType = AbsOp
  deriving (Eq)

instance Show UnaryOpType where
  show :: UnaryOpType -> String
  show AbsOp = "abs"

data LogOpType = AndOp | OrOp
  deriving (Eq)

instance Show LogOpType where
  show :: LogOpType -> String
  show AndOp = "&&"
  show OrOp = "||"

-- Definición de expresiones
data Expression
  = Var Variable
  | Val (Either Int String)
  | BinOp BinOpType Expression Expression
  | UnaryOp UnaryOpType Expression 

instance Show Expression where
  show (Var v) = v
  show (Val x) = show x
  show (BinOp op e1 e2) = "( BinOp ( " ++ show op ++ " ) ( " ++ show e1 ++ " ) ( " ++ show e2 ++ " ) "
  show (UnaryOp op e) = "( UnaryOp ( " ++ show op ++ " ) ( " ++ show e ++ " )"

data BoolExpression
  = RelOp BoolOpType Expression Expression
  | LogOp LogOpType BoolExpression BoolExpression
  | Not BoolExpression

instance Show BoolExpression where
  show (RelOp op e1 e2) = "( RelOp ( " ++ show op ++ " ) ( " ++ show e1 ++ " ) ( " ++ show e2 ++ " ) "
  show (LogOp op e1 e2) = "( LogOp ( " ++ show op ++ " ) ( " ++ show e1 ++ " ) ( " ++ show e2 ++ " ) "
  show (Not e) = "NOT " ++ show e

-- Aplica operadores aritméticos `BinOpType` sobre `Either Int String`
applyUnaryOp :: UnaryOpType -> Either Int String -> Maybe (Either Int String)
applyUnaryOp AbsOp (Left a) = Just (Left (abs a))  -- abs()` solo aplica a números
applyUnaryOp _ _ = Nothing  -- No aplica a Strings


applyBinOp :: BinOpType -> Either Int String -> Either Int String -> Maybe (Either Int String)
applyBinOp AddOp (Left a) (Left b) = Just (Left (a + b))
applyBinOp SubOp (Left a) (Left b) = Just (Left (a - b)) 
applyBinOp MulOp (Left a) (Left b) = Just (Left (a * b))
applyBinOp DivOp (Left a) (Left b)
  | b /= 0    = Just (Left (a `div` b))  -- División válida si `b ≠ 0`
  | otherwise = Nothing  -- Evita división por 0
applyBinOp ConcOp (Right a) (Right b) = Just (Right (a ++ b))  -- Concatenación de Strings
applyBinOp _ _ _ = Nothing  -- Otros casos inválidos

-- Aplica operadores relacionales `BoolOpType` sobre `Either Int String`
applyBoolOpType :: BoolOpType -> Either Int String -> Either Int String -> Bool
applyBoolOpType EqOp  = (==)
applyBoolOpType NeqOp = (/=)
applyBoolOpType LtOp  = (<)
applyBoolOpType LeOp  = (<=)
applyBoolOpType GtOp  = (>)
applyBoolOpType GeOp  = (>=)

applyLogOpType:: LogOpType -> Bool -> Bool -> Bool
applyLogOpType AndOp = (&&)
applyLogOpType OrOp = (||)

-- Evaluación de expresiones
evaluateExpr :: Assignment -> Expression -> Maybe (Either Int String)
evaluateExpr _ (Val x) = Just x
evaluateExpr assignment (Var v) = Map.lookup v assignment
evaluateExpr assignment (BinOp op e1 e2) = do
  v1 <- evaluateExpr assignment e1
  v2 <- evaluateExpr assignment e2
  applyBinOp op v1 v2  -- Aplica `applyBinOp` correctamente
evaluateExpr assignment (UnaryOp op e) = do
  v <- evaluateExpr assignment e
  applyUnaryOp op v 

-- Evaluación de expresiones booleanas
evaluateBool :: Assignment -> BoolExpression -> Maybe Bool
evaluateBool assignment (RelOp op e1 e2) = do
  v1 <- evaluateExpr assignment e1
  v2 <- evaluateExpr assignment e2
  return (applyBoolOpType op v1 v2)  -- Aplica `BoolOpType`
evaluateBool assignment (LogOp op e1 e2) = do
  v1 <- evaluateBool assignment e1
  v2 <- evaluateBool assignment e2
  return (applyLogOpType op v1 v2)
evaluateBool assignment (Not e) = do
  v <- evaluateBool assignment e
  return (not v)

-- Conversión de expresiones a restricciones
expressionConstraint :: BoolExpression -> Constraint
expressionConstraint expr assignment =
  case evaluateBool assignment expr of
    Just True -> True
    _         -> False

satisfiesAllConstraints :: [BoolExpression] -> Assignment -> Bool
satisfiesAllConstraints consts assignment =
  all (`expressionConstraint` assignment) consts

-- Función para convertir un valor Either a String
eitherToString :: Either Int String -> String
eitherToString (Left i) = show i
eitherToString (Right s) = show s

-- Función para convertir una asignación a JSON
assignmentToJson :: Assignment -> String
assignmentToJson assignment =
  " { " ++ intercalate ", " (map toJsonPair (Map.toList assignment)) ++ " } "
  where
    toJsonPair (var, val) = var ++ ": " ++ eitherToString val

-- Función para imprimir una lista de asignaciones en formato JSON con saltos de línea
printAssignmentsResults :: [Assignment] -> String
printAssignmentsResults assignments =
  "[" ++ intercalate ",\n" (map assignmentToJson assignments) ++ "]\n"

-- Definición del CSP
data CSP = CSP
  { variables   :: [Variable]      -- Variables
  , domains     :: Map.Map Variable Domain  -- Dominios
  , constraints :: [BoolExpression]  -- Restricciones
  }
