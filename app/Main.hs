module Main where

import qualified Data.Map as Map
import CSP 
import qualified Solvers

-- Variables
x1, x2, x3 :: Expression Int
x1 = Var "x1"
x2 = Var "x2"
x3 = Var "x3"

-- Restricciones
-- 1. x1 + x2 > x3
constraint1 :: Constraint Int
constraint1 = expressionConstraint (RelOp (==) (BinOp (+) x1 x2) x3)

-- 2. x1 != x2
constraint2 :: Constraint Int
constraint2 = expressionConstraint (RelOp (/=) x1 x2)

-- CSP
exampleCSP :: CSP Int
exampleCSP = CSP
  { variables   = ["x1", "x2", "x3"]
  , domains     = Map.fromList [("x1", [1, 2, 3]), ("x2", [1, 2, 3]), ("x3", [1, 2, 3])]
  , constraints = [constraint1, constraint2]
  }

main :: IO ()
main = do
  print ( Solvers.bruteForceSolver exampleCSP)
