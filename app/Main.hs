module Main where

import qualified Data.Map as Map
import CSP
import Variables
import Domains 
import Constraints
import Solvers
import Data.Either
import Control.Arrow (ArrowChoice(right))



-- -- Variables
-- x1 :: Expression
-- x1 = Var "x1"

-- x2 :: Expression
-- x2 = Var "x2"

-- x3 :: Expression
-- x3 = Var "x3"

-- -- Valores constantes
-- val1 :: Expression
-- val1 = Val (Left 1)  -- Número 1

-- valRojo :: Expression
-- valRojo = Val (Right "rojo")  -- Texto "rojo"

-- -- Constraints (Restricciones mezcladas)
-- constraint1 :: Constraint
-- constraint1 = expressionConstraint (RelOp EqOp ( (AddOp x1 x2)) x3)
-- -- `x3` debe ser la suma de `x1` y `x2` (para valores `Int`).

-- constraint2 :: Constraint
-- constraint2 = expressionConstraint (RelOp (/=) x1 x2)
-- -- `x1` y `x2` deben ser diferentes (puede ser `Int` o `String`).

-- constraint3 :: Constraint
-- constraint3 = expressionConstraint (RelOp (/=) x1 val1)
-- -- `x1` no puede ser `1` (valor entero).

-- constraint4 :: Constraint
-- constraint4 = expressionConstraint (RelOp (/=) x2 valRojo)
-- -- `x2` no puede ser `"rojo"` (valor string).

-- constraint5 :: Constraint
-- constraint5 = expressionConstraint (RelOp (==) (BinOp (ConcOp x1 x2) x3))
-- -- `x3` debe ser la concatenación de `x1` y `x2` si son `String`.

-- -- CSP con valores `Either Int String`
-- exampleMixedCSP :: CSP
-- -- exampleMixedCSP = CSP
-- --   { variables   = ["x1", "x2", "x3"]
-- --   , domains     = Map.fromList
-- --       [ ("x1", [Left 1, Left 2, Right "rojo", Right "azul"])
-- --       , ("x2", [Left 3, Left 4, Right "verde", Right "amarillo"])
-- --       , ("x3", [Left 4, Left 6, Right "rojoverde", Right "azulamarillo"])
-- --       ]
-- --   , constraints = [constraint2, constraint3, constraint4, constraint5]
-- --   }

exampleMixedCSP :: CSP
exampleMixedCSP = CSP
  { variables   = fromRight [] (parseVariables "x1, x2, x3")
  , domains     = fromRight Map.empty (parseDomains "x1: {1,2, \"rojo\", \"azul\"} \n x2: {3,4, \"verde\", \"amarillo\"} \n x3: {4,6, \"rojoverde\", \"azulamarillo\"} \n")
  , constraints = fromRight [] (parseConstraints "x1 + x2 - x3 == 0 \n x1 - x2 /= 0 \n x1 /= 1 \n x2 /= \"rojo\"")
  }

main :: IO ()
main = do

  -- print (bruteForceSolver exampleCSP)
  print (bruteForceSolver exampleMixedCSP)

  -- print (parseVariables  "A, B, C, o")

  -- print (parseDomain "{\"rojo \", \"azul\"  ,   \"verde\"}" )
  -- print (parseDomains "var_1: {10, 20} \n y2: {\"hola\", \"mundo\"}")

  -- print (parseRelExpr "x1 < 5")
  -- let expr = RelOp LtOp (Val (Left 3)) (Val (Left 10))
  -- print (evaluateBool Map.empty expr)
  -- print (parseRelExpr "x1 < 5" )
  -- print (parseRelExpr "x1 * (x2 + x3) == x4")
  -- print (parseRelExpr "x1 * x2 + x3 == x4")
  -- print (parseRelExpr "x1 ++ x2 == \"hello world\"")
  -- print (parseBoolExpr "x1 < 5 && x2 >= 10 || x3 /= 4")
  -- print (parseBoolExpr "x1 < 5 && (x2 >= 10 || x3 == \"hello world\")")
  -- print (parseBoolExpr "x1 + x2 * 3 < 10 || x3 == 5")
  -- print (parseBoolExpr "NOT (x1 > 5 || x2 < 3)")
  -- print (parseBoolExpr "   4 > 55   ")
  -- print (parseConstraints " 4 > 5    \n    25 > 10 \n NOT (x1 > 5 || x2 < 3) \n x1 < 5 && (x2 >= 10 || x3 == \"hello world\")")
  -- print (parseConstraints "x1 + x2 == x3 \n x1 /= x2 \n x1 /= 1 \n x2 /= \"rojo\"")
  -- print (parseConstraints "x1 + x2 - x3 == 0 \n x1 - x2 /= 0 \n x1 /= 1 \n x2 /= \"rojo\"")