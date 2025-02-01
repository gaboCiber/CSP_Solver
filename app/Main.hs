module Main where

import qualified Data.Map as Map
import CSP
--import qualified Solvers
import Variables
import Domains 
import Text.Megaparsec.Char
import Solvers (bruteForceSolver)
import Variables (getVariables)
import Data.Either
import CSP (eitherOp)
import Data.Either (fromRight)

----------------------------------------------------------------------------------------
-- -- Variables
-- x1, x2, x3 :: Expression Int -- (Either Int String)
-- x1 = Var "x1"
-- x2 = Var "x2"
-- x3 = Var "x3"

-- -- Restricciones
-- constraint1 :: Constraint Int --(Either Int String)
-- constraint1 = expressionConstraint (RelOp (==) (BinOp (+) x1 x2) x3)

-- constraint2 :: Constraint Int --(Either Int String)
-- constraint2 = expressionConstraint (RelOp (/=) x1 x2)

-- constraint3 :: Constraint Int --(Either Int String)
-- constraint3 = expressionConstraint (RelOp (/=) x1 (Val 1))
-- -- CSP
-- exampleCSP :: CSP Int --(Either Int String)
-- exampleCSP = CSP
--   { variables   = ["x1", "x2", "x3"]
--   , domains     = Map.fromList [("x1", [1, 2, 3]), ("x2", [1, 2, 3]), ("x3", [1, 2, 3])]
--   , constraints = [constraint1, constraint2, constraint3]
--   }

------------------------------------------------------------------------------------------------

-- exampleCSP2 :: CSP (Either Int String)
-- exampleCSP2 = CSP
--   { variables   = getVariables (parseVariables "x1, x2, x3")
--   , domains     = getDomains (parseDomains "x1 : {1,2,3} \n x2 : {1,2,3} \n x3 : {1,2,3}" ) --Map.fromList [("x1", [1, 2, 3]), ("x2", [1, 2, 3]), ("x3", [1, 2, 3])]
--   , constraints = [constraint1, constraint2]
--   }

-- exampleCSP3 = CSP
--   { variables   = ["x1", "x2", "x3"]
--   , domains     = Map.fromList [("x1", ["hola", "pan"]), ("x2", ["hola", "pan"]), ("x3", ["hola", "pan"])]
--   , constraints = [constraint1, constraint2]
--   }

-------------------------------------------------------------------------------------------------

-- -- Variables
-- x1 :: Expression String
-- x1 = Var "x1"

-- x2 :: Expression String
-- x2 = Var "x2"

-- x3 :: Expression String
-- x3 = Var "x3"

-- -- Constraints (Restricciones de tipo String)
-- constraint1 :: Constraint String
-- constraint1 = expressionConstraint (RelOp (==) (BinOp (++) x1 x2) x3)  
-- -- Esto impone que `x3` debe ser la concatenación de `x1` y `x2`.

-- constraint2 :: Constraint String
-- constraint2 = expressionConstraint (RelOp (/=) x1 x2)
-- -- Esto impone que `x1` y `x2` deben ser diferentes.

-- constraint3 :: Constraint String
-- constraint3 = expressionConstraint (RelOp (/=) x1 (Val "rojo"))
-- -- Esto impone que `x1` no puede ser `"rojo"`.

-- -- CSP con valores String
-- exampleCSP :: CSP String
-- exampleCSP = CSP
--   { variables   = ["x1", "x2", "x3"]
--   , domains     = Map.fromList [("x1", ["rojo", "azul"]), ("x2", ["verde", "amarillo"]), ("x3", ["rojoazul", "verdeamarillo", "azulverde"])]
--   , constraints = [constraint1, constraint2, constraint3]
--   }

-------------------------------------------------------------------------------------------------

-- Variables
x1 :: Expression (Either Int String)
x1 = Var "x1"

x2 :: Expression (Either Int String)
x2 = Var "x2"

x3 :: Expression (Either Int String)
x3 = Var "x3"

-- Valores constantes
val1 :: Expression (Either Int String)
val1 = Val (Left 1)  -- Número 1

valRojo :: Expression (Either Int String)
valRojo = Val (Right "rojo")  -- Texto "rojo"

-- Constraints (Restricciones mezcladas)
constraint1 :: Constraint (Either Int String)
constraint1 = expressionConstraint (RelOp (==) (BinOp (eitherOp (+) (\_ _ -> "")) x1 x2) x3)
-- `x3` debe ser la suma de `x1` y `x2` (para valores `Int`).

constraint2 :: Constraint (Either Int String)
constraint2 = expressionConstraint (RelOp (/=) x1 x2)
-- `x1` y `x2` deben ser diferentes (puede ser `Int` o `String`).

constraint3 :: Constraint (Either Int String)
constraint3 = expressionConstraint (RelOp (/=) x1 val1)
-- `x1` no puede ser `1` (valor entero).

constraint4 :: Constraint (Either Int String)
constraint4 = expressionConstraint (RelOp (/=) x2 valRojo)
-- `x2` no puede ser `"rojo"` (valor string).

constraint5 :: Constraint (Either Int String)
constraint5 = expressionConstraint (RelOp (==) (BinOp (eitherOp (\_ _ -> 0) (++)) x1 x2) x3)
-- `x3` debe ser la concatenación de `x1` y `x2` si son `String`.

-- CSP con valores `Either Int String`
exampleMixedCSP :: CSP (Either Int String)
-- exampleMixedCSP = CSP
--   { variables   = ["x1", "x2", "x3"]
--   , domains     = Map.fromList
--       [ ("x1", [Left 1, Left 2, Right "rojo", Right "azul"])
--       , ("x2", [Left 3, Left 4, Right "verde", Right "amarillo"])
--       , ("x3", [Left 4, Left 6, Right "rojoverde", Right "azulamarillo"])
--       ]
--   , constraints = [constraint2, constraint3, constraint4, constraint5]
--   }

exampleMixedCSP = CSP
  { variables   = getVariables (parseVariables "x1, x2, x3")
  , domains     = fromRight Map.empty (parseDomains "x1: {1,2, \"rojo\", \"azul\"} \n x2: {3,4, \"verde\", \"amarillo\"} \n x3: {4,6, \"rojoverde\", \"azulamarillo\"} \n")
  , constraints = [constraint1, constraint2, constraint3, constraint4]
  }
main :: IO ()
main = do
  --print (parseVariables  "A, B, C, o")
  --print (parseDomain "{\"rojo \", \"azul\"  ,   \"verde\"}" )
  --print (parseDomains "var_1: {10, 20} \n y2: {\"hola\", \"mundo\"}")
  --print (bruteForceSolver exampleCSP)
  print (bruteForceSolver exampleMixedCSP)





