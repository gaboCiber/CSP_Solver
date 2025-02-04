module Solvers where

import CSP
import qualified Data.Map as Map

-- Genera todas las combinaciones posibles de asignaciones
generateAssignments :: [Variable] -> Map.Map Variable (Domain) -> [Assignment]
generateAssignments [] _ = [Map.empty]
generateAssignments (v:vs) doms =
  case Map.lookup v doms of
    Nothing -> []  -- Si no hay dominio para la variable, no se generan asignaciones
    Just domain ->
      [ Map.insert v value assignment
      | value <- domain
      , assignment <- generateAssignments vs doms
      ]

-- Resuelve un CSP mediante fuerza bruta
bruteForceSolver :: CSP -> [Assignment]
bruteForceSolver (CSP vars doms consts) =
  filter (satisfiesAllConstraints consts) (generateAssignments vars doms)

backtrackingSolver :: CSP -> [Assignment]
backtrackingSolver (CSP vars doms consts) = backtrack Map.empty vars
  where
    backtrack :: Assignment -> [Variable] -> [Assignment]
    backtrack assignment [] = [assignment]  -- Si no hay más variables, devuelve la asignación actual
    backtrack assignment (v:vs) =
      [ result
      | value <- Map.findWithDefault [] v doms  -- Obtiene los valores del dominio de la variable actual
      , let newAssignment = Map.insert v value assignment  -- Asigna el valor a la variable
      , satisfiesAllConstraints consts newAssignment  -- Verifica si la asignación es válida
      , result <- backtrack newAssignment vs  -- Continúa con las siguientes variables
      ]
