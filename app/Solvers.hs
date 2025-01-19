module Solvers where

import CSP
import qualified Data.Map as Map


-- Genera todas las combinaciones posibles de asignaciones
generateAssignments :: [Variable] -> Map.Map Variable (Domain a) -> [Assignment a]
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
bruteForceSolver :: (Ord a) => CSP a -> [Assignment a]
bruteForceSolver (CSP vars doms consts) =
  filter (satisfiesAllConstraints consts) (generateAssignments vars doms)
