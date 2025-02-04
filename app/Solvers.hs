module Solvers where

import CSP
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Genera todas las combinaciones posibles de asignaciones
generateAssignments :: [Variable] -> Map.Map Variable (Domain) -> [Assignment]
generateAssignments [] _ = [Map.empty]
generateAssignments (v:vs) doms =
  case Map.lookup v doms of
    Nothing -> []
    Just domain ->
      [ Map.insert v value assignment
      | value <- domain
      , assignment <- generateAssignments vs doms
      ]

-------------------------------
-- Algoritmo de Fuerza Bruta --
-------------------------------

bruteForceSolver :: CSP -> [Assignment]
bruteForceSolver (CSP vars doms consts) =
  filter (satisfiesAllConstraints consts) (generateAssignments vars doms)

-------------------------------
-- Algoritmo de Backtracking --
-------------------------------

backtrackingSolver :: CSP -> [Assignment]
backtrackingSolver (CSP vars doms consts) = backtrack Map.empty vars
  where
    backtrack :: Assignment -> [Variable] -> [Assignment]
    backtrack assignment [] = [assignment]
    backtrack assignment (v:vs) =
      [ result
      | value <- Map.findWithDefault [] v doms
      , let newAssignment = Map.insert v value assignment
      , satisfiesAllConstraints consts newAssignment
      , result <- backtrack newAssignment vs
      ]

--------------------------------
-- Algoritmo Forward Checking --
--------------------------------

forwardCheckingSolver :: CSP -> [Assignment]
forwardCheckingSolver (CSP vars doms consts) = fcSearch Map.empty vars doms
  where
    fcSearch :: Assignment -> [Variable] -> Map.Map Variable Domain -> [Assignment]
    fcSearch assignment [] _ = [assignment]
    fcSearch assignment (v:vs) doms =
      [ result
      | value <- Map.findWithDefault [] v doms
      , let newAssignment = Map.insert v value assignment
      , let newDoms = forwardCheck consts newAssignment vs doms
      , all (not . null) (Map.elems newDoms)
      , result <- fcSearch newAssignment vs newDoms
      ]

    forwardCheck :: [BoolExpression] -> Assignment -> [Variable] -> Map.Map Variable Domain -> Map.Map Variable Domain
    forwardCheck consts assignment vs doms = foldr (reduceDomain consts assignment) doms vs

--------------------------------------
-- Algoritmo Arc Consistency (AC-3) --
--------------------------------------

arcConsistencySolver :: CSP -> [Assignment]
arcConsistencySolver (CSP vars doms consts) = acSearch Map.empty vars (makeArcConsistent consts Map.empty vars doms)
  where
    acSearch :: Assignment -> [Variable] -> Map.Map Variable Domain -> [Assignment]
    acSearch assignment [] _ = [assignment]
    acSearch assignment (v:vs) doms =
      [ result
      | value <- Map.findWithDefault [] v doms
      , let newAssignment = Map.insert v value assignment
      , let newDoms = makeArcConsistent consts newAssignment vs doms
      , all (not . null) (Map.elems newDoms)
      , result <- acSearch newAssignment vs newDoms
      ]

    makeArcConsistent :: [BoolExpression] -> Assignment -> [Variable] -> Map.Map Variable Domain -> Map.Map Variable Domain
    makeArcConsistent consts assignment vs doms = foldr (reduceDomain consts assignment) doms vs

-----------------------------
-- Algoritmo Min-Conflicts --
-----------------------------

minConflictsSolver :: CSP -> [Assignment]
minConflictsSolver csp@(CSP vars doms consts) = 
  case minConflicts (initialAssignment vars doms) 1000 of  -- 1000 pasos máximos
    Just assignment -> [assignment]
    Nothing -> []
  where
    minConflicts :: Assignment -> Int -> Maybe Assignment
    minConflicts assignment 0 = Nothing
    minConflicts assignment steps
      | satisfiesAllConstraints consts assignment = Just assignment
      | otherwise = 
          let conflictedVars = filter (\v -> conflicts assignment v consts > 0) vars
              v = head conflictedVars
              newValue = minimumBy (comparing (\value -> conflicts (Map.insert v value assignment) v consts)) 
                                  (Map.findWithDefault [] v doms)
              newAssignment = Map.insert v newValue assignment
          in minConflicts newAssignment (steps - 1)

    conflicts :: Assignment -> Variable -> [BoolExpression] -> Int
    conflicts assignment v consts = length $ filter (\c -> not (expressionConstraint c assignment)) consts

    initialAssignment :: [Variable] -> Map.Map Variable Domain -> Assignment
    initialAssignment vars doms = Map.fromList [(v, head (Map.findWithDefault [] v doms)) | v <- vars]

-- Función auxiliar para reducir dominios
reduceDomain :: [BoolExpression] -> Assignment -> Variable -> Map.Map Variable Domain -> Map.Map Variable Domain
reduceDomain consts assignment v doms = 
  Map.adjust (filter (\value -> satisfiesAllConstraints consts (Map.insert v value assignment))) v doms