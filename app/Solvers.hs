module Solvers where

import CSP
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing, fromJust)

-------------------------------
-- Algoritmo de Fuerza Bruta --
-------------------------------

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

bruteForceSolver :: CSP -> [Assignment]
bruteForceSolver (CSP vars doms consts) =
  filter (satisfiesAllConstraints consts) (generateAssignments vars doms)

-------------------------------
-- Algoritmo de Backtracking --
-------------------------------

-- **Backtracking Solver**
backtrackingSolver :: CSP -> [Assignment]
backtrackingSolver csp = backtrack csp Map.empty

backtrack :: CSP -> Assignment -> [Assignment]
backtrack csp assignment
  --  Si todas las variables están asignadas, verificar restricciones antes de aceptar la solución
  | length assignment == length (variables csp) =
      [assignment | satisfiesAllConstraints (constraints csp) assignment] 

  -- Buscar la siguiente variable no asignada
  | otherwise =
      case nextUnassignedVariable csp assignment of
        Nothing -> []  -- No hay más variables disponibles
        Just var ->
          case Map.lookup var (domains csp) of
            Nothing -> []  -- No hay dominio para la variable
            Just domain -> concatMap (tryValue csp assignment var) domain

tryValue :: CSP -> Assignment -> Variable -> Either Int String -> [Assignment]
tryValue csp assignment var value =
  let newAssignment = Map.insert var value assignment
  in backtrack csp newAssignment  -- Continuar con el backtracking

-- **Selecciona la siguiente variable sin asignar**
nextUnassignedVariable :: CSP -> Assignment -> Maybe Variable
nextUnassignedVariable csp assignment = 
  case filter (`Map.notMember` assignment) (variables csp) of
    []    -> Nothing
    (v:_) -> Just v


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


-- forwardCheckingSolver :: CSP -> [Assignment]
-- forwardCheckingSolver csp = backtrackFC csp Map.empty (domains csp)

-- -- **Backtracking con Forward Checking**
-- backtrackFC :: CSP -> Assignment -> Map.Map Variable Domain -> [Assignment]
-- backtrackFC csp assignment currentDomains
--   | length assignment == length (variables csp) =
--       if satisfiesAllConstraints (constraints csp) assignment
--       then [assignment]  -- ✅ Solución encontrada
--       else []
--   | otherwise =
--       case nextUnassignedVariable csp assignment of
--         Nothing -> []
--         Just var ->
--           case Map.lookup var currentDomains of
--             Nothing -> []
--             Just domain -> concatMap (tryValueFC csp assignment currentDomains var) domain

-- -- **Intentar asignar un valor con Forward Checking**
-- tryValueFC :: CSP -> Assignment -> Map.Map Variable Domain -> Variable -> Either Int String -> [Assignment]
-- tryValueFC csp assignment currentDomains var value =
--   let newAssignment = Map.insert var value assignment
--       reducedDomains = applyForwardChecking csp newAssignment currentDomains var value
--   in if isNothing reducedDomains  -- ❌ Si Forward Checking vacía algún dominio, hacer backtrack
--      then []
--      else backtrackFC csp newAssignment (fromJust reducedDomains)

-- -- **Aplicar Forward Checking**
-- applyForwardChecking :: CSP -> Assignment -> Map.Map Variable Domain -> Variable -> Either Int String -> Maybe (Map.Map Variable Domain)
-- applyForwardChecking csp assignment domains var value =
--   let updatedDomains = foldl (pruneDomain csp assignment var value) domains (variables csp)
--   in if any null (Map.elems updatedDomains)  -- ❌ Si algún dominio queda vacío, hacer backtrack
--      then Nothing
--      else Just updatedDomains

-- -- **Eliminar valores inválidos de los dominios no asignados**
-- pruneDomain :: CSP -> Assignment -> Variable -> Either Int String -> Map.Map Variable Domain -> Variable -> Map.Map Variable Domain
-- pruneDomain csp assignment var _ dom otherVar
--   | otherVar == var = dom  -- ❌ No modificar la variable ya asignada
--   | Map.member otherVar assignment = dom  -- ❌ No modificar variables ya asignadas
--   | otherwise =
--       let filteredDomain = filter (\val -> isConsistent csp assignment var otherVar val) 
--                               (Map.findWithDefault [] otherVar dom)
--       in Map.insert otherVar filteredDomain dom

-- -- **Verificar si la asignación es consistente con las restricciones locales**
-- isConsistent :: CSP -> Assignment -> Variable -> Variable -> Either Int String -> Bool
-- isConsistent csp assignment assignedVar checkingVar value =
--   let tempAssignment = Map.insert checkingVar value assignment
--   in all (\constraint -> expressionConstraint constraint tempAssignment) (relevantConstraints csp assignedVar checkingVar)

-- -- **Obtener restricciones relevantes**
-- relevantConstraints :: CSP -> Variable -> Variable -> [BoolExpression]
-- relevantConstraints csp assignedVar checkingVar =
--   filter (\constraint -> assignedVar `elem` getVariablesInConstraint constraint
--                       && checkingVar `elem` getVariablesInConstraint constraint)
--          (constraints csp)

-- -- **Extraer variables que aparecen en una restricción**
-- getVariablesInConstraint :: BoolExpression -> [Variable]
-- getVariablesInConstraint (RelOp _ e1 e2) = getVariablesInExpr e1 ++ getVariablesInExpr e2
-- getVariablesInConstraint (LogOp _ b1 b2) = getVariablesInConstraint b1 ++ getVariablesInConstraint b2
-- getVariablesInConstraint (Not b) = getVariablesInConstraint b

-- getVariablesInExpr :: Expression -> [Variable]
-- getVariablesInExpr (Var v) = [v]
-- getVariablesInExpr (Val _) = []
-- getVariablesInExpr (BinOp _ e1 e2) = getVariablesInExpr e1 ++ getVariablesInExpr e2


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