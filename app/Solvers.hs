{-# LANGUAGE ScopedTypeVariables #-}
module Solvers where

import CSP
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing, fromJust, fromMaybe)

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

-- Forward Checking Solver que usa backtracking con forward checking
forwardCheckingSolver :: CSP -> [Assignment]
forwardCheckingSolver csp = forwardCheckBacktrack csp Map.empty (domains csp)

-- Backtracking con Forward Checking
forwardCheckBacktrack :: CSP -> Assignment -> Map.Map Variable Domain -> [Assignment]
forwardCheckBacktrack csp assignment currentDomains
  -- Si todas las variables están asignadas, se evalúan todas las restricciones
  | length assignment == length (variables csp) =
      if satisfiesAllConstraints (constraints csp) assignment
      then [assignment]
      else []
  | otherwise =
      case nextUnassignedVariable csp assignment of
        Nothing -> []
        Just var ->
          case Map.lookup var currentDomains of
            Nothing -> []
            Just domain ->
              concatMap (tryAssignValue csp assignment currentDomains var) domain

-- Intenta asignar un valor a una variable y aplica forward checking
tryAssignValue :: CSP -> Assignment -> Map.Map Variable Domain -> Variable -> Either Int String -> [Assignment]
tryAssignValue csp assignment currentDomains var value =
  let newAssignment = Map.insert var value assignment
      newDomains    = applyForwardChecking csp newAssignment currentDomains var value
  in case newDomains of
       Nothing -> []  -- Si algún dominio queda vacío, se hace backtracking
       Just reducedDomains -> forwardCheckBacktrack csp newAssignment reducedDomains

-- Aplica forward checking: actualiza los dominios de las variables aún no asignadas
applyForwardChecking :: CSP -> Assignment -> Map.Map Variable Domain -> Variable -> Either Int String -> Maybe (Map.Map Variable Domain)
applyForwardChecking csp assignment domains var value =
  let updatedDomains = foldl (pruneDomain csp assignment var value) domains (variables csp)
  in if any null (Map.elems updatedDomains)
       then Nothing
       else Just updatedDomains

-- Elimina de los dominios de variables no asignadas aquellos valores que no sean consistentes
pruneDomain :: CSP -> Assignment -> Variable -> Either Int String -> Map.Map Variable Domain -> Variable -> Map.Map Variable Domain
pruneDomain csp assignment assignedVar _ dom otherVar
  | otherVar == assignedVar = dom  -- No modificamos la variable ya asignada
  | Map.member otherVar assignment = dom  -- No se modifica el dominio de variables ya asignadas
  | otherwise =
      let domainValues   = Map.findWithDefault [] otherVar dom
          filteredDomain = filter (\val ->
                              let tempAssignment      = Map.insert otherVar val assignment
                                  relevantConstraints = filter (isConstraintRelevant assignedVar otherVar) (constraints csp)
                              in all (\constr -> partialConstraintSatisfied constr tempAssignment) relevantConstraints
                            ) domainValues
      in Map.insert otherVar filteredDomain dom


-- Evalúa una restricción en una asignación parcial.
-- Si todas las variables de la restricción están asignadas, se requiere que se cumpla.
-- Si no, se asume que no está violada aún.
partialConstraintSatisfied :: BoolExpression -> Assignment -> Bool
partialConstraintSatisfied expr assignment =
  let vars = getVariablesInConstraint expr
  in if all (`Map.member` assignment) vars
       then case evaluateBool assignment expr of
              Just True -> True
              _         -> False
       else True

-- Determina si una restricción involucra ambas variables:
-- la variable a la que se acaba de asignar y la variable cuyo dominio se está filtrando.
isConstraintRelevant :: Variable -> Variable -> BoolExpression -> Bool
isConstraintRelevant v1 v2 constraint =
  let vars = getVariablesInConstraint constraint
  in v1 `elem` vars && v2 `elem` vars

-- Extrae las variables involucradas en una expresión booleana
getVariablesInConstraint :: BoolExpression -> [Variable]
getVariablesInConstraint (RelOp _ e1 e2) = getVariablesInExpr e1 ++ getVariablesInExpr e2
getVariablesInConstraint (LogOp _ b1 b2) = getVariablesInConstraint b1 ++ getVariablesInConstraint b2
getVariablesInConstraint (Not b) = getVariablesInConstraint b

-- Extrae las variables involucradas en una expresión
getVariablesInExpr :: Expression -> [Variable]
getVariablesInExpr (Var v) = [v]
getVariablesInExpr (Val _) = []
getVariablesInExpr (BinOp _ e1 e2) = getVariablesInExpr e1 ++ getVariablesInExpr e2



------------------------------
-- AC-3 (Arc Consistency 3) --
------------------------------

arcConsistencySolver :: CSP -> [Assignment]
arcConsistencySolver (CSP vars doms consts) =
  let reducedDoms = ac3 vars doms consts
      reducedCSP  = CSP vars reducedDoms consts
  in bruteForceSolver reducedCSP


-- Genera la cola de arcos relevantes: solo aquellos pares (xi,xj)
-- para los cuales existe al menos una restricción que involucre ambas.
ac3 :: [Variable] -> Map.Map Variable Domain -> [BoolExpression] -> Map.Map Variable Domain
ac3 vars doms consts =
  let arcs = [(xi, xj) | xi <- vars, xj <- vars, xi /= xj, any (\c -> constraintInvolves c xi && constraintInvolves c xj) consts]
  in ac3Process arcs doms consts

-- Procesa la cola de arcos
ac3Process :: [(Variable, Variable)] -> Map.Map Variable Domain -> [BoolExpression] -> Map.Map Variable Domain
ac3Process [] doms _ = doms
ac3Process ((xi, xj):queue) doms consts =
  let (revised, newDoms) = revise xi xj doms consts
      -- Si se modificó el dominio de xi, se reinsertan en la cola los arcos (xk, xi)
      newQueue = if revised
                   then queue ++ [(xk, xi) | xk <- Map.keys newDoms, xk /= xi, any (\c -> constraintInvolves c xk && constraintInvolves c xi) consts]
                   else queue
  in ac3Process newQueue newDoms consts

-- Revisa el arco (xi, xj): elimina de xi los valores sin soporte en xj
revise :: Variable -> Variable -> Map.Map Variable Domain -> [BoolExpression] -> (Bool, Map.Map Variable Domain)
revise xi xj doms consts =
  let domainXi = Map.findWithDefault [] xi doms
      domainXj = Map.findWithDefault [] xj doms
      -- Filtra solo las restricciones que involucran ambas variables:
      relConstraints = [ c | c <- consts, constraintInvolves c xi, constraintInvolves c xj ]
      -- Para cada valor vi de xi, se conserva si existe algún vj en xj que, en la asignación parcial,
      -- haga que *todas* las restricciones relevantes se evalúen de forma satisfactoria (o no evaluables aún)
      reviseValue vi = any (\vj ->
                              all (\c -> partialConstraintSatisfied c (Map.fromList [(xi, vi), (xj, vj)]))
                                  relConstraints
                           ) domainXj
      newDomainXi = filter reviseValue domainXi
      revised     = length newDomainXi < length domainXi
      newDoms     = Map.insert xi newDomainXi doms
  in (revised, newDoms)

---------------------------------------------------------
-- Funciones auxiliares para evaluar restricciones parciales
---------------------------------------------------------

-- Determina si una restricción involucra a una variable dada.
constraintInvolves :: BoolExpression -> Variable -> Bool
constraintInvolves expr var = var `elem` getVariablesInConstraint expr



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