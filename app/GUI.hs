module GUI where

import qualified Data.Map as Map
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.HPaned
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import CSP
import Variables
import Domains
import Constraints (parseBoolExpr, parseConstraints)
import Solvers
import Data.Either
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)

createGUI :: Window -> IO ()
createGUI window = do
    set window [ windowTitle := "CSP Solver",
                 windowDefaultWidth := 800,
                 windowDefaultHeight := 600 ]

    hpaned <- hPanedNew
    containerAdd window hpaned

    leftPanel <- vBoxNew False 10
    panedAdd1 hpaned leftPanel

    rightPanel <- vBoxNew False 10
    panedAdd2 hpaned rightPanel

    createInputField leftPanel "Variables (ej: x1, x2):"
    varsTextView <- textViewNew
    packTextView leftPanel varsTextView

    createInputField leftPanel "Dominios (ej: x1: {1,2}, x2: {\"a\",\"b\"}):"
    domainsTextView <- textViewNew
    packTextView leftPanel domainsTextView

    createInputField leftPanel "Restricciones (ej: x1 < 5 && x2 == \"a\"):"
    constraintsTextView <- textViewNew
    packTextView leftPanel constraintsTextView

    controlsBox <- hBoxNew False 10
    boxPackStart leftPanel controlsBox PackNatural 0
    
    algoCombo <- comboBoxNewText
    mapM_ (comboBoxAppendText algoCombo . T.pack)
        ["Brute Force", "Backtracking", "Forward Checking", "Arc Consistency (AC-3)", "Min-Conflicts"]
    comboBoxSetActive algoCombo 0
    boxPackStart controlsBox algoCombo PackGrow 0

    solveButton <- buttonNewWithLabel "Resolver CSP"
    boxPackStart controlsBox solveButton PackNatural 0

    resultLabel <- labelNew (Just "Resultados aparecerán aquí")
    labelSetLineWrap resultLabel True
    scrolledResults <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledResults resultLabel
    boxPackStart rightPanel scrolledResults PackGrow 0

    void $ solveButton `on` buttonPressEvent $ tryEvent $ do
        varsText <- getTextFromView varsTextView
        domainsText <- getTextFromView domainsTextView
        constraintsText <- getTextFromView constraintsTextView

        algoIndex <- liftIO $ comboBoxGetActive algoCombo
        let solver = case algoIndex of
                      0 -> bruteForceSolver
                      1 -> backtrackingSolver
                      2 -> forwardCheckingSolver
                      3 -> arcConsistencySolver
                      4 -> minConflictsSolver
                      _ -> bruteForceSolver

        let varsResult = parseVariables varsText
            domsResult = parseDomains domainsText
            constrResult = parseConstraints constraintsText
            allErrors = lefts [varsResult] ++ 
                        lefts [domsResult] ++ 
                        lefts [constrResult]

        if not (null allErrors)
            then showErrors resultLabel allErrors
            else do
                let Right vars = varsResult
                    Right doms = domsResult
                    Right constr = constrResult
                    csp = CSP vars doms constr
                    solutions = solver csp
                liftIO $ labelSetText resultLabel $ formatSolutions solutions

        liftIO $ panedSetPosition hpaned 400

    widgetShowAll window

-- Funciones auxiliares (sin cambios)
createInputField :: VBox -> String -> IO ()
createInputField container labelText = do
    label <- labelNew (Just labelText)
    boxPackStart container label PackNatural 0

packTextView :: VBox -> TextView -> IO ()
packTextView container textView = do
    scrolled <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrolled PolicyAutomatic PolicyAutomatic
    containerAdd scrolled textView
    boxPackStart container scrolled PackGrow 0

getTextFromView :: TextView -> EventM EButton String
getTextFromView view = do
    buffer <- liftIO $ textViewGetBuffer view
    start <- liftIO $ textBufferGetStartIter buffer
    end <- liftIO $ textBufferGetEndIter buffer
    liftIO $ textBufferGetText buffer start end True

showErrors :: Label -> [ParseErrorBundle String Void] -> EventM EButton ()
showErrors label errors = do
    let errorMsg = "Errores de parseo:\n" ++ unlines (map errorBundlePretty errors)
    liftIO $ labelSetText label errorMsg

formatSolutions :: [Assignment] -> String
formatSolutions [] = "No se encontraron soluciones"
formatSolutions solutions = unlines $ map show solutions