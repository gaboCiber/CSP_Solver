module GUI where

import qualified Data.Map as Map
import Graphics.UI.Gtk
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import CSP
import Variables
import Domains
import Constraints (parseBoolExpr, parseConstraints)
import Solvers
import Data.Either
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

createGUI :: Window -> IO ()
createGUI window = do
    vbox <- vBoxNew False 10
    containerAdd window vbox

    -- Campos de entrada
    varsTextView <- textViewNew
    domainsTextView <- textViewNew
    constraintsTextView <- textViewNew

    -- Labels
    varsLabel <- labelNew (Just "Variables (ej: x1, x2):")
    domainsLabel <- labelNew (Just "Dominios (ej: x1: {1,2}, x2: {\"a\",\"b\"}):")
    constraintsLabel <- labelNew (Just "Restricciones (ej: x1 < 5 && x2 == \"a\"):")
    resultLabel <- labelNew (Just "Resultados aparecerán aquí")

    -- Botón
    solveButton <- buttonNewWithLabel "Resolver CSP"

    -- Layout
    boxPackStart vbox varsLabel PackNatural 0
    boxPackStart vbox varsTextView PackGrow 0
    boxPackStart vbox domainsLabel PackNatural 0
    boxPackStart vbox domainsTextView PackGrow 0
    boxPackStart vbox constraintsLabel PackNatural 0
    boxPackStart vbox constraintsTextView PackGrow 0
    boxPackStart vbox solveButton PackNatural 0
    boxPackStart vbox resultLabel PackNatural 0

    -- Manejador del botón
    void $ solveButton `on` buttonPressEvent $ tryEvent $ do
        -- Obtener texto de varsTextView
        varsBuffer <- liftIO $ textViewGetBuffer varsTextView
        varsStart <- liftIO $ textBufferGetStartIter varsBuffer
        varsEnd <- liftIO $ textBufferGetEndIter varsBuffer
        varsText <- liftIO $ textBufferGetText varsBuffer varsStart varsEnd True

        -- Obtener texto de domainsTextView
        domainsBuffer <- liftIO $ textViewGetBuffer domainsTextView
        domainsStart <- liftIO $ textBufferGetStartIter domainsBuffer
        domainsEnd <- liftIO $ textBufferGetEndIter domainsBuffer
        domainsText <- liftIO $ textBufferGetText domainsBuffer domainsStart domainsEnd True

        -- Obtener texto de constraintsTextView
        constraintsBuffer <- liftIO $ textViewGetBuffer constraintsTextView
        constraintsStart <- liftIO $ textBufferGetStartIter constraintsBuffer
        constraintsEnd <- liftIO $ textBufferGetEndIter constraintsBuffer
        constraintsText <- liftIO $ textBufferGetText constraintsBuffer constraintsStart constraintsEnd True

        -- Parsear y recolectar errores
        let varsResult = parseVariables varsText
            domsResult = parseDomains domainsText
            constrResult = parseConstraints constraintsText

            -- Lista de errores (si existen)
            varErrors = [err | Left err <- [varsResult]]
            domErrors = [err | Left err <- [domsResult]]
            constrErrors = [err | Left err <- [constrResult]]

            allErrors = varErrors ++ domErrors ++ constrErrors

        -- Si hay errores, mostrarlos
        if not (null allErrors)
            then do
                let errorMsg = "Errores de parseo:\n" ++ unlines (map errorBundlePretty allErrors)
                liftIO $ labelSetText resultLabel errorMsg
            else do
                -- Extraer valores correctos
                let Right vars = varsResult
                    Right doms = domsResult
                    Right constr = constrResult

                    csp = CSP 
                        { variables = vars
                        , domains = doms
                        , constraints = constr
                        }
                    solutions = bruteForceSolver csp
                liftIO $ labelSetText resultLabel $ "Soluciones:\n" ++ show solutions

