module Main where

import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)  -- Importar liftIO
import GUI

main :: IO ()
main = do
    initGUI

    window <- windowNew
    set window [ windowTitle := "CSP Solver"
               , windowDefaultWidth := 600
               , windowDefaultHeight := 400 ]

    createGUI window

    -- Usar liftIO para elevar mainQuit al contexto correcto
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI