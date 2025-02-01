module Variables where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Void
import CSP (Variable)  -- Importamos `Variable` de CSP
import Data.Either (rights)

type Parser = Parsec Void String  -- Definimos nuestro tipo de parser

-- Parser para una sola variable SIN `eof`
variableParser :: Parser Variable
variableParser = do
  space
  first <- letterChar
  rest <- many (letterChar <|> digitChar <|> char '_')
  space
  return (first : rest)

-- Parser que consume solo UNA variable y exige que termine la entrada (con `eof`)
singleVariableParser :: Parser Variable
singleVariableParser = do
  space
  var <- variableParser
  space
  eof  -- Asegurar que no haya más texto después
  return var

-- Parser para una lista de variables separadas por comas
variablesParser :: Parser [Variable]
variablesParser = do
  space
  vars <- variableParser `sepBy1` (char ',' >> space)  -- Permite espacios después de `,`
  space
  eof
  return vars

-- Función auxiliar para parsear UNA variable
parseVariable :: String -> Either (ParseErrorBundle String Void) Variable
parseVariable = parse singleVariableParser "variable"

-- Función auxiliar para parsear MÚLTIPLES variables
parseVariables :: String -> Either (ParseErrorBundle String Void) [Variable]
parseVariables = parse variablesParser "variables"

getVariables :: Either (ParseErrorBundle String Void) [Variable] -> [Variable]
getVariables (Right var) = var
getVariables (Left _) = error "Parsing failed"