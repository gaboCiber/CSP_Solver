module Parser where

import CSP
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Text.Megaparsec 
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void (Void)

parseVariables :: String -> [Variable]
parseVariables input =
  case words input of
    ("variables:" : vars) -> map (filter (/= ',')) vars
    _                     -> error "Formato incorrecto para las variables"

parseDomains :: (Read a) => String -> Map.Map Variable (Domain a)
parseDomains input =
  let parseDomain line =
        case break (== ':') line of
          (var, ':' : dom) ->
            case readMaybe dom of
              Just d  -> (var, d)
              Nothing -> error $ "Formato incorrecto en el dominio: " ++ line
          _ -> error $ "Formato incorrecto en la línea: " ++ line
  in Map.fromList (map parseDomain (lines input))

-- Tipos y espacios básicos
type Parser = Parsec Void String

-- Espacios en blanco
sc :: Parser ()
sc = L.space space1 empty empty

-- Auxiliar para manejar espacios alrededor de un parser
lexeme :: Parser a -> Parser a
lexeme p = sc *> p <* sc

-- Parser de símbolos (con espacios)
symbol :: String -> Parser String
symbol = lexeme . L.symbol sc

-- Parser para paréntesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parser para números enteros (con manejo de signos y espacios)
integer :: Parser Int
integer = lexeme (L.signed sc L.decimal)

-- Parser para términos básicos (valores o paréntesis)
term :: Parser (Expression Int)
term = parens expression <|> (Val <$> integer)

-- Parser para operadores binarios
binOpParser :: String -> (Int -> Int -> Int) -> Parser (Expression Int -> Expression Int -> Expression Int)
binOpParser name op = do
  _ <- symbol name
  return (BinOp op)

-- Función auxiliar para aplicar operadores con precedencia
chainLeft :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft base op = foldl (\x (f, y) -> f x y) <$> base <*> many ((,) <$> op <*> base)

-- Precedencia de operadores
expression :: Parser (Expression Int)
expression = addSub
  where
    -- Nivel de suma/resta
    addSub = mulDiv `chainLeft` addSubOps
    addSubOps = binOpParser "+" (+) <|> binOpParser "-" (-)
    -- Nivel de multiplicación/división
    mulDiv = term `chainLeft` mulDivOps
    mulDivOps = binOpParser "*" (*) <|> binOpParser "/" div

