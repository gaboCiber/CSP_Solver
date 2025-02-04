module Constraints where

import Text.Megaparsec 
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Variables (variableParser)
import Control.Applicative ()
import CSP

type Parser = Parsec Void String

-- Funciones auxiliares para espacios
lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

-- Parser de números y strings
valueParser :: Parser (Either Int String)
valueParser = (Left <$> numberParser) <|> (Right <$> stringParser)

numberParser :: Parser Int
numberParser = lexeme (read <$> some digitChar)

stringParser :: Parser String
stringParser = lexeme (char '"' *> manyTill anySingle (char '"'))

-- Parser de expresiones aritméticas (con manejo de operadores relacionales)
factorParser :: Parser Expression
factorParser = 
  choice
    [ Var <$> variableParser
    , Val <$> valueParser
    , between (lexeme (char '(')) (lexeme (char ')')) exprParser
    ]

termParser :: Parser Expression
termParser = do
  first <- factorParser
  rest <- many $ do
    op <- lexeme $ choice
      [ MulOp <$ char '*'
      , try (DivOp <$ char '/' <* notFollowedBy (char '='))  -- ✅ Evita confundir "/" con "/="
      ]
    expr <- factorParser
    return (op, expr)
  return $ foldl (\acc (op, e) -> BinOp op acc e) first rest

exprParser :: Parser Expression
exprParser = do
  first <- termParser
  rest <- many $ do
    op <- lexeme $ choice
      [ ConcOp <$ string "++" 
      ,  AddOp  <$ char '+' <* notFollowedBy (char '+')
      , SubOp  <$ char '-'
      ]
    expr <- termParser
    return (op, expr)
  return $ foldl (\acc (op, e) -> BinOp op acc e) first rest

-- Parser de operadores relacionales (corregidos)
relOpParser :: Parser BoolOpType
relOpParser = lexeme $ choice
  [ EqOp  <$ string "=="
  , NeqOp <$ string "/="
  , LeOp  <$ string "<="
  , LtOp  <$ string "<"  <* notFollowedBy (char '=')  -- ✅ Evita confundir "<" con "<="
  , GeOp  <$ string ">="
  , GtOp  <$ string ">"  <* notFollowedBy (char '=')  -- ✅ Evita confundir ">" con ">="
  ]

relExprParser :: Parser BoolExpression
relExprParser = do
  e1 <- exprParser
  op <- relOpParser
  e2 <- exprParser
  return $ RelOp op e1 e2

-- Parser de expresiones lógicas con paréntesis
boolFactorParser :: Parser BoolExpression
boolFactorParser = 
  choice
    [ between (lexeme (char '(')) (lexeme (char ')')) boolExprParser  -- ✅ Permite paréntesis anidados
    , notParser
    , relExprParser
    ]

logOpParser :: Parser LogOpType
logOpParser = lexeme $ choice
  [ AndOp <$ string "&&"
  , OrOp  <$ string "||"
  ]

logExprParser :: Parser BoolExpression
logExprParser = do
  first <- boolFactorParser
  rest <- many $ do
    op <- logOpParser
    expr <- boolFactorParser
    return (op, expr)
  return $ foldl (\acc (op, e) -> LogOp op acc e) first rest

notParser :: Parser BoolExpression
notParser = do
  _ <- lexeme $ string "NOT"
  Not <$> boolFactorParser

boolExprParser :: Parser BoolExpression
boolExprParser = logExprParser

-- Función auxiliar para parsear una restricción individual
parseRelExpr :: String -> Either (ParseErrorBundle String Void) BoolExpression
parseRelExpr = parse relExprParser "relExpr"

-- Función principal
parseBoolExpr :: String -> Either (ParseErrorBundle String Void) BoolExpression
parseBoolExpr = parse (space *> boolExprParser <* eof) "boolExpr"

-- Parser para múltiples restricciones (una por línea)
constraintsParser :: Parser [BoolExpression]
constraintsParser = do
  skipMany (void spaceChar)  -- ✅ Ignora espacios iniciales
  c <- lexeme boolExprParser `sepEndBy` newline  -- ✅ Una restricción por línea
  eof  -- ✅ Asegura que no haya input residual
  return c

-- Función auxiliar para parsear una lista de restricciones
parseConstraints :: String -> Either (ParseErrorBundle String Void) [BoolExpression]
parseConstraints = parse constraintsParser "constraints"