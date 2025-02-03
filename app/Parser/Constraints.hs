module Constraints where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import CSP (BoolExpression(..), Expression(..), BoolOpType(..), BinOpType(..), LogOpType(..))  
import Variables (variableParser)

type Parser = Parsec Void String  -- Tipo de parser

-- Parser para un número entero o string entre comillas
valueParser :: Parser (Either Int String)
valueParser = (Left <$> numberParser) <|> (Right <$> stringParser)

-- Parser para un número entero
numberParser :: Parser Int
numberParser = read <$> some digitChar

-- Parser para una cadena entre comillas dobles ("texto")
stringParser :: Parser String
stringParser = char '"' *> manyTill anySingle (char '"')

-- Parser para operadores relacionales
relOpParser :: Parser BoolOpType
relOpParser = choice
  [ string "=="  >> return EqOp
  , string "/="  >> return NeqOp
  , string "<="  >> return LeOp
  , string "<"   >> return LtOp
  , string ">="  >> return GeOp
  , string ">"   >> return GtOp
  ]

-- Parser para operadores binarios
binOpParser :: Parser BinOpType
binOpParser = choice
  [ string "++" >> return ConcOp
  ,  string "+"  >> return AddOp
  , string "-"  >> return SubOp
  , string "*"  >> return MulOp
  , string "/"  >> return DivOp
  ]

-- Parser para operadores lógicos
logOpParser :: Parser LogOpType
logOpParser = choice
  [ string "&&" >> return AndOp
  , string "||" >> return OrOp
  ]

-- Parser para una variable o un valor constante
factorParser :: Parser Expression
factorParser =
  choice
    [ Var <$> variableParser
    , Val <$> valueParser
    , between (char '(' >> space) (char ')' >> space) exprParser
    ]

-- Parser manual para términos con `*` y `/`
termParser :: Parser Expression
termParser = do
  first <- factorParser
  rest <- many ((,) <$> (space *> binOpParser <* space) <*> factorParser)
  return (foldl (\acc (op, expr) -> BinOp op acc expr) first rest)

-- Parser manual para expresiones con `+`, `-`, `++`
exprParser :: Parser Expression
exprParser = do
  first <- termParser
  rest <- many ((,) <$> (space *> binOpParser <* space) <*> termParser)
  return (foldl (\acc (op, expr) -> BinOp op acc expr) first rest)

-- Parser para una restricción con `RelOp`
relExprParser :: Parser BoolExpression
relExprParser = do
  space
  e1 <- exprParser
  space
  op <- relOpParser
  space
  e2 <- exprParser
  space
  return (RelOp op e1 e2)

-- Función auxiliar para parsear una restricción individual
parseRelExpr :: String -> Either (ParseErrorBundle String Void) BoolExpression
parseRelExpr = parse relExprParser "relExpr"

-- Parser para la negación `NOT`
notParser :: Parser BoolExpression
notParser = do 
  _ <- string "NOT"
  space
  Not <$> boolExprParser

-- Parser para expresiones booleanas con `&&` y `||`
logExprParser :: Parser BoolExpression
logExprParser = do
  first <- relExprParser
  rest <- many ((,) <$> (space *> logOpParser <* space) <*> relExprParser)
  return (foldl (\acc (op, expr) -> LogOp op acc expr) first rest)

-- Parser general de expresiones booleanas
boolExprParser :: Parser BoolExpression
boolExprParser = try notParser <|> logExprParser

parseBoolExprParser:: String -> Either (ParseErrorBundle String Void) BoolExpression
parseBoolExprParser = parse boolExprParser "logExpr"

