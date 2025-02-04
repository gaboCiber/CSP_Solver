module Domains where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as Map
import Data.Void
import Variables (variableParser)
import CSP (Variable, Domain)

type Parser = Parsec Void String

-- Funciones auxiliares para espacios
lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

-- Parser para un valor numérico o string dentro del dominio
domainValueParser :: Parser (Either Int String)
domainValueParser = lexeme ((Left <$> numberParser) <|> (Right <$> stringParser))

-- Parser para un número entero
numberParser :: Parser Int
numberParser = do
  sign <- optional (char '~')
  digits <- some digitChar
  return $ case sign of
    Just _  -> - (read digits)
    Nothing -> read digits

-- Parser para una cadena entre comillas dobles ("texto")
stringParser :: Parser String
stringParser = char '"' *> manyTill anySingle (char '"')

-- **Nuevo: Parser para rangos `{1..10}`**
domainRangeParser :: Parser [Either Int String]
domainRangeParser = do
  start <- lexeme numberParser
  _ <- lexeme (string "..")  -- Detecta el rango `..`
  end <- lexeme numberParser
  if start <= end
    then return [Left n | n <- [start..end]]  -- Genera la lista de números
    else fail "Invalid range: start must be <= end"

-- **Modificación de `domainParser` para incluir rangos y listas**
domainParser :: Parser [Either Int String]
domainParser = do
  space
  _ <- char '{'
  space
  values <- try domainRangeParser  -- Intenta primero parsear un rango `{1..10}`
         <|> (domainValueParser `sepBy` (char ',' >> space))  -- Si falla, parsea una lista `{1,2,3}`
  space
  _ <- char '}'
  space
  return values

-- Parser para una línea de dominio: `"x1: {1,2,3}"` o `"x2: {1..10}"`
domainLineParser :: Parser (Variable, [Either Int String])
domainLineParser = do
  var <- variableParser
  _ <- lexeme (char ':')
  domain <- domainParser
  return (var, domain)

-- Parser para múltiples líneas de dominios
domainsParser :: Parser (Map.Map Variable [Either Int String])
domainsParser = do
  entries <- domainLineParser `sepEndBy` space
  eof
  return (Map.fromList entries)

-- Función auxiliar para parsear un dominio individual
parseDomain :: String -> Either (ParseErrorBundle String Void) [Either Int String]
parseDomain = parse domainParser "domain"

-- Función auxiliar para parsear un mapa de dominios
parseDomains :: String -> Either (ParseErrorBundle String Void) (Map.Map Variable [Either Int String])
parseDomains = parse domainsParser "domains"

getDomains :: Either (ParseErrorBundle String Void) (Map.Map Variable [Either Int String]) -> (Map.Map Variable [Either Int String])
getDomains (Right dom) = dom
getDomains (Left _) = error "Parsing failed"
