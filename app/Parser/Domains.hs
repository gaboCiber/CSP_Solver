module Domains where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as Map
import Data.Void
import Variables (variableParser)
import CSP (Variable, Domain)

type Parser = Parsec Void String

-- Parser para un valor numérico o string dentro del dominio
domainValueParser :: Parser (Either Int String)
domainValueParser = do
  space
  value <- (Left <$> numberParser) <|> (Right <$> stringParser)
  space
  return value

-- Parser para un número entero
numberParser :: Parser Int
numberParser = read <$> some digitChar

-- Parser para una cadena entre comillas dobles ("texto")
stringParser :: Parser String
stringParser = char '"' *> manyTill anySingle (char '"')

-- Parser para un dominio (lista de valores entre `{}` separados por `,`)
domainParser :: Parser [Either Int String]
domainParser = do
  space
  _ <- char '{'
  space
  values <- domainValueParser `sepBy` (char ',' >> space)
  space
  _ <- char '}'
  space
  return values

-- Parser para una línea de dominio: "x1: {1,2,3}"
domainLineParser :: Parser (Variable, [Either Int String])
domainLineParser = do
  var <- variableParser  -- ✅ Usa `variableParser` en lugar de `some letterChar`
  _ <- char ':'  -- Detecta el separador
  domain <- domainParser  -- Parsea el dominio
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