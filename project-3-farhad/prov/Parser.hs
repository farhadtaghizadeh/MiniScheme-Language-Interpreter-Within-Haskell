module Parser where

import Data.Char
import Text.Parsec hiding (token)

import qualified Interface as AST
--------------------------------------------------------------------------------
-- Parsing
--
-- Parsing Scheme is actually pretty simple.  The only real complication are the
-- quote-like symbols and dotted tails.  I've used Parsec here---need to figure
-- out if I'm going to introduce Parsec or, if not, translate this to the Bird
-- parser combinators.

type Parser t = Parsec [Char] () t

followedBy :: Parser t -> Parser u -> Parser t
followedBy p q = do x <- p
                    q
                    return x

whiteSpace :: Parser String
whiteSpace = many (satisfy isSpace <|> (char ';' `followedBy` many (satisfy (not . ('\n' ==)))))

token :: Parser t -> Parser t
token p = p `followedBy` whiteSpace

surround :: Char -> Char -> Parser t -> Parser t
surround c d t = token (char c) >> (t `followedBy` token (char d))

bracketed :: Parser t -> Parser t
bracketed t = choice [ surround '(' ')' t
                     , surround '[' ']' t ]


braces :: Parser t -> Parser t
braces t = surround '{' '}' t

symbol :: Parser [Char]
symbol = token (many1 (satisfy (not . (isSpace .||. (`elem` ['(', ')', '[', ']', '{', '}', '\'', '$', '.', ';'])))))
    where (f .||. g) x = f x || g x

int :: Parser Integer
int = token (do f <- option id (char '-' >> return negate)
                ds <- many1 (satisfy isDigit)
                return (f (read ds)))

expr :: AST.SchemeData t => Bool -> Parser t
expr includeSugar
     | includeSugar = choice (always ++ sugar)
     | otherwise    = choice always
     where always = [ try (AST.number <$> int)
                    , try (token (string "#t")) >> return (AST.boolean True)
                    , try (token (string "#f")) >> return (AST.boolean False)
                    , symbol >>= return . AST.symbol
                    , bracketed (do (ds, e) <- listy
                                    return (case e of
                                                Nothing -> AST.proper ds
                                                Just e' -> AST.improper ds e')) ]
           listy = do ds <- many (expr includeSugar)
                      tl <- optionMaybe $
                            do token (char '.')
                               choice [ bracketed listy
                                      , do e <- expr includeSugar; return ([], Just e) ]
                      case tl of
                          Nothing -> return (ds, Nothing)
                          Just (es, f) -> return (ds ++ es, f)
           sugar = [ braces (do es <- many (expr includeSugar)
                                return $ AST.proper [AST.symbol "quote", AST.proper es])
                   , do e <- char '\'' >> (expr includeSugar)
                        return $ AST.proper [AST.symbol "quote", e]
                   , do e <- char '$' >> (expr includeSugar)
                        return $ AST.proper [AST.symbol "splice", e] ]

terminal :: Parser t -> Parser t
terminal p = p `followedBy` eof

parseExprs :: AST.SchemeData a => Bool -> String -> Either ParseError [a]
parseExprs includeSugar = runParser (terminal (whiteSpace >> many1 (expr includeSugar))) () ""