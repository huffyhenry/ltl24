-- This file is part of the LTL24 data querying system.
-- Copyright (c) Marek Kwiatkowski, 2015. All rights reserved.
-- Unlicensed use and distribution prohibited.

module Parsers where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

import Literal
import EventProp
import LTL24

ltlDef = emptyDef { Token.commentStart = "/*",
                    Token.commentEnd = "*/",
                    Token.commentLine = "#",
                    Token.reservedNames = ["player_id", "team_id", "type_id",
                                           "period_id", "x", "y","qval", "hasq",
                                           "is successful", "is not successful",
                                           "is set", "is not set", "exists", "always", "next",
                                           "not", "implies", "and", "or"],
                    Token.reservedOpNames = ["=", "<", ">"]
                    }
lexer = Token.makeTokenParser ltlDef
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
natural = Token.natural lexer

ltl24 :: Parser LTL24
ltl24 = buildExpressionParser ltlOps ltl24Term <?> "game predicate"

ltlOps :: OperatorTable Char () LTL24
ltlOps = [[Prefix (reserved "exists" >> return Future),
           Prefix (reserved "always" >> return (Neg . Future . Neg)),
           Prefix (reserved "not" >> return Neg),
           Prefix (reserved "next" >> return Next),
           Infix (reserved "or" >> return Alt) AssocLeft,
           Infix (reserved "and" >> return (\p -> \p' -> Neg $ Alt (Neg p) (Neg p'))) AssocLeft,
           Infix (reserved "implies" >> return (\p -> \p' -> Alt (Neg p) p')) AssocNone]]


ltl24Term :: Parser LTL24
ltl24Term = parens ltl24 <|>
            (reserved "is not successful" >> (return . Neg . Atomic) Successful) <|>
            try (do{l <- literal; reserved "is not set"; (return . Neg . Atomic . IsSet) l}) <|>
            (eventProp >>= return . Atomic) <?>
            "game predicate"

eventProp :: Parser EventProp
eventProp = parens eventProp <|>
            (reserved "is successful" >> return Successful) <|>
            (reserved "hasq" >> parens natural >>= return . HasQ . fromIntegral) <|>
            try (do{l <- literal; reserved "is set"; return (IsSet l)}) <|>
            literalComp <?>
            "event predicate"

literalComp :: Parser EventProp
literalComp = (do l <- literal
                  op <- (reservedOp "=" >> return Equal) <|>
                        (reservedOp ">" >> return Greater) <|>
                        (reservedOp "<" >> return (flip Greater))
                  l' <- literal
                  return (op l l')) <?>
               "comparison of values"

literal :: Parser Literal
literal = (reserved "player_id" >> return PlayerId) <|>
          (reserved "team_id" >> return TeamId) <|>
          (reserved "type_id" >> return TypeId) <|>
          (reserved "x" >> return X) <|>
          (reserved "y" >> return Y) <|>
          (reserved "time" >> return Time) <|>
          (reserved "period_id" >> return PeriodId) <|>
          (reserved "qval" >> parens natural >>= return . QValue . fromIntegral) <|>
          (natural >>= return . Constant . show) <|>
          (parens literal >>= return) <?>
          "literal"
