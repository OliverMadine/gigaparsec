{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Text.Gigaparsec.Token.Patterns (overloadedStrings) where

import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Token.Lexer (lexeme, sym)

import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax (Q, Dec, Exp)

{-|
When given a quoted reference to a 'Text.Gigaparsec.Token.Lexer', for example
@[|lexer|]@, this function will synthesise an `IsString` instance that will
allow string literals to serve as @Parsec ()@. These literals will parse symbols
in the language associated with the lexer, followed by consuming valid whitespace.

@since 0.2.2.0
-}
overloadedStrings :: Q Exp   -- ^ the quoted 'Text.Gigaparsec.Token.Lexer'
                  -> Q [Dec] -- ^ a synthesised `IsString` instance.
overloadedStrings qlexer = [d|
    instance u ~ () => IsString (Parsec u) where
      fromString = sym (lexeme $qlexer) -- TODO: one day, $qlexer.lexeme.sym
  |]
