{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Text.Gigaparsec.Internal.Fuzzing.Compiler where

import           Control.Applicative
import           Test.Tasty.QuickCheck
import           Text.Gigaparsec
import           Text.Gigaparsec.Char
import           Text.Gigaparsec.Internal.Fuzzing.CombinatorAST qualified as C
import           Text.Gigaparsec.Internal.Fuzzing.ParsecAST

arbitraryCharPred :: Gen (Char -> Bool)
arbitraryCharPred = flip elem <$> (arbitrary :: Gen [Char])

arbitraryChar :: Gen Char
arbitraryChar = arbitrary

arbitraryString :: Gen String
arbitraryString = arbitrary

-- TODO: we should make b coArbitary with a
arbitraryFmap :: (Arbitrary b) => Gen (a -> b)
arbitraryFmap = const <$> arbitrary

traverseCombinator :: (Arbitrary a, Show a) => C.Combinator a -> Gen (ParsecAST a)
traverseCombinator combinator =
    case combinator of
        C.Pure         -> Pure <$> arbitrary
        C.Satisfy      -> Satisfy <$> arbitraryCharPred
        C.Empty        -> return Empty
        C.Char         -> Char <$> arbitraryChar
        C.Item         -> return Item
        C.StringC      -> StringC <$> arbitraryString
        C.Atomic c     -> Atomic <$> traverseCombinator c
        C.LookAhead c  -> LookAhead <$> traverseCombinator c
        c C.:<*: d     -> liftA2 (:<*:) (traverseCombinator c) (traverseCombinator d)
        c C.:*>: d     -> liftA2 (:*>:) (traverseCombinator c) (traverseCombinator d)
        C.Fmap c       -> liftA2 Fmap arbitraryFmap (traverseCombinator c)

compile :: ParsecAST a -> Parsec a
compile (Pure a)      = pure a
compile (Satisfy p)   = satisfy p
compile Empty         = empty
compile (Char c)      = char c
compile Item          = item
compile (StringC s)   = string s
compile (Atomic p)    = atomic (compile p)
compile (LookAhead p) = lookAhead (compile p)
compile (l :<*: r)    = undefined -- TODO
compile (l :*>: r)    = undefined -- TODO
compile (Fmap f p)    = f <$> compile p
