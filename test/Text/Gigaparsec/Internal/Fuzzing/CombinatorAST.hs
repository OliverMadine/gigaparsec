{-# LANGUAGE FlexibleInstances    #-}

module Text.Gigaparsec.Internal.Fuzzing.CombinatorAST where

import           Control.Applicative
import           Test.Tasty.QuickCheck

{-|
This is a data type that represents the structure of a parser. It excludes any runtime values, which
allows it to serve as a fully inspectable intermediate representation.
-}
data Combinator a where
  Pure      :: Combinator a
  Satisfy   :: Combinator Char
  Empty     :: Combinator a
  Char      :: Combinator Char
  Item      :: Combinator Char
  StringC   :: Combinator String
  Atomic    :: Combinator a -> Combinator a
  LookAhead :: Combinator a -> Combinator a
  (:<*:)    :: (Show b, Arbitrary b) => Combinator a -> Combinator b -> Combinator a
  (:*>:)    :: (Show b, Arbitrary b) => Combinator b -> Combinator a -> Combinator a
  Fmap      :: (Show b, Arbitrary b) => Combinator b -> Combinator a

bracket :: Show a => a -> String
bracket s = "(" ++ show s ++ ")"

instance Show (Combinator a) where
  show :: Combinator a -> String
  show combinator =
    case combinator of
      Pure        -> "Pure |x|"
      Satisfy     -> "Satisfy |f|"
      Empty       -> "Empty"
      Char        -> "Char |c|"
      Item        -> "Item"
      StringC     -> "String |s|"
      Atomic c    -> "Atomic " ++ bracket c
      LookAhead c -> "LookAhead " ++ bracket c
      l :*>: r    -> show l ++ " *> " ++ show r
      l :<*: r    -> show l ++ " <* " ++ show r
      Fmap c      ->  "f <$> " ++ show c

arbitraryEndomorphism :: Gen (Combinator a) -> Gen (Combinator a)
arbitraryEndomorphism a = frequency
  [ (1, Atomic <$> a)
  , (1, LookAhead <$> a)
  ]

arbitraryMorphism :: (Arbitrary b, Show b) => Gen (Combinator b) -> Gen (Combinator a)
arbitraryMorphism a = Fmap <$> a

arbitraryBinary :: (Arbitrary b, Show b) => Gen (Combinator a) -> Gen (Combinator b) -> Gen (Combinator a)
arbitraryBinary a b = frequency
  [ (1, liftA2 (:<*:) a b)
  , (1, liftA2 (:*>:) b a)
  ]

arbitraryChar :: Gen (Combinator Char)
arbitraryChar = arbitrary

arbitraryString :: Gen (Combinator String)
arbitraryString = arbitrary

-- Todo: How to reduce duplication here?
-- instance Arbitrary a => Arbitrary (Combinator a) where ...

instance Arbitrary (Combinator Char) where
  arbitrary :: Gen (Combinator Char)
  arbitrary = frequency
    [ (1, arbitraryBinary arbitrary arbitraryChar)
    , (1, arbitraryBinary arbitrary arbitraryString)
    , (1, arbitraryEndomorphism arbitrary)
    , (1, arbitraryMorphism arbitraryChar)
    , (1, arbitraryMorphism arbitraryString)
    , (1, pure Pure)
    , (1, pure Empty)
    , (1, pure Satisfy)
    , (1, pure Char)
    , (1, pure Item)
    ]

instance Arbitrary (Combinator String) where
  arbitrary :: Gen (Combinator String)
  arbitrary = frequency
    [ (1, arbitraryBinary arbitrary arbitraryChar)
    , (1, arbitraryBinary arbitrary arbitraryString)
    , (1, arbitraryEndomorphism arbitrary)
    , (1, arbitraryMorphism arbitraryChar)
    , (1, arbitraryMorphism arbitraryString)
    , (1, pure Pure)
    , (1, pure Empty)
    , (1, pure StringC)
    ]