module Text.Gigaparsec.Internal.Fuzzing.ParsecAST where
import Test.Tasty.QuickCheck (Arbitrary)

{-|
This is a data type that represents a parser.
-}
data ParsecAST a where
  Pure      :: Show a => a -> ParsecAST a
  Satisfy   :: (Char -> Bool) -> ParsecAST Char
  Empty     :: ParsecAST a
  Char      :: Char -> ParsecAST Char
  Item      :: ParsecAST Char
  StringC   :: String -> ParsecAST String
  Atomic    :: ParsecAST a -> ParsecAST a
  LookAhead :: ParsecAST a -> ParsecAST a
  (:<*:)    :: ParsecAST a -> ParsecAST b -> ParsecAST a
  (:*>:)    :: ParsecAST b -> ParsecAST a -> ParsecAST a
  Fmap      :: (Arbitrary a, Show b) => (b -> a) -> ParsecAST b -> ParsecAST a

bracket :: Show a => a -> String
bracket s = "(" ++ show s ++ ")"

instance Show (ParsecAST a) where
  show :: ParsecAST a -> String
  show (Pure a)      = "Pure" ++ bracket a
  show (Satisfy _)   = "Satisfy |predicate|"
  show Empty         = "Empty"
  show (Char c)      = [c]
  show Item          = "Item"
  show (StringC s)   = s
  show (Atomic p)    = "Atomic" ++ bracket p
  show (LookAhead p) = "LookAhead" ++ bracket p
  show (l :*>: r)    = bracket l ++ " *> " ++ bracket r
  show (l :<*: r)    = bracket l ++ " <* " ++ bracket r
  show (Fmap _ p)    = "|f| <$> " ++ bracket p
