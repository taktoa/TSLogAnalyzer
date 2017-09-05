{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Utility functions
module Utility.TSLogAnalyzer.Util where

import           ClassyPrelude
import           Prelude.Unicode

import Control.Monad (void)
import Control.Applicative (Alternative (..))

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import qualified Data.ListLike as LL

import           Data.Text.ICU.Char   (Bool_ (WhiteSpace), property)


concatMapM :: (Monad μ,
              Traversable τ,
              LL.ListLike λ β,
              LL.ListLike (τ λ) λ) => (α -> μ λ) -> τ α -> μ λ
concatMapM f xs = LL.concat <$> mapM f xs

singleQuote, doubleQuote :: Parser Char
openParen, closeParen    :: Parser Char
colon, semicolon         :: Parser Char
period, comma            :: Parser Char
hyphen                   :: Parser Char
space                    :: Parser Char
whitespace               :: Parser Char
whitespace'              :: Parser Text

ε :: Alternative φ => φ α
ε = empty

singleQuote = A.char '\''
doubleQuote = A.char '"'
openParen   = A.char '('
closeParen  = A.char ')'
colon       = A.char ':'
semicolon   = A.char ';'
period      = A.char '.'
comma       = A.char ','
hyphen      = A.char '-'
space       = A.char ' '
whitespace  = A.satisfy (property WhiteSpace)
whitespace' = A.takeWhile (property WhiteSpace)

optional :: Parser α -> Parser ()
optional p = void $ A.option Nothing (Just <$> p)

optionMaybe :: MonadPlus μ => Parser α -> Parser (μ α)
optionMaybe p = A.option mzero (return <$> p)

(<~>) :: Functor φ => (α -> β) -> (γ -> φ α) -> γ -> φ β
f <~> g = \x -> f <$> g x
infixr 9 <~>

(<∘>) :: Functor φ => (α -> β) -> (γ -> φ α) -> γ -> φ β
f <∘> g = \x -> f <$> g x
infixr 9 <∘>

toPairs :: [α] -> [(α, α)]
toPairs []       = []
toPairs [_]      = []
toPairs (x:y:xs) = (x, y) : toPairs xs

data GenParser α where
  GPPure   :: α -> GenParser a
  GPThen   :: GenParser (α -> β) -> GenParser α -> GenParser β
  GPChoose :: GenParser α -> GenParser α -> GenParser α
  GPMany   :: GenParser α -> GenParser [α]
  GPPeek   :: GenParser Char
  GPChar   :: GenParser Char
  GPString :: IsString ς => GenParser ς
  GPLook   :: IsString ς => GenParser ς

class Applicative α => IsParser α where {}

-- instance Applicative GenParser where
--   pure  = GPPure
--   (<*>) = GPSeq
-- instance Alternative GenParser where
--   empty                   = GenParser empty
--   (GenParser a) <|> (GenParser b) = GenParser $ (a <|> b)
-- instance Functor GenParser where
--   fmap f (GenParser m) = GenParser (fmap f m)
-- instance Monad GenParser where
--   return           = GenParser ∘ return
--   fail             = GenParser ∘ fail
--   (GenParser m) >>= f  = GenParser $ m >>= getReadP ∘ f

