{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utility.TSLogAnalyzer.Util where

import           ClassyPrelude
import           Prelude.Unicode

import           Control.Applicative  (Alternative (..))
import           Data.Attoparsec.Text (Parser, char)


singleQuote, doubleQuote ∷ Parser ()
openParen, closeParen    ∷ Parser ()
colon, semicolon         ∷ Parser ()
period, comma            ∷ Parser ()
hyphen                   ∷ Parser ()
space                    ∷ Parser ()

singleQuote = void $ char '\''
doubleQuote = void $ char '"'
openParen   = void $ char '('
closeParen  = void $ char ')'
colon       = void $ char ':'
semicolon   = void $ char ';'
period      = void $ char '.'
comma       = void $ char ','
hyphen      = void $ char '-'
space       = void $ char ' '

(~>) ∷ Monad μ ⇒ μ α → β → μ β
a ~> b = a *> return b

(<~>) ∷ Functor φ ⇒ (α → β) → (γ → φ α) → γ → φ β
f <~> g = \x -> f <$> g x
infixr 9 <~>


-- <$ = flip (fmap . const)
-- fmap f x = pure f <*> x
-- (*>) :: f a -> f b -> f b
-- a1 *> return a2 = (id <$ a1) <*> pure a2

data GenParser α where
  GPPure   :: α → GenParser a
  GPThen   :: GenParser (α → β) → GenParser α → GenParser β
  GPChoose :: GenParser α → GenParser α → GenParser α
  GPMany   :: GenParser α → GenParser [α]
  GPPeek   :: GenParser Char
  GPChar   :: GenParser Char
  GPString :: IsString ς ⇒ GenParser ς
  GPLook   :: IsString ς ⇒ GenParser ς

class Applicative α ⇒ IsParser α where {}

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

