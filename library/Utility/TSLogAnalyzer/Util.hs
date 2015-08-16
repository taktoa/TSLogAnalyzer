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

(<~>) ∷ Functor φ ⇒ (α → β) → (γ → φ α) → γ → φ β
f <~> g = \x -> f <$> g x
infixr 9 <~>


data GenParser a where
  GPPure   :: a → GenParser a
  GPThen   :: GenParser (a → b) → GenParser a → GenParser b
  GPChoose :: GenParser a → GenParser a → GenParser a
  GPMany   :: GenParser a → GenParser [a]
  GPPeek   :: GenParser Char
  GPChar   :: GenParser Char
  GPString :: IsString s ⇒ GenParser s
  GPLook   :: IsString s ⇒ GenParser s

class Applicative a ⇒ IsParser a where {}

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

