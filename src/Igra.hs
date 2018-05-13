module Igra where

data VrednostPolja = Nista | Slika Int
  deriving (Eq, Show)

data StanjeIgre = Igra [VrednostPolja]
 | Pobeda [VrednostPolja]
 | Poraz [VrednostPolja]
