module Igra where

data VrednostPolja = Nista | Slika Int
  deriving (Eq, Ord, Show)

data StanjeIgre = Igra ([VrednostPolja], Int)
 | Pobeda [VrednostPolja]
 | Poraz [VrednostPolja]
   deriving (Eq, Show)

data Tacnost = Crveno
 | Zuto
 | Prazno
 | T VrednostPolja
  deriving (Eq, Ord, Show)