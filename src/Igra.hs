module Igra where

data VrednostPolja = Nista | Slika Int
  deriving (Eq, Ord, Show)

data StanjeIgre = Igra ([VrednostPolja], Int, [Tacnost])
 | Pobeda ([VrednostPolja], [Tacnost])
 | Poraz ([VrednostPolja], [Tacnost])
   deriving (Eq, Show)

data Tacnost = Crveno
 | Zuto
 | Prazno
 | T VrednostPolja
  deriving (Eq, Ord, Show)