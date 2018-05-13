module Prozor where

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color

--velicina jednog polja
velicina_polja :: (Num a) => a
velicina_polja = 64

--velicina prozora
sirina :: Int
sirina = 9*velicina_polja

visina :: Int
visina = 8*velicina_polja

--pozicija prozora
pozicija :: Int
pozicija = 0

--glavni prozor
prozor :: Display
prozor = InWindow "Skocko" (sirina,visina) (pozicija,pozicija)

--boja pozadine
boja :: Color
boja = blue
