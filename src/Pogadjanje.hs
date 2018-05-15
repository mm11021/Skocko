module Pogadjanje where

import Prozor
import Igra
import Graphics.Gloss.Interface.Pure.Game

--funkcija koja reaguje na dogadjaj (samo na klik misem)
dogadjaj :: Event -> StanjeIgre -> StanjeIgre

--reakcija na levi klik
dogadjaj (EventKey (MouseButton LeftButton) Down _ (x,y)) igra = sledeceStanje x y igra

--na ostale dogadjaje ne reaguje
dogadjaj _ igra = igra

sledeceStanje :: Float -> Float -> StanjeIgre -> StanjeIgre
sledeceStanje x y igra = klikniPolje (floor (x/vp-0.5)) (floor ((-y)/vp-2)) igra
                              where vp = velicina_polja

klikniPolje :: Int -> Int -> StanjeIgre -> StanjeIgre
klikniPolje x y igra@(Igra stanje) = if (elem x [0,1,2]) && (elem y [0,1])
                                       then Igra (Slika (3*y+x) : (drop 1 stanje))
                                       else igra
