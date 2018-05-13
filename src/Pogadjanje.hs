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
sledeceStanje x y igra = klikniPolje (floor ((x/vp)-1)) (floor ((y/vp)+2.5)) igra
                              where vp = velicina_polja

klikniPolje :: Int -> Int -> StanjeIgre -> StanjeIgre
klikniPolje _ _ igra = igra


