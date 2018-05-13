module Main where

import Igra
import Prozor
import Crtanje
import Graphics.Gloss

sva_polja :: [Picture]
sva_polja = v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli

slike_putanje :: [FilePath]
slike_putanje = map (\a -> a ++ ".bmp") ["srce", "romb", "skocko", "detelina", "list", "zvezda"]

pocetni_svet :: [VrednostPolja]
pocetni_svet = take 24 $ repeat Nista

pocetna_slika :: [Picture] -> [Picture]
pocetna_slika slike = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..3], j<-[0,1..5]] pocetni_svet

render :: [Picture] -> Picture
render slike = pictures $ (pocetna_slika slike) ++ (simboli_1 slike) ++ (simboli_2 slike) ++ sva_polja

main :: IO ()
main = do
         slike <- mapM loadBMP slike_putanje
         display prozor boja $ render slike
