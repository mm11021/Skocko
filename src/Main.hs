module Main where

import Igra
import Prozor
import Crtanje
import Pogadjanje(dogadjaj)
import Graphics.Gloss

linije :: [Picture]
linije = v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli

slike_putanje :: [FilePath]
slike_putanje = map (\a -> a ++ ".bmp") ["srce", "romb", "skocko", "detelina", "list", "zvezda"]

pocetni_svet :: StanjeIgre
pocetni_svet = Igra (take 24 $ repeat Nista)

slika_sveta :: [Picture] -> StanjeIgre -> Picture
slika_sveta slike (Igra stanje) = pictures $ (map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..3], j<-[0,1..5]] stanje) ++ bla slike

slika_sveta slike (Pobeda stanje) = slika_sveta slike (Igra stanje)

slika_sveta slike (Poraz stanje) = slika_sveta slike (Igra stanje)

bla :: [Picture] -> [Picture]
bla slike = (simboli_1 slike) ++ (simboli_2 slike) ++ linije

main :: IO ()
main = do
         slike <- mapM loadBMP slike_putanje
         play prozor boja 24 pocetni_svet (slika_sveta slike) dogadjaj prazno
           where prazno :: Float -> StanjeIgre -> StanjeIgre
                 prazno _ = id
