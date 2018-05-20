module Main where

import Igra
import Prozor
import Crtanje(slika_sveta)
import Pogadjanje(dogadjaj)
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.Time.Clock.POSIX
import Random

slike_putanje :: [FilePath]
slike_putanje = map (\a -> a ++ ".bmp") ["srce", "romb", "skocko", "detelina", "list", "zvezda"]

pocetni_svet :: StanjeIgre
pocetni_svet = Igra (take 24 $ repeat Nista,0,take 24 $ repeat Prazno)

main :: IO ()
main = do
         trenutnoVreme <- fmap round getPOSIXTime
         slike <- mapM loadBMP slike_putanje
         let resenje = map (\x -> Slika x) $ create_random_list 4 trenutnoVreme
         play prozor boja 24 pocetni_svet (slika_sveta slike resenje) (dogadjaj resenje) prazno
           where prazno :: Float -> StanjeIgre -> StanjeIgre
                 prazno _ = id
