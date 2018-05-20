module Main where

import Igra
import Prozor
import Crtanje
import Pogadjanje(resenje,dogadjaj)
import Graphics.Gloss.Interface.IO.Game

linije :: [Picture]
linije = v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli ++ v_linije_desno ++ u_linije_desno

slike_putanje :: [FilePath]
slike_putanje = map (\a -> a ++ ".bmp") ["srce", "romb", "skocko", "detelina", "list", "zvezda"]

pocetni_svet :: StanjeIgre
pocetni_svet = Igra (take 24 $ repeat Nista,0,take 24 $ repeat Prazno)

slika_sveta :: [Picture] -> StanjeIgre -> IO Picture
slika_sveta slike (Igra (stanje,_,tacnost)) = let slika_levo = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] stanje
                                                  slika_desno = map (\((a,b),polje) -> nacrtaj_polje_1 (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] tacnost
                                              in return $ pictures $ slika_levo ++ slika_desno ++ bla slike

slika_sveta slike (Pobeda (stanje,tacnost)) = slika_sveta slike (Igra (stanje,0,tacnost))

slika_sveta slike (Poraz (stanje,tacnost)) = let slika_levo = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] stanje
                                                 slika_desno = map (\((a,b),polje) -> nacrtaj_polje_1 (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] tacnost
                                                 slika_dole = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(7,j)| j<-[0,1..3]] resenje
                                                 v_linije_dole = take 2 $ map (color black . (\x -> line [(-ps,x*vp-pv),(4*vp-ps,x*vp-pv)])) [0,1..]
                                                 u_linije_dole = take 5 $ map (color black . (\x -> line [(x*vp-ps,-pv),(x*vp-ps,vp-pv)])) [0,1..]
                                             in return $ pictures $ slika_levo ++ slika_desno ++ slika_dole ++ bla slike ++ v_linije_dole ++ u_linije_dole
                                                      where vp = velicina_polja
                                                            ps = (fromIntegral sirina) / 2
                                                            pv = (fromIntegral visina) / 2

bla :: [Picture] -> [Picture]
bla slike = (simboli_1 slike) ++ (simboli_2 slike) ++ linije

main :: IO ()
main = do
         slike <- mapM loadBMP slike_putanje
         playIO prozor boja 24 pocetni_svet (slika_sveta slike) dogadjaj prazno
           where prazno :: Float -> StanjeIgre -> IO StanjeIgre
                 prazno _ x = return x
