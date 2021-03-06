module Crtanje where

import Igra
import Prozor(velicina_polja,sirina,visina)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

slika_sveta :: [Picture] -> [VrednostPolja] -> StanjeIgre -> Picture
slika_sveta slike _ (Igra (stanje,_,tacnost)) = let slika_levo = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] stanje
                                                    slika_desno = map (\((a,b),polje) -> nacrtaj_polje_1 (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] tacnost
                                              in pictures $ slika_levo ++ slika_desno ++ donja_tabla slike

slika_sveta slike x (Pobeda (stanje,tacnost)) = slika_sveta slike x (Igra (stanje,0,tacnost))

slika_sveta slike resenje (Poraz (stanje,tacnost)) = let slika_levo = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] stanje
                                                         slika_desno = map (\((a,b),polje) -> nacrtaj_polje_1 (a,b) polje) $ zip [(i,j)| i<-[0,1..5], j<-[0,1..3]] tacnost
                                                         slika_dole = map (\((a,b),polje) -> nacrtaj_polje slike (a,b) polje) $ zip [(7,j)| j<-[0,1..3]] resenje
                                                     in pictures $ slika_levo ++ slika_desno ++ donja_tabla slike ++ slika_dole ++ v_linije_dole ++ u_linije_dole
                                                          where vp = velicina_polja
                                                                ps = (fromIntegral sirina) / 2
                                                                pv = (fromIntegral visina) / 2

--linije na levoj, desno i donjoj tabli
linije :: [Picture]
linije = v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli ++ v_linije_desno ++ u_linije_desno

--
donja_tabla :: [Picture] -> [Picture]
donja_tabla slike = (simboli_1 slike) ++ (simboli_2 slike) ++ linije

--funkcija koja crta pravouganik cija su dijagonalna temena (x0,y0) i (x1,y1)
nacrtaj_pravougaonik :: Float -> Float -> Float -> Float -> Picture
nacrtaj_pravougaonik x0 y0 x1 y1 = color white $ polygon [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]

--vodoravne linije table sa leve strane
v_linije :: [Picture]
v_linije = take 7 $ map (color black . (\x -> line [(-ps,pv-x*vp),(4*vp-ps,pv-x*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--uspravne linije table sa leve strane
u_linije :: [Picture]
u_linije = take 5 $ map (color black . (\x -> line [(x*vp-ps,pv),(x*vp-ps,pv-6*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--vodoravne linije table sa desne strane
v_linije_desno :: [Picture]
v_linije_desno = take 7 $ map (color black . (\x -> line [(ps,pv-x*vp),(ps-4*vp,pv-x*vp)])) [0,1..]
                   where vp = velicina_polja
                         ps = (fromIntegral sirina) / 2
                         pv = (fromIntegral visina) / 2

--uspravne linije table sa desne strane
u_linije_desno :: [Picture]
u_linije_desno = take 5 $ map (color black . (\x -> line [(ps-x*vp,pv),(ps-x*vp,pv-6*vp)])) [0,1..]
                   where vp = velicina_polja
                         ps = (fromIntegral sirina) / 2
                         pv = (fromIntegral visina) / 2

--vodoravne linije resenja
v_linije_dole :: [Picture]
v_linije_dole = take 2 $ map (color black . (\x -> line [(-ps,x*vp-pv),(4*vp-ps,x*vp-pv)])) [0,1..]
                   where vp = velicina_polja
                         ps = (fromIntegral sirina) / 2
                         pv = (fromIntegral visina) / 2

--uspravne linije resenja
u_linije_dole :: [Picture]
u_linije_dole = take 5 $ map (color black . (\x -> line [(x*vp-ps,-pv),(x*vp-ps,vp-pv)])) [0,1..]
                   where vp = velicina_polja
                         ps = (fromIntegral sirina) / 2
                         pv = (fromIntegral visina) / 2

--tabla na kojoj se nalaze simboli pomocu kojih igrac pogadja
simboli_1 :: [Picture] -> [Picture]
simboli_1 slike = map (\(x,y) -> translate (x*vp) ((-2.5)*vp) $ scale 0.125 0.125 y) $ zip [1..] $ take 3 slike
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

simboli_2 :: [Picture] -> [Picture]
simboli_2 slike = map (\(x,y) -> translate (x*vp) ((-3.5)*vp) $ scale 0.125 0.125 y) $ zip [1..] $ drop 3 slike
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--vodoravne linije table simbola
v_linije_simboli :: [Picture]
v_linije_simboli = take 3 $ map (color black . (\x -> line [(5*vp-ps,x*vp-pv),(8*vp-ps,x*vp-pv)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--uspravne linije table simbola
u_linije_simboli :: [Picture]
u_linije_simboli = take 4 $ map (color black . (\x -> line [((x+5)*vp-ps,-pv),((x+5)*vp-ps,2*vp-pv)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--(Int, Int) su indeksi polja u matrici table za pogadjanje
nacrtaj_polje :: [Picture] -> (Int, Int) -> VrednostPolja -> Picture
nacrtaj_polje _ (i,j) Nista = let x = fromIntegral j
                                  y = fromIntegral i
                                  vp = velicina_polja
                                  ps = (fromIntegral sirina) / 2
                                  pv = (fromIntegral visina) / 2
                              in nacrtaj_pravougaonik (x*vp-ps) (pv-y*vp) ((x+1)*vp-ps) (pv-(y+1)*vp)
                             
nacrtaj_polje slike (i,j) (Slika s) = let x = fromIntegral j
                                          y = fromIntegral i
                                          vp = velicina_polja
                                          ps = (fromIntegral sirina) / 2
                                          pv = (fromIntegral visina) / 2
                                      in translate ((x+0.5)*vp-ps) (pv-(y+0.5)*vp) $ scale 0.125 0.125 $ slike !! s

--(Int, Int) su indeksi polja u matrici table sa opisom pogotka
nacrtaj_polje_1 :: (Int, Int) -> Tacnost -> Picture
nacrtaj_polje_1 (i,j) Prazno = let x = fromIntegral j
                                   y = fromIntegral i
                                   vp = velicina_polja
                                   ps = (fromIntegral sirina) / 2
                                   pv = (fromIntegral visina) / 2
                               in nacrtaj_pravougaonik ((x+0.5)*vp) (pv-y*vp) ((x+1.5)*vp) (pv-(y+1)*vp)
                               
nacrtaj_polje_1 (i,j) q = let x = (fromIntegral j) + 1
                              y = fromIntegral i
                              vp = velicina_polja
                              ps = (fromIntegral sirina) / 2
                              pv = (fromIntegral visina) / 2
                          in translate (x*vp) (pv-(y+0.5)*vp) $ kvadratIKrug q

kvadratIKrug :: Tacnost -> Picture
kvadratIKrug Crveno = pictures $ [nacrtaj_pravougaonik (-pvp) (-pvp) pvp pvp, color red $ circleSolid pvp]
                        where pvp = velicina_polja / 2
kvadratIKrug Zuto = pictures $ [nacrtaj_pravougaonik (-pvp) (-pvp) pvp pvp, color yellow $ circleSolid pvp]
                        where pvp = velicina_polja / 2
