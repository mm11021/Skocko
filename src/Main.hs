module Main where

import Igra
import Graphics.Gloss

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

--funkcija koja crta pravouganik cija su dijagonalna temena (x0,y0) i (x1,y1)
nacrtaj_pravougaonik :: Float -> Float -> Float -> Float -> Picture
nacrtaj_pravougaonik x0 y0 x1 y1 = color white $ polygon [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]

--vodoravne linije table
v_linije :: [Picture]
v_linije = take 7 $ map (color black . (\x -> line [(-ps,pv-x*vp),(4*vp-ps,pv-x*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--uspravne linije table
u_linije :: [Picture]
u_linije = take 5 $ map (color black . (\x -> line [(x*vp-ps,pv),(x*vp-ps,pv-6*vp)])) [0,1..]
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

sva_polja :: [Picture]
sva_polja = v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli

slike_putanje :: [FilePath]
slike_putanje = map (\a -> a ++ ".bmp") ["srce", "romb", "skocko", "detelina", "list", "zvezda"]

pocetni_svet :: [VrednostPolja]
pocetni_svet = take 24 $ repeat Nista

--(Int, Int) su indeksi polja u matrici table za pogadjanje
bla :: [Picture] -> (Int, Int) -> VrednostPolja -> Picture
bla _ (i,j) Nista = let x = fromIntegral i
                        y = fromIntegral j
                        vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2
                     in nacrtaj_pravougaonik ((x*vp-ps)) (pv-(y*vp)) ((x+1)*vp-ps) (pv-(y+1)*vp)
                               
bla slike (i,j) (Slika s) = let x = fromIntegral i
                                y = fromIntegral j
                                vp = velicina_polja
                                ps = (fromIntegral sirina) / 2
                                pv = (fromIntegral visina) / 2
                            in translate ((x*vp)-ps) (pv-(y+0.5)*vp) $ scale 0.125 0.125 $ slike!!s

pocetna_slika :: [Picture] -> [Picture]
pocetna_slika slike = map (\((a,b),polje) -> bla slike (a,b) polje) $ zip [(i,j)| i<-[0,1..3], j<-[0,1..5]] pocetni_svet

render :: [Picture] -> Picture
render slike = pictures $ (pocetna_slika slike) ++ (simboli_1 slike) ++ (simboli_2 slike) ++ sva_polja

main :: IO ()
main = do
         slike <- mapM loadBMP slike_putanje
         display prozor boja $ render slike
