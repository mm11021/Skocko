module Main where

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
boja = white

--funkcija koja crta pravouganik cija su dijagonalna temena (x0,y0) i (x1,y1)
nacrtaj_pravougaonik :: Float -> Float -> Float -> Float -> Picture
nacrtaj_pravougaonik x0 y0 x1 y1 = polygon [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]

--tabla na kojoj se nalaze kombinacije koje igrac pogadja
tabla :: Picture
tabla = nacrtaj_pravougaonik (-ps) pv (4*vp-ps) (pv-6*vp)
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--vodoravne linije table
v_linije :: [Picture]
v_linije = take 7 $ map (color white . (\x -> line [(-ps,pv-x*vp),(4*vp-ps,pv-x*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--uspravne linije table
u_linije :: [Picture]
u_linije = take 5 $ map (color white . (\x -> line [(x*vp-ps,pv),(x*vp-ps,pv-6*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--tabla na kojoj se nalaze simboli pomocu kojih igrac pogadja
simboli :: Picture
simboli = nacrtaj_pravougaonik (5*vp-ps) (2*vp-pv) (8*vp-ps) (-pv)
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--vodoravne linije table simbola
v_linije_simboli :: [Picture]
v_linije_simboli = take 3 $ map (color white . (\x -> line [(5*vp-ps,x*vp-pv),(8*vp-ps,x*vp-pv)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

--uspravne linije table simbola
u_linije_simboli :: [Picture]
u_linije_simboli = take 4 $ map (color white . (\x -> line [((x+5)*vp-ps,-pv),((x+5)*vp-ps,2*vp-pv)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

sva_polja :: Picture
sva_polja = pictures $ tabla : simboli: v_linije ++ u_linije ++ v_linije_simboli ++ u_linije_simboli

main :: IO ()
main = display prozor boja sva_polja
