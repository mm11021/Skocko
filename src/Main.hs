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

nacrtaj_polje :: Float -> Float -> Float -> Float -> Picture
nacrtaj_polje x0 y0 x1 y1 = polygon [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]

tabla :: [Picture]
tabla = map (\(x,y) -> nacrtaj_polje ((x*vp)-ps) (pv-(y*vp)) ((x+1)*vp-ps) (pv-(y+1)*vp)) [(i,j)| i<-[0,1..3], j<-[0,1..5]]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

v_linije :: [Picture]
v_linije = map (color white . (\x -> line [(-ps,pv-x*vp),(4*vp-ps,pv-x*vp)])) [0,1..6]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2

u_linije :: [Picture]
u_linije = take 5 $ map (color white . (\x -> line [(x*vp-ps,pv),(x*vp-ps,pv-6*vp)])) [0,1..]
                  where vp = velicina_polja
                        ps = (fromIntegral sirina) / 2
                        pv = (fromIntegral visina) / 2



main :: IO ()
main = display prozor boja $ pictures (tabla ++ v_linije ++ u_linije)
