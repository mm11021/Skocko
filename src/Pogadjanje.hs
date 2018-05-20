module Pogadjanje where

import Prozor
import Igra
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe

resenje :: [VrednostPolja]
resenje = [Slika 3, Slika 0, Slika 0, Slika 5]

--funkcija koja reaguje na dogadjaj (samo na klik misem)
dogadjaj :: Event -> StanjeIgre -> IO StanjeIgre

--reakcija na levi klik
dogadjaj (EventKey (MouseButton LeftButton) Down _ (x,y)) igra = sledeceStanje x y igra

--na ostale dogadjaje ne reaguje
dogadjaj _ igra = return igra

sledeceStanje :: Float -> Float -> StanjeIgre -> IO StanjeIgre
sledeceStanje x y igra = klikniPolje (floor (x/vp-0.5)) (floor ((-y)/vp-2)) igra
                              where vp = velicina_polja

klikniPolje :: Int -> Int -> StanjeIgre -> IO StanjeIgre
klikniPolje x y igra@(Igra (stanje,broj)) = if (elem x [0,1,2]) && (elem y [0,1])
                                              then let pre = take broj stanje
                                                       posle = drop (broj+1) stanje
                                                       novoStanje = Igra ((pre ++ (Slika (3*y+x)) : posle),broj+1) 
                                                   in proveri novoStanje
                                              else return igra

klikniPolje _ _ x = return x

proveri :: StanjeIgre -> IO StanjeIgre
proveri igra@(Igra (stanje,broj)) = if broj `mod` 4 == 0
	                                  then let kombinacija = take 4 $ drop (broj-4) stanje
	                                           tacnost = uporedi kombinacija resenje
	                                       in if tacnost == (take 4 $ repeat Crveno)
	                                       	    then do
	                                       	    	   putStrLn "Pobeda! :)"
	                                       	    	   print tacnost
	                                       	    	   return $ Pobeda stanje
	                                       	    else if broj == 24
	                                       	    	   then do
	                                       	    	   	      putStrLn "Poraz! :("
	                                       	    	   	      print tacnost
	                                       	    	   	      return $ Poraz stanje
	                                       	    	   else do
	                                       	    	   	      print tacnost
	                                       	    	   	      return igra
	                                  else return igra
proveri x = return x

-- mozak igre
uporedi :: [VrednostPolja] -> [VrednostPolja] -> [Tacnost]
uporedi kombinacija resenje = let uporediNaMestu = map (\(x,y) -> if x==y then Crveno else (T x)) $ zip kombinacija resenje
                                  uporediNaMestuResenje = map (\(x,y) -> if x==y then Crveno else (T y)) $ zip kombinacija resenje
                              in fst $ foldl f (uporediNaMestu,uporediNaMestuResenje) [0,1..3]
                                 -- foldl nam simulura rad for petlje; i za for se uzima iz liste [0,1..3] koja se folduje, a akcije obavlja f
                                 -- treba nam ovako jer se kod provere za Zuto i Prazno menjaju liste za kombinaciju i resenje
                                 where f :: ([Tacnost],[Tacnost]) -> Int -> ([Tacnost],[Tacnost])
                                       f (l1,l2) pozicija = let element = l1 !! pozicija
                                                                indeks = elemIndex element l2
                                                                pre = take pozicija l1
                                                            	posle = drop (pozicija+1) l1
                                                            in if element == Crveno
                                                            	  then (l1,l2)
                                                            	  else if isNothing indeks
                                                            	         then (pre ++ Prazno : posle,l2)
                                                            	         else if pozicija == (fromJust indeks)
                                                            	  	            then (l1,l2)
                                                            	                else let i = fromJust indeks
                                                            	                         prel2 = take i l2
                                                            	                         poslel2 = drop (i+1) l2
                                                            	                         l1' = pre ++ Zuto : posle
                                                            	                         l2' = prel2 ++ Zuto : poslel2
                                                            	                     in (l1',l2')