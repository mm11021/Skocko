# Skočko

Veb lokacija projekta: https://github.com/mm11021/Skocko

Neophodni alati i paketi: Za kompilaciju projekta neophodan je prevodilac GHC za programski jezik Haskel, kao i biblioteke "gloss" (najmanje verzija 1.8), "time" (najmanje verzija 1.4) i "random" (najmanje verzija 1.1). Pored toga, neophodna je i biblioteka "freeglut" za programski jezik C. Za preuzimanje biblioteka za Haskel potreban je alat "cabal" i preuzimanje se vrši naredbom "cabal install <ime_biblioteke>". Njime se takođe vrše kompilacija i pokretanje.

Kompilacija i pokretanje projekta: Kompilacija i kreiranje izvršne datoteke vrši se naredbom "cabal build", a pokretanje sa "cabal run". Naredba "cabal run" će automatski izvršiti ponovnu kompilaciju i kreiranje izvršne datoteke ukoliko primeti promenu u izvornom kodu ili nedostatak izvršne datoteke.

Opis projekta: Projekat predstavlja igru Skočko iz kviza TV Slagalica, implementiranu u programskom jeziku Haskel. Igrač pogađa kombinaciju od 4 simbola koja se nasumično bira pri pokretanju igre. Pogađanje se vrši klikom miša na odgovarajuće simbole u donjem desnom uglu prozora. Ukoliko pogreši, može pritiskom na taster "Backspace" poništiti poslednji odabrani simbol. Nakon unetog četvrtog simbola, onemogućava se poništavanje poslednjeg unetog simbola i proverava se koliko se simbola u odabranoj kombinaciji nalazi u rešenju na istom mestu, a koliko ih se uopšte nalazi u rešenju i prikazuje informacije na tabli na desnoj strani prozora. Crveni krug označava da se neki od simbola nalazi u rešenju na istom mestu, a žuti krug da se neki od simbola nalazi u rešenju, ali ne na odgovarajućem mestu. Uvek se prvo postavljaju crveni pa žuti krugovi. Igrač ima 6 pokušaja da pogodi tačnu kombinaciju. Ukoliko u tome uspe (može i u manje od 6 poteza), igra je gotova i igrač je pobedio, a ukoliko ne uspe, igra je gotova i igrač je izgubio. U slučaju pobede videće 4 crvena kruga, a u slučaju poraza videće rešenje u donjem levom uglu prozora. Iz igre se izlazi klikom na dugme "Esc".

Izvršna datoteka napravljena je za operativni sistem Arč Linuks (Arch Linux), 64-bit.

Autori:
1. Dimitrije Špadijer, 398/2016, mm11021@alas.matf.bg.ac.rs
2. Selena Vukadinović, 88/2015, selenakv96@gmail.com
