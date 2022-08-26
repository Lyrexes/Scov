# Scov a Simple Chess Opening Viewer
A Bitboard Chess openign viewer written in Haskell

## Usage
```
scov [Option] [Opening]
 --search, -s [Opening name]
 --search, -s [Number of seach results] [Opening]
 --view, -v [Opening name]
 --view, -v [Opening ID]
 --view-with-size, -vws [Opening name] [window size]
 --view-with-size, -vws [Opening ID] [window size]
 --help, -h
 press LeftArrow for previous move
 press RightArrow for next move
 press H for previous move
 press L for next move
```


## Build
```
cabal install --lib sdl2
cabal install --lib sdl2-ttf
cabal install --lib aeson
cabal install --lib bytestring
cabal install --lib utf8-string
cabal install --lib linear
cabal install --lib vector
ghc --make scov
```
