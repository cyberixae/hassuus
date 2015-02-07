all:
	ghc -O2 -with-rtsopts="-K100m" wunderdog.hs

prof:
	ghc -prof -fprof-auto -rtsopts wunderdog.hs
