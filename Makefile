all:
	ghc -O2 -with-rtsopts="-K100m" hassuus.hs

prof:
	ghc -prof -fprof-auto -rtsopts hassuus.hs
