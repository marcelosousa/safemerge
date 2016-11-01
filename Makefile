CABAL-CONFIGURE-FLAGS 	:= --user
CABAL-BUILD-FLAGS     	:=
VERSION					:= 0.10.2

all : haskell 

haskell :
	cd dependencies/language-java-0.2.7 && cabal install
	cabal install

doc :
	cabal haddock --executables --haddock-options=--odir=doc/haddock
  
dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist
