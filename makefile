SRC=Parser.hs 
SRC+=ParserCombinators.hs
SRC+=Tests.hs
SRC+=immediate.hs
SRC+=lc4PP.hs
SRC+=lc4draw.hs
SRC+=lc4parser.hs
SRC+=lc4vm.hs
SRC+=simulator.hs
SRC+=vmLoader.hs
SRC+=Main.hs

all: Main

TARGET=lc4sim

Main: $(SRC)
	ghc --make -package hcurses Main.hs -o $(TARGET)

clean : 
	rm -f *.o *.hi $(TARGET)
