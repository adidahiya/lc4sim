all: Main

SRCS=Parser.hs ParserCombinators.hs Tests.hs immediate.hs lc4PP.hs lc4draw.hs lc4parser.hs lc4vm.hs simulator.hs vmLoader.hs Main.hs

GHC=ghc
GHC_FLAGS=--make

TARGET=lc4sim

Main: $(SRC)
	$(GHC) $(GHC_FLAGS) Main.hs -o $(TARGET)

clean : 
	rm -f *.o *.hi $(TARGET)
