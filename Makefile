all: DeepThought

rebuild: clean all

DeepThought: DeepThought.hs DataAcquisitor.hs InfiniteImprobabilityDrive.hs
	ghc -o $@ --make $<

clean:
	rm -f *.o *.hi DeepThought


.PHONY: all clean rebuild
