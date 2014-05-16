all: DeepThought

rebuild: clean all

DeepThought: DeepThought.hs DataAcquisitor.hs InfiniteImprobabilityDrive.hs
	ghc -O2 -Wall -o $@ --make $<

clean:
	rm -f *.o *.hi DeepThought

submit:
	rm -rf ra115966 ra115966.zip
	mkdir ra115966
	cp *.hs Makefile nome_do_jogador ra115966
	zip -9r ra115966.zip ra115966

.PHONY: all clean rebuild
