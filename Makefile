all: DeepThought

DeepThought: DeepThought.hs
	ghc -o $@ --make $^
