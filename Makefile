all: RobotFighter

RobotFighter: RobotFighter.hs
	ghc -o $@ --make $^
