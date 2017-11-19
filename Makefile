BNFC_SRC = src/grammar.cf

ALL_FILES = src/MilMonad.hs src/LexGrammar.hs src/ParGrammar.hs src/AbsGrammar.hs src/ErrM.hs
GHC_FLAGS = -fno-warn-tabs -fno-warn-warnings-deprecations

all:
	ghc $(GHC_FLAGS) src/Interpreter.hs $(ALL_FILES) -o interpreter

clean:
	-rm interpreter src/*.o src/*.hi

