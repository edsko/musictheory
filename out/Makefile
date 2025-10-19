all: music-theory-exercises.pdf

music-theory-exercises.pdf: music-theory-exercises.ly
	lilypond music-theory-exercises.ly

# Declare as phony, depends on all Haskell files
.PHONY: music-theory-exercises.ly
music-theory-exercises.ly:
	cabal run -- music-theory-exercises -o .

.PHONY: clean
clean:
	rm -f *.ly *.pdf
