all: encoder decoder
encoder:
	ghc --make encoder.hs

decoder:
	ghc --make decoder.hs

clean:
	rm -rf decoder encoder *.hi *.o
