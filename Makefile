all: encoder decoder static_encoder
static_encoder:
	ghc --make static_encoder.hs
encoder:
	ghc --make encoder.hs

decoder:
	ghc --make decoder.hs

clean:
	rm -rf decoder encoder *.hi *.o
