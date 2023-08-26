OPTIONS := -Wall

build: hpack
	cabal build --ghc-options='${OPTIONS}'

hpack:
	hpack .

test: hpack
	cabal test --ghc-options='${OPTIONS}'

format:
	alejandra --quiet .
	find src/ test/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

format-check:
	find src/ test/ -name "*.hs" -exec fourmolu -m check -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

ghcid: hpack
	ghcid -c "cabal repl --ghc-options='${OPTIONS}'"

ghcid-test: hpack
	ghcid -c "cabal repl snail-test --ghc-options='${OPTIONS}'"

clean:
	cabal clean

hlint:
	hlint .

# This will add version bounds to dependencies
bounds: hpack
	cabal gen-bounds

# Mostly documentation for future uploads
hackage: hpack
	cabal new-haddock --haddock-for-hackage snail
	cabal upload --publish -d dist-newstyle/snail-0.1.0.0-docs.tar.gz

.PHONY: build hpack test format format-check ghcid ghcid-test clean hlint bounds hackage
