.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-fmt --inplace state-machine-testing-presentation.cabal 

.PHONY: hlint
hlint:
	hlint . 
