.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-fmt --inplace state-machine-testing-presentation.cabal 

.PHONY: hlint
hlint:
	hlint . 

.PHONY: ide
ide:
	ghcid -c'stack repl --main-is state-machine-testing-presentation:test:state-machine-testing-presentation-test state-machine-testing-presentation:lib state-machine-testing-presentation:exe:app-server'

.PHONY: slides
slides:
	pandoc -t beamer slides.md -o slides.pdf
