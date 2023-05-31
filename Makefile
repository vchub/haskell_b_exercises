.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build --enable-tests --write-ghc-environment-files=always

.PHONY: test-all
test-all:
	cabal test all --enable-tests --test-show-details=direct

.PHONY: test-lecture1
test-lecture1:
	cabal test doctest-lecture1 --enable-tests --test-show-details=direct
	cabal run exercises-test --enable-tests -- -m "Lecture 1"

.PHONY: test-lecture2
test-lecture2:
	cabal test doctest-lecture2 --enable-tests --test-show-details=direct
	cabal run exercises-test --enable-tests -- -m "Lecture 2"

.PHONY: test-lecture3
test-lecture3:
	cabal test doctest-lecture3 --enable-tests --test-show-details=direct
	cabal run exercises-test --enable-tests -- -m "Lecture 3"

.PHONY: test-lecture4
test-lecture4:
	cabal run exercises-test --enable-tests -- -m "Lecture 4"

.PHONY: ghcid-test
ghcid-test:
	ghcid -T="Test.Misc.main"
	# ghcid -c="stack ghci"  --test="Test.Misc.main"

.PHONY: quickcheck
quickcheck:
	ghcid -T="Test.QuickCheckTutor.main"

.PHONY: test-l1
test-l1:
	stack test --file-watch :exercises-test --test-arguments='-m "Lecture 1"'
	# stack test --file-watch :doctest-lecture1
	
.PHONY: current-test
current-test:
	ghcid -T="main"
