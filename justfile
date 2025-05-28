GHC_VERSION := "9.8.4"

# prepare the environment for the project
prepare: 
    cabal update
    gen-hie > hie.yaml
    haskell-ci regenerate

install: 
    cabal new-install exe:lineman --overwrite-policy=always

build: 
    cabal build

run: 
    cabal run lineman -- ./lineman.dhall

# update the bounds of dependencies
update:
    cabal-bounds update

