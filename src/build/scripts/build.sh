export GIT_COMMIT_HASH=$(git log --format=%H -n1)
export GIT_DESCRIBE=$(git describe --always)
runhaskell src/build/hs/CabalTemplate.hs
cabal update
cabal install


