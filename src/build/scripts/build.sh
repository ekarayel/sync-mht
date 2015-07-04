export GIT_COMMIT_HASH=$(git log --format=%H -n1)
export GIT_DESCRIBE=$(git describe)
runhaskell src/build/hs/CabalTemplate.hs
cabal update
cabal install
