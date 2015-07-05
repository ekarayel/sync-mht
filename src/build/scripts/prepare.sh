export GIT_DESCRIBE=$(git describe --always)
runhaskell src/build/hs/CabalTemplate.hs
