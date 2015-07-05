export GIT_COMMIT_HASH=$(git log --format=%H -n1)
runhaskell src/build/hs/CabalTemplate.hs
