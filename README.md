# SimpleCheck

Not usable. This is just a playground for me to experiment with the shrinking
techniques described
[here](http://projects.haskell.org/pipermail/quickcheck/2013-November/000115.html)
and
[here](http://www.haskell.org/pipermail/libraries/2013-November/021674.html).
If it's successful, I'll try and get these changes merged into the canonical
Haskell QuickCheck.

## Installation

```bash
cabal sandbox init
cabal install --only-dependencies -j --enable-tests
cabal build && cabal test
```
