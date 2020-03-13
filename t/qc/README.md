# Setup

* Move `Exception.hs` to `src/Parser/Generator/`;
* Move `QC.hs` to `src/`;
* Modify all generator modules in `src/Parser/Generator/` so that their
functions are total. See `Syntax.hs` in this directory for an example.

# Testing

* Load `QC.hs` using ghci and use the QuickCheck functions to test;
* There are two properties defined in `QC.hs`, and the needed auxiliary
functions.

# Dependencies

Originally, trying to use QuickCheck with `ghc-7.10.3` did not work. Testing
worked with a newer version, specifically `ghc-8.6.5`. Beware that your `ghc`
version may come into play.
