# Backport of `Foreign.C.ConstPtr`

This requires the following to be added to your `.cabal` file:

```cabal
if impl(ghc <9.6.1)
  ghc-options:
    -optc=-Wno-discarded-qualifiers
    -optc=-Wno-incompatible-pointer-types-discards-qualifiers
```
