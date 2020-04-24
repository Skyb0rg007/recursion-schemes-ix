
# recursion-schemes-ix

## Recursion schemes over indexed datatypes

Heavily inspired by `multirec`, this library uses `PolyKinds`
and `SingI` constraints to achieve the same dependent typing as `multirec`.

This library defines the `IFunctor` typeclass and recursion-schemes
on `IFunctor`s.

Goals:
* Minimal dependencies
  - This library depends only on `singlethongs`, which in turn only depends
    on `template-haskell`. As a result, it should be easy to include in
    any project
* GHCJS compatibility
  - This package is used in a compiler that I want to be able to run in the
    browser
* Ease of use
  - Once everything is set-up, writing a recursion-scheme should not be
    overly complex (looking at you, `mulrirec`)

Non-Goals:
* Eliminating boilerplate
  - This library requires hand-written `IShow`, `IRead`, `IEq`, `IOrd`, `ITraversable` instances.
    I also recommend writing pattern synonyms to remove the `IFix` constructor.
    Writing these can be a pain, but its only required once for the main data structure,
    and one `IFunctor` instance whenever you write a dependent recursion-scheme.
* Speed
  - The code in the library mimics the `recurion-schemes` library almost directly,
    simply lifting everything up from `*` to `k -> *`. In a future release,
    `INLINE` pragmas will be added and possibly benchmarked.

## Documentation

Not written, but haddocks on the github user site.

