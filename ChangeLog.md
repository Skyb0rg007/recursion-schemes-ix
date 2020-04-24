# Changelog for recursion-schemes-ix

## 0.0.1.0

Initial release

Multiple last-minute changes:

* Switched from `singletons` to `singlethongs`
  - `singletons` has unnecessary dependencies, and is unable to compile
    with the current `ghcjs` version.
* Removed QuantifiedConstraints extension usage throughout
  - Allowed the removal of UndecidableInstances in most files
* Added the `ITraversable` class and monadic recursion-schemes
  - TODO: determine if `IFunctor` should just be removed,
    ie. determine if there are any `IFunctor`s that are not `ITraversable`

