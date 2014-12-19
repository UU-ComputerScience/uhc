# Changelog

## 1.1.8.2 - 20141219

- [build] Fixes some GHC 7.8.3 Typeable warnings
- [core, api] Removed broken default alternative from case expressions API

## 1.1.8.1 - 20141217

- [core] Core prettyprinting/parsing changed such that bindings (in a let) and alternatives (of a case) now are semicolon terminated instead of curly embraced semicolon separated; should improve readability.
- [build] Separate target in makefiles for 'ehcr', with trace option.
- [corerun, api] CoreRun API extended to be usable by CCO lab exercise.

## 1.1.8.0 - 20141212

- [corerun, api] CoreRun has its own API.
- [core, api] Interface to Core main changed in that the state parameter is already passed explicitly (not implicitly added during later compiler pipeline stages), influences the api.
- [corerun] Cleaned up CoreRun AST, spec of the operational semantics under development (separate outside src/doc tree).
- [feature] Separate executable 'uhcr' as part of uhc-light for running CoreRun file .rcr as pretty printed by dump, a temporary solution.
- [corerun] Parser for CoreRun.

## 1.1.7.4 - 20141203

- [core] Detection of yes/no ANormal form of parsed Core and corresponding no/yes transformation (not tested for cases where it matters; unknown whether this is good enough for codegen to work properly)
- [cleanup] Removal of src/ruler from source tree (has its own repo)
- [api] Added better documentation and one small example program
- [api] Fix issue #36 (wrong runtime behaviour and/or crashes)

## 1.1.7.3 - 20141127

- [core] Core running engine works (FFI stuff still to be done).
- [api] Core.API made more consistent.

## 1.1.7.2 - 20141126

- [core] Core allows exports, can mix .hs and .bcr/.tcr files (tested rudimentary only).
- [api] Core.API extended.

## 1.1.7.1 - 20141119

- [api] Initial Core.API ([#31](https://github.com/UU-ComputerScience/uhc/pull/31)).
- [feature] Start with changelog.

## 1.1.7.0 - 20141118

- [feature] Initial cabal/hackage distribution tryout.
