Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Init
  *

## Config Upgrade
  *

## Install
  * Only track `.install` content when install field is empty instead of whole switch directory, makes install faster [#4494 @kit-ty-kate @rjbou - fix #4422]

## Remove
  *

## Switch
  *

## Pin
  *

## List
  *

## Show
  *

## Var
  *

## Option
  *

## Lint
  *

## Lock
  *

## Opamfile
  * Fix `features` parser [#4507 @rjbou]

## External dependencies

## Sandbox
  *

## Repository management
  *

## VCS
  *

## Build
  * Fix opam-devel's tests on platforms without openssl, GNU-diff and a system-wide ocaml [#4500 @kit-ty-kate]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State
  *

# Opam file format
  *

## Solver
  * Add bultin support for the 'deprecated' flag.
    Any packages flagged with deprecated would be avoided by the solver unless there is no other choice (e.g. some user wants to install package a which depends on b which is deprecated)
    If it is installed, show up a note after installation notifying the user that the package is deprecated.
    [#4523 @kit-ty-kate]

## Client
  *

## Internal
  * Be more robust w.r.t. new caches updates [#4429 @AltGr - fix #4354]
  * ActionGraph: removal postponing, protect against addition of cycles [#4358 @AltGr - fix #4357]
  * Initialise random [#4391 @rjbou]
  * Fix CLI debug log printed without taking into account debug sections [#4391 @rjbou]
  * Internal caches: use size checks from Marshal [#4430 @AltGr]
  * openssl invocation: Fix permission denied fallback [#4449 @Blaisorblade - fix #4448]
  * If all action error are fetching failure, return code 31 (`Sync_error`) instead of code 40 (`Package_operation_error`) [#4416 @rjbou - fix #4214]
  * Add debug & verbose log for patch & subst application [#4464 @rjbou - fix #4453]
  * Be more robust w.r.t. new caches updates when `--read-only` is not used [#4467 @AltGr - fix #4354]

## Test
  *

## Shell
  *

## Doc
  * Install page: add OSX arm64 [#4506 @eth-arm]
  * Document the default build environment variables [#4496 @kit-ty-kate]
