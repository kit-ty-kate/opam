Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init

## Config report
  *

## Install
  *

## Remove
  *

## Switch

## Pin
  *

## List

## Show

## Var/Option

## Lint
  *

## Lock
  *

## Opamfile
  *

## External dependencies

## Sandbox

## Repository management
  *

## VCS
  *

## Build
  * Bump src_exts and fix build compat with Dune 2.9.0 [#4752 @dra27]
  * Upgrade to dose3 >= 6.1 and vendor dose3 7.0.0 [#4760 @kit-ty-kate]
  * Change minimum required OCaml to 4.03.0 [#4770 @dra27]
  * Change minimum required Dune to 2.0 [#4770 @dra27]
  * Change minimum required OCaml to 4.08.0 for everything except opam-core, opam-format and opam-installer [#4775 @dra27]
  * Fix the cold target in presence of an older OCaml compiler version on macOS [#4802 @kit-ty-kate - fix #4801]
  * Harden the check for a C++ compiler [#4776 @dra27 - fix #3843]
  * Add `--without-dune` to configure to force compiling vendored Dune [#4776 @dra27]
  * Use `--without-dune` in `make cold` to avoid picking up external Dune [#4776 @dra27 - fix #3987]
  * Add `--with-vendored-deps` to replace `make lib-ext` instruction [#4776 @dra27 - fix #4772]
  * Fix vendored build on mingw-w64 with g++ 11.2 [#4835 @dra27]
  * Switch to vendored build if spdx_licenses is missing [#4842 @dra27]
  * Check versions of findlib packages in configure [#4842 @dra27]
  * Fix dose3 download url since gforge is gone [#4870 @avsm]
  * Update bootstrap ocaml to 4.12.1 to integrate mingw fix [#4927 @rjbou]
  * Update bootstrap to use `-j` for Unix (Windows already does) [#4988 @dra27]
  * Update cold compiler to 4.13 [#5017 @dra27]
  * Bring the autogen script from ocaml/ocaml to be compatible with non-ubuntu-patched autoconf [#5090 @kit-ty-kate #5093 @dra27]
  * configure: Use gmake instead of make on Unix systems (fixes BSDs) [#5090 @kit-ty-kate]
  * Patch AltGr/ocaml-mccs#36 in the src_ext build to fix Cygwin32 [#5094 @dra27]
  * Silence warning 70 [#5104 @dra27]
  * Add `jsonm` (and `uutf`) dependency [#5098 @rjbou - fix #5085]
  * Bump opam-file-format to 2.1.4 [#5117 @kit-ty-kate - fix #5116]
  * Add `sha` dependency [#5042 @kit-ty-kate]
  * Add a 'test' target [#5129 @kit-ty-kate @mehdid - partially fixes #5058]
  * Add `with-tools` handling in selection process [#5016 @rjbou]
  * Bump cudf to 0.10 [#5195 @kit-ty-kate]
  * shell/bootstrap-ocaml.sh: do not fail if curl/wget is missing [#5223 #5233 @kit-ty-kate]
  * Upgrade to cmdliner >= 1.1 [#5269 @kit-ty-kate]
  * Cleared explanation of dependency vendoring in configure [#5277 @dra27 - fix #5271]
  * Switch autoconf required version to 2.71 [#5161 @dra27]
  * Remove src/client/no-git-version when calling make clean [#5290 @kit-ty-kate]
  * Update the bootstrap compiler to 4.14.0 [#5250 @kit-ty-kate]
  * Upgrade the vendored dune to 3.5.0 to fix make cold in an OCaml 5.0 env [#5355 @kit-ty-kate]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State

# Opam file format
  *

## Solver
  *

## Client

## Internal
  *

## Test
  *

## Reftests
### Tests
### Engine
  * Add switch-invariant test [#4866 @rjbou]
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Add & update env tests [#4861 #4841 @rjbou @dra27]
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
  * Add & update env tests [#4861 #4841 #4974 @rjbou @dra27 @AltGr]
  * Add remove test [#5004 @AltGr]
  * Add some simple tests for the "opam list" command [#5006 @kit-ty-kate]
  * Add clean test for untracked option [#4915 @rjbou]
  * Harmonise some repo hash to reduce opam repository checkout [#5031 @AltGr]
  * Add repo optim enable/disable test [#5015 @rjbou]
  * Update list with co-instabillity [#5024 @AltGr]
  * Add lint test [#4967 @rjbou]
  * Add lock test [#4963 @rjbou]
  * Add working dir/inplace/assume-built test [#5081 @rjbou]
  * Fix github url: `git://` form no more handled [#5097 @rjbou]
  * Add source test [#5101 @rjbou]
  * Add upgrade (and update) test [#5106 @rjbou]
  * Update var-option test with no switch examples [#5025]
  * Escape for cmdliner.1.1.1 output change [#5131 @rjbou]
  * Add deprectaed flag test [#4523 @kit-ty-kate]
  * Add deps-only, install formula [#4975 @AltGr]
  * Update opam root version test do escape `OPAMROOTVERSION` sed, it matches generated hexa temporary directory names [#5007 @AltGr]
  * Add json output test [#5143 @rjbou]
  * Add test for opam file write with format preserved bug in #4936, fixed in #4941 [#4159 @rjbou]
  * Add test for switch upgrade from 2.0 root, with pinned compiler [#5176 @rjbou @kit-ty-kate]
  * Add switch import (for pinned packages) test [#5181 @rjbou]
  * Add `--with-tools` test [#5160 @rjbou]
  * Add a series of reftests showing empty conflict messages [#5253 @kit-ty-kate]
### Engine
  * Add `opam-cat` to normalise opam file printing [#4763 @rjbou @dra27] [2.1.0~rc2 #4715]
  * Fix meld reftest: open only with failing ones [#4913 @rjbou]
  * Add `BASEDIR` to environement [#4913 @rjbou]
  * Replace opam bin path [#4913 @rjbou]
  * Add `grep -v` command [#4913 @rjbou]
  * Apply grep & seds on file order [#4913 @rjbou]
  * Precise `OPAMTMP` regexp, `hexa` instead of `'alphanum` to avoid confusion with `BASEDIR` [#4913 @rjbou]
  * Hackish way to have several replacement in a single line [#4913 @rjbou]
  * Substitution in regexp pattern (for environment variables) [#4913 @rjbou]
  * Substitution for opam-cat content [#4913 @rjbou]
  * Allow one char package name on repo [#4966 @AltGr]
  * Remove opam output beginning with `###` [#4966 @AltGr]
  * Add `<pin:path>` header to specify incomplete opam files to pin, it is updated from a template in reftest run (no lint errors) [#4966 @rjbou]
  * Unescape output [#4966 @rjbou]
  * Clean outputs from opam error reporting block [#4966 @rjbou]
  * Avoid diff when the repo is too old [#4979 @AltGr]
  * Escape regexps characters in string replacements primitives [#5009 @kit-ty-kate]
  * Automatically update default repo when adding a package file [#5004 @AltGr]
  * Make all the tests work on macOS/arm64 [#5019 @kit-ty-kate]
  * Add unix only tests handling [#5031 @AltGr]

## Github Actions
  * Add solver backends compile test [#4723 @rjbou] [2.1.0~rc2 #4720]
  * Fix ocaml link (http -> https) [#4729 @rjbou]
  * Separate code from install workflow [#4773 @rjbou]
  * Specify whitelist of changed files to launch workflow [#473 @rjbou]
  * Update changelog checker list [#4773 @rjbou]
  * Launch main hygiene job on configure/src_ext changes [#4773 @rjbou]
  * Add opam.ocaml.org cache to reach disappearing archive [#4865 @rjbou]
  * Update ocaml version frm 4.11.2 to  4.12.0 (because of macos failure) [#4865 @rjbou]
  * Add a depext checkup, launched only is `OpamSysInteract` is changed [#4788 @rjbou]
  * Arrange scripts directory [#4922 @rjbou]
  * Run ci on tests changes [#4966 @rjbou]

## Shell
  * fish: fix deprecated redirection syntax `^` [#4736 @vzaliva]

## Doc

## Security fixes
  *

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
