#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
# XXX This should be matching up with $PREFIX in main
export PATH=~/local/bin:$PATH
export OPAMKEEPLOGS=1

if [[ $OPAM_COLD -eq 1 ]] ; then
  export PATH=$PWD/bootstrap/ocaml/bin:$PATH
fi

# Test basic actions
# The SHA is fixed so that upstream changes shouldn't affect CI. The SHA needs
# to be moved forwards when a new version of OCaml is added to ensure that the
# ocaml-system package is available at the correct version.
opam init --bare default git+$OPAM_REPO_CACHE#$OPAM_TEST_REPO_SHA
cat >> $(opam var root --global 2>/dev/null)/config <<EOF
archive-mirrors: "https://opam.ocaml.org/cache"
EOF
opam switch create default --empty --no-install
if [ "$(ocamlc -version)" = "5.4.0~beta1" ]; then
  OPAMEDITOR="sed -i.bak -e 's/\"5\\.3\\.0\"/\"5.4.0~beta1\"/' -e 's/{= \"5\\.4\\.0~beta1\"/{= \"5.4.0\"/' " opam pin edit -n ocaml-system
fi
opam switch set-invariant --formula '"ocaml-system"'
eval $(opam env)
opam install lwt
opam list
opam config report

# Make sure opam init (including the sandbox script) works in the absence of OPAMROOT
unset OPAMROOT
rm -r ~/.opam
opam init --bare -vvv
rm -r ~/.opam
