#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b

# All environment variable are overwritten in job description
# One cache per solver, $CACHE/opam.<solver>.cached
export OPAMROOT=$OPAMBSROOT
echo $OPAMROOT

case "$SOLVER" in
  z3)
    PKGS=$SOLVER
    if [[ $RUNNER_OS = 'macOS' ]]; then
      # brew may require extra flags to override the system-installed python,
      # so we assume the presence of python3 on the macOS runners.
      opam option --global 'depext-bypass=["python@3.9"]'
    fi
    ;;
  0install)
    PKGS="$SOLVER opam-0install-cudf"
    ;;
  *)
    echo -e "\e[31mSolver $SOLVER not handled\e[0m";
    exit 3
    ;;
esac

opam update --depexts
opam switch create $SOLVER --empty --no-install
if [ "$(ocamlc -version)" = "5.4.0~beta1" ]; then
  OPAMEDITOR="sed -i.bak -e 's/\"5\\.3\\.0\"/\"5.4.0~beta1\"/' -e 's/{= \"5\\.4\\.0~beta1\"/{= \"5.4.0\"/' " opam pin edit -n ocaml-system
fi
opam switch set-invariant --formula '"ocaml-system"'
opam upgrade --all
opam install $PKGS
opam install . --deps
opam clean --logs --switch-cleanup
eval $(opam env)
./configure
make
