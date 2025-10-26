#!/bin/sh

set -euo pipefail

rm -rf *.ml *.mli dune

git clone https://github.com/dbuenzli/cmdliner tmp-vendor
git -C tmp-vendor switch --detach v1.3.0

mv tmp-vendor/src/*.{ml,mli} .
rm -rf tmp-vendor

mv cmdliner.ml opamCmdliner.ml
mv cmdliner.mli opamCmdliner.mli

cat > dune << EOF
(library
 (name opamCmdliner)
 (public_name opam-core.cmdliner))
EOF
