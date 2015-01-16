#!/usr/bin/env bash

green='\e[0;32m'
red='\e[0;31m'
nc='\e[0m' # No Color

function step {
  echo -e "${green}$1 ...${nc}"
  bash /dev/stdin || exit 1
}

function step_suppress {
  echo -ne "${green}$1 ... ${nc}"
  tmp=$(mktemp)
  bash /dev/stdin &> $tmp && echo -e "${green}Done${nc}" || (
    echo -e "${red}Failed${nc}"
    echo "Output: "
    cat $tmp
    exit 1
  )
}

step "Configuring project" << 'EOF'
  tmp=$(mktemp)
  cabal configure --enable-tests --enable-benchmarks $CABALFLAGS -v2 --ghc-options="-Wall -Werror" &> $tmp || (
    cat $tmp
    exit 1
  )
  echo "Using packages: "
  sed -nre 's/Dependency ([^ ]*) ==([^ :]*).*/\1 \2/p' $tmp | column -t | sed -e "s/^/  /"
EOF

step "Building project" << EOF
  cabal build
EOF

step "Running tests" << EOF
  head -n1 dist/setup-config
  cabal test
EOF

step "Creating source distribution" << EOF
  cabal check
  cabal sdist # tests that a source-distribution can be generated
EOF

step_suppress "Checking source distribution" << 'EOF'
  # The following scriptlet checks that the resulting source distribution can be built & installed
  SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}')
  cd dist/
  if [ -f "$SRC_TGZ" ]; then
    cabal install "$SRC_TGZ"
  else
    echo "expected '$SRC_TGZ' not found"
    exit 1
  fi
EOF
