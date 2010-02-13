#!/bin/sh

PKGDIR=pkg.conf.d
hcpkg=ghc-pkg

if [ -d ${PKGDIR} ] ; then
    rm -rf ${PKGDIR}
fi

${hcpkg} init ${PKGDIR}

cabal clean
cabal configure --package-db ${PKGDIR} || exit 0
cabal build || exit 0
cabal register --inplace || exit 0

