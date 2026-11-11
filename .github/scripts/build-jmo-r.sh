#!/bin/bash
# Builds the .jmo inside rocker/r-ver without apt (the Ubuntu archives were
# unreachable from the runners): Node from nodejs.org, the jamovi compiler
# from GitHub, a stand-in jamovi home so the .jmo declares this R version,
# then cSEM 0.7.1 replaces the compiler snapshot's 0.6.1.
# ES: Construye el .jmo dentro de rocker/r-ver sin apt (los archivos de
# Ubuntu no respondían desde los runners).
set -euo pipefail
R --version | head -1
which gcc g++ gfortran make || true
curl -fsSL https://nodejs.org/dist/v20.18.1/node-v20.18.1-linux-x64.tar.xz | tar -xJ -C /opt
export PATH=/opt/node-v20.18.1-linux-x64/bin:$PATH
node --version
Rscript -e "install.packages(c('jmvcore', 'R6', 'lavaan', 'ggplot2', 'rlang'), repos = c('https://repo.jamovi.org', 'https://packagemanager.posit.co/cran/__linux__/noble/2025-05-25'))"
mkdir -p /tmp/jmc && curl -fsSL https://codeload.github.com/jamovi/jamovi-compiler/tar.gz/refs/heads/main | tar -xz -C /tmp/jmc --strip-components=1
(cd /tmp/jmc && npm install --omit=dev --silent)
mkdir -p /tmp/fakejam/bin
printf '#!/bin/sh\necho %s\n' "$APP_VERSION" > /tmp/fakejam/bin/jamovi
chmod +x /tmp/fakejam/bin/jamovi
RHOME=$(R RHOME)
RLIB=$(Rscript -e "cat(.libPaths()[1])")
build() { node /tmp/jmc/index.js --build . "$@" --home /tmp/fakejam --rhome "$RHOME" --rlibs "$RLIB" --assume-app-version "$APP_VERSION"; }
build
Rscript -e "install.packages('cSEM', lib = 'build/R', repos = 'https://cloud.r-project.org', dependencies = FALSE)"
Rscript -e ".libPaths(c('build/R', .libPaths())); stopifnot(packageVersion('cSEM') >= '0.7.1'); loadNamespace('cSEM'); cat('cSEM', format(packageVersion('cSEM')), 'loads\n')"
rm -f *.jmo
build --skip-deps
Rscript -e "f <- Sys.glob('*.jmo'); d <- tempfile(); utils::unzip(f, exdir = d); cat(grep('rVersion', readLines(file.path(d, 'PLSsemLab/jamovi.yaml')), value = TRUE), '\n'); cat(read.dcf(file.path(d, 'PLSsemLab/R/cSEM/DESCRIPTION'))[, 'Version'], '\n')"
chmod a+rw *.jmo
rm -rf build
