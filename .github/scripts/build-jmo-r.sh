#!/bin/bash
# Builds the .jmo inside rocker/r-ver (called by build-jmo.yml): the jamovi
# compiler with a stand-in jamovi home, so the .jmo declares this R version;
# then cSEM 0.7.1 replaces the compiler snapshot's 0.6.1.
# ES: Construye el .jmo dentro de rocker/r-ver: el compilador de jamovi con
# un jamovi sustituto, así el .jmo declara esta versión de R; luego cSEM
# 0.7.1 reemplaza al 0.6.1 de la instantánea del compilador.
set -euo pipefail
# GitHub runners reach Ubuntu through Azure's mirror; archive.ubuntu.com
# did not resolve from the runners.
# ES: Los runners llegan a Ubuntu por el espejo de Azure.
sed -i 's#http://archive.ubuntu.com#http://azure.archive.ubuntu.com#g; s#http://security.ubuntu.com#http://azure.archive.ubuntu.com#g' \
    /etc/apt/sources.list.d/*.sources /etc/apt/sources.list 2>/dev/null || true
apt-get update -qq
apt-get install -y -qq git unzip zip nodejs npm libcurl4-openssl-dev libssl-dev libxml2-dev libglpk-dev libgmp3-dev >/dev/null
R --version | head -1
node --version
# Packages jamovi provides (not bundled in the .jmo, but needed to build it).
Rscript -e "install.packages(c('jmvcore', 'R6', 'lavaan', 'ggplot2', 'rlang'), repos = c('https://repo.jamovi.org', 'https://packagemanager.posit.co/cran/__linux__/noble/2025-05-25'))"
git clone -q --depth 1 https://github.com/jamovi/jamovi-compiler.git /tmp/jmc
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
unzip -p *.jmo PLSsemLab/jamovi.yaml | grep rVersion
unzip -p *.jmo PLSsemLab/R/cSEM/DESCRIPTION | grep "^Version"
chmod a+rw *.jmo
rm -rf build
