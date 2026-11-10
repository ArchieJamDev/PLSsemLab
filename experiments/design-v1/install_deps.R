# -----------------------------------------------------------------------------
# Installs what the experiments need. cSEM pulls in about 76 packages.
# ES: Instala lo que necesitan los experimentos. cSEM arrastra unos 76 paquetes.
# -----------------------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("cSEM", quietly = TRUE)) install.packages("cSEM")
cat("cSEM", as.character(packageVersion("cSEM")), "\n")
