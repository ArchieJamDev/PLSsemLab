# -----------------------------------------------------------------------------
# PLSsemLab — design experiments
# Runs every experiment and writes its console output to results/.
# Usage (from this folder): Rscript run_all.R            # all
#                           Rscript run_all.R E1 E5       # some
# ES: Ejecuta cada experimento y guarda su salida de consola en results/.
# ES: Uso (desde esta carpeta): Rscript run_all.R  |  Rscript run_all.R E1 E5
# -----------------------------------------------------------------------------
scripts <- sort(list.files(".", pattern = "^E[0-9]+_.*\\.R$"))
wanted  <- commandArgs(trailingOnly = TRUE)
if (length(wanted)) scripts <- scripts[sub("_.*", "", scripts) %in% wanted]
dir.create("results", showWarnings = FALSE)

status <- data.frame(script = scripts, exit = NA_integer_, seconds = NA_real_)
for (i in seq_along(scripts)) {
  out <- file.path("results", sub("\\.R$", ".txt", scripts[i]))
  t0 <- Sys.time()
  # Each script runs in a fresh R process, as a jamovi analysis would.
  # ES: Cada script corre en un proceso R nuevo, como un análisis de jamovi.
  status$exit[i] <- system2(file.path(R.home("bin"), "Rscript"), scripts[i],
                            stdout = out, stderr = out)
  status$seconds[i] <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("%-40s exit %d  %6.1f s\n", scripts[i], status$exit[i], status$seconds[i]))
}

# Session record: versions used for these results.
# ES: Registro de sesión: versiones usadas para estos resultados.
writeLines(c(sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
             sprintf("R: %s", R.version.string),
             sprintf("cSEM: %s", as.character(packageVersion("cSEM"))),
             sprintf("lavaan: %s", as.character(packageVersion("lavaan"))),
             sprintf("Platform: %s", R.version$platform)),
           file.path("results", "session.txt"))
if (any(status$exit != 0)) quit(status = 1)
