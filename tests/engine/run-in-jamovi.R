# -----------------------------------------------------------------------------
# Runs PLSsemLab inside jamovi's own R (docker image) with the libraries the
# .jmo ships, to reproduce engine crashes that a plain R session hides.
# ES: Ejecuta PLSsemLab dentro del R de jamovi (imagen docker) con las
# bibliotecas del .jmo, para reproducir caídas del motor que un R normal
# no muestra.
# -----------------------------------------------------------------------------
cat("R:", R.version.string, "\n"); print(.libPaths())
library(PLSsemLab)
for (p in c("cSEM", "jmvcore", "ggplot2", "rlang", "lavaan"))
    cat(p, as.character(utils::packageVersion(p)), find.package(p), "\n")
d <- read.csv(Sys.getenv("PL_DATA"))
cls <- get("plssemClass", envir = asNamespace("PLSsemLab"))
opts <- get("plssemOptions", envir = asNamespace("PLSsemLab"))
o <- opts$new(reflective = list(list(label = "Like", vars = c("like_1", "like_2")),
                                list(label = "Comp", vars = c("comp_1", "comp_2", "comp_3")),
                                list(label = "Loy", vars = c("loy_1", "loy_2", "loy_3"))),
              formative = list(list(label = "Qual", vars = c("qual_1", "qual_2", "qual_4", "qual_5", "qual_6", "qual_7"))),
              structural = list(list("Qual"), list(), list("Like"), list("Comp")),
              bootstrap = TRUE, bootReps = 100, loadingsPlot = TRUE, htmtPlot = TRUE)
a <- cls$new(options = o, data = d)
a$init(); cat("init ok\n")
a$run(); cat("run ok\n")
print(a$results$paths$asDF)
theme <- list(color = c("#333333", "#6B9DE8"), fill = c("#FFFFFF", "#E6E6E6"))
f <- function(n) get(n, envir = asNamespace("PLSsemLab"))
grDevices::png("/tmp/out/diagram.png", 800, 600)
print(f(".pl_plot_paths")(a$results$pathDiagram$state, ggplot2::theme_minimal(), theme)); grDevices::dev.off()
cat("diagram ok\n")
grDevices::png("/tmp/out/loadings.png", 800, 600)
print(f(".pl_plot_loadings")(a$results$loadingsPlot$state, ggplot2::theme_minimal(), theme)); grDevices::dev.off()
grDevices::png("/tmp/out/htmt.png", 800, 600)
print(f(".pl_plot_htmt")(a$results$htmtPlot$state, ggplot2::theme_minimal(), theme)); grDevices::dev.off()
cat("plots ok\n")
cat("max RSS (MB):", as.numeric(system(paste("grep VmHWM /proc", Sys.getpid(), "status", sep = "/"), intern = TRUE) |> sub(pattern = "\\D+(\\d+).*", replacement = "\\1")) / 1024, "\n")
