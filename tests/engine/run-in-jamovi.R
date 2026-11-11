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
              bootstrap = TRUE, bootReps = 100)
a <- cls$new(options = o, data = d)
a$init(); cat("init ok\n")
a$run(); cat("run ok\n")
print(a$results$paths$asDF)
theme <- list(color = c("#333333", "#6B9DE8"), fill = c("#FFFFFF", "#E6E6E6"))
f <- function(n) get(n, envir = asNamespace("PLSsemLab"))
cat("semPlot", format(utils::packageVersion("semPlot")), find.package("semPlot"), "\n")
draw <- function(a, file, ...) {
    st <- a$results$pathDiagram$state
    o <- utils::modifyList(list(diagramLayout = "treeLR", edgeLabels = TRUE, diagramCaption = TRUE, diagramTitle = ""), list(...))
    sz <- a$results$pathDiagram
    grDevices::png(file.path("/tmp/out", file), sz$width, sz$height, res = 96)
    f(".pl_plot_sem")(st, o, theme)
    grDevices::dev.off()
    cat(file, sz$width, "x", sz$height, "\n")
}
draw(a, "diagram-user-LR.png")
draw(a, "diagram-user-TB.png", diagramLayout = "treeTB")
b <- cls$new(options = opts$new(reflective = o$reflective, formative = o$formative, structural = o$structural,
                                diagramIndicators = FALSE, bootstrap = TRUE, bootReps = 100), data = d)
b$init(); b$run(); draw(b, "diagram-user-noind.png")
cr <- read.csv(Sys.getenv("PL_DATA2"))
for (v in names(cr)) cr[[v]][cr[[v]] == -99] <- NA
ext <- opts$new(reflective = list(list(label = "COMP", vars = paste0("comp_", 1:3)), list(label = "LIKE", vars = paste0("like_", 1:3)),
                                  list(label = "CUSA", vars = "cusa"), list(label = "CUSL", vars = paste0("cusl_", 1:3))),
                formative = list(list(label = "QUAL", vars = paste0("qual_", 1:8)), list(label = "PERF", vars = paste0("perf_", 1:5)),
                                 list(label = "CSOR", vars = paste0("csor_", 1:5)), list(label = "ATTR", vars = paste0("attr_", 1:3))),
                structural = list(list("QUAL", "PERF", "CSOR", "ATTR"), list("QUAL", "PERF", "CSOR", "ATTR"), list("COMP", "LIKE"),
                                  list("COMP", "LIKE", "CUSA"), list(), list(), list(), list()),
                missing = "mean", bootstrap = TRUE, bootReps = 100)
e <- cls$new(options = ext, data = cr); e$init(); e$run()
draw(e, "diagram-ext-LR.png")
draw(e, "diagram-ext-TB.png", diagramLayout = "treeTB")
draw(e, "diagram-ext-nolabels.png", edgeLabels = FALSE, edgeWidth = "uniform")
cat("diagrams ok\n")
cat("max RSS (MB):", as.numeric(sub("\\D+(\\d+).*", "\\1", system(paste("grep VmHWM /proc", Sys.getpid(), "status", sep = "/"), intern = TRUE))) / 1024, "\n")
