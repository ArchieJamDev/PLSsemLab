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
    sz <- list(width = a$results$pathDiagram$width, height = a$results$pathDiagram$height)
    if (identical(o$diagramLayout, "treeTB"))
        sz <- list(width = max(560, min(1100, 62 * (diff(range(st$x_tb)) + 1.6) + 120)),
                   height = max(360, min(1100, 75 * (diff(range(st$y_tb)) + 1) + 170)))
    grDevices::png(file.path("/tmp/out", file), sz$width, sz$height, res = 96)
    f(".pl_plot_sem")(st, o, theme, width = sz$width, height = sz$height)
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
t3 <- cls$new(options = opts$new(reflective = list(list(label = "Trust", vars = paste0("trust_", 1:4)), list(label = "Loy", vars = paste0("loy_", 1:3))),
                                 formative = list(list(label = "Qual", vars = c("qual_1", "qual_2", "qual_4", "qual_5", "qual_6", "qual_7"))),
                                 structural = list(list("Qual"), list("Qual", "Trust"), list()), bootstrap = TRUE, bootReps = 100), data = d)
t3$init(); t3$run(); draw(t3, "diagram-trust-LR.png")
sat <- read.csv(sub("bank", "satisfaction", Sys.getenv("PL_DATA")))
hoc <- cls$new(options = opts$new(reflective = list(list(label = "SAT", vars = paste0("sat", 1:4)), list(label = "LOY", vars = paste0("loy", 1:4))),
                                  formative = list(list(label = "IMAG", vars = paste0("imag", 1:5)), list(label = "EXPE", vars = paste0("expe", 1:5))),
                                  reflectiveHoc = list(list(label = "QUAL", components = list("IMAG", "EXPE"))),
                                  structural = list(list("QUAL"), list("SAT", "QUAL"), list(), list(), list()),
                                  bootstrap = TRUE, bootReps = 100), data = sat)
hoc$init(); hoc$run(); draw(hoc, "diagram-hoc-LR.png")
# Archie's bank model: Superior (Qual + Trust) and Comp predict Loy
# ES: El modelo bancario de Archie: Superior (Qual + Trust) y Comp predicen Loy
bk <- cls$new(options = opts$new(reflective = list(list(label = "Trust", vars = paste0("trust_", 1:4)), list(label = "Loy", vars = paste0("loy_", 1:3)),
                                                    list(label = "Comp", vars = paste0("comp_", 1:3))),
                                 formative = list(list(label = "Qual", vars = c("qual_1", "qual_2", "qual_4", "qual_5", "qual_6", "qual_7"))),
                                 reflectiveHoc = list(list(label = "Superior", components = list("Qual", "Trust"))),
                                 structural = list(list(), list("Superior", "Comp"), list(), list(), list()),
                                 bootstrap = TRUE, bootReps = 100), data = d)
bk$init(); bk$run(); draw(bk, "diagram-bank-hoc-LR.png")
print(bk$results$hocHtmt$asDF)
it <- cls$new(options = opts$new(reflective = list(list(label = "IMAG", vars = paste0("imag", 1:5)), list(label = "SAT", vars = paste0("sat", 1:4)),
                                                    list(label = "LOY", vars = paste0("loy", 1:4))),
                                 structural = list(list(), list(), list("SAT", "IMAG", c("SAT", "IMAG"))),
                                 bootstrap = TRUE, bootReps = 100), data = sat)
it$init(); it$run(); draw(it, "diagram-interaction-LR.png")
grDevices::png("/tmp/out/moderation.png", 760, 360, res = 96)
print(f(".pl_plot_moderation")(it$results$moderationPlots$items[[1]]$state, ggplot2::theme_minimal(), theme)); grDevices::dev.off()
cat("diagrams ok\n")
cat("max RSS (MB):", as.numeric(sub("\\D+(\\d+).*", "\\1", system(paste("grep VmHWM /proc", Sys.getpid(), "status", sep = "/"), intern = TRUE))) / 1024, "\n")
