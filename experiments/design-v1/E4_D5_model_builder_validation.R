# -----------------------------------------------------------------------------
# E4 — D5: from the options panel to a cSEM model, with clear messages.
# User reports in JASP: forum 2023 (uninformative errors on wrong syntax),
# issue #2955 (help file). PLSsemLab builds the model from boxes, like
# EGAnetLab's "Theoretical structure" and SEMLj's latent boxes, so the user
# never types syntax; the module validates the boxes before calling cSEM.
# Questions: (1) what does cSEM say on its own for each common mistake?
# (2) does a builder + validator catch every one with a teaching message?
# (3) do names with spaces/accents survive (EGAnetLab 0.1.0 lesson)?
# ES: E4 — D5: del panel de opciones a un modelo cSEM, con mensajes claros.
# ES: Preguntas: (1) ¿qué dice cSEM por sí solo ante cada error común?
# (2) ¿un constructor + validador detecta cada uno con un mensaje didáctico?
# (3) ¿sobreviven los nombres con espacios/acentos (lección de EGAnetLab 0.1.0)?
# -----------------------------------------------------------------------------
source("00_setup.R")

# The options exactly as jamovi would pass them from three ListBoxes
# (Reflective constructs, Composite constructs, Structural model): each item is
# a Group with a label and a variable/construct list.
# ES: Las opciones tal como jamovi las pasaría desde tres ListBox (constructos
# reflectivos, compuestos, modelo estructural): cada ítem es un Group con
# etiqueta y lista.
options_ok <- list(
  reflective = list(list(label = "Competencia", vars = c("comp_1", "comp_2", "comp_3")),
                    list(label = "Simpatía",    vars = c("like_1", "like_2", "like_3")),
                    list(label = "Satisfacción del cliente", vars = c("cusa")),
                    list(label = "Lealtad",     vars = c("cusl_1", "cusl_2", "cusl_3"))),
  composite  = list(list(label = "Calidad", vars = paste0("qual_", 1:8))),
  structural = list(list(outcome = "Competencia", predictors = c("Calidad")),
                    list(outcome = "Simpatía",    predictors = c("Calidad")),
                    list(outcome = "Satisfacción del cliente", predictors = c("Competencia", "Simpatía")),
                    list(outcome = "Lealtad", predictors = c("Competencia", "Simpatía", "Satisfacción del cliente"))),
  higher = list(), interactions = list())

# Builder + validator. Category A problems -> one message each (jmvcore::reject
# in the module). Category B -> notes that never block. Internal names are
# C1..Ck for constructs and V1..Vp for indicators; maps translate back.
# ES: Constructor + validador. Categoría A -> un mensaje cada uno (reject en el
# módulo). Categoría B -> notas que nunca bloquean. Nombres internos C1..Ck y
# V1..Vp; los mapas traducen de vuelta.
build_model <- function(opt, data) {
  errors <- character(0); notes <- character(0)
  blocks <- c(lapply(opt$reflective, c, list(type = "=~")),
              lapply(opt$composite,  c, list(type = "<~")))
  labels <- vapply(blocks, function(b) trimws(b$label), "")
  inds   <- lapply(blocks, `[[`, "vars")
  hlabels <- vapply(opt$higher, function(h) trimws(h$label), "")
  all_labels <- c(labels, hlabels)

  if (length(blocks) < 2) errors <- c(errors, "Define at least two constructs: a structural model needs a predictor and an outcome.")
  if (any(all_labels == "")) errors <- c(errors, "Every construct needs a name.")
  dup <- unique(all_labels[duplicated(all_labels)])
  if (length(dup)) errors <- c(errors, sprintf("The name '%s' is used for more than one construct.", dup))
  empty <- labels[lengths(inds) == 0]
  if (length(empty)) errors <- c(errors, sprintf("'%s' has no indicators: drag at least one variable into it.", empty))
  used <- unlist(inds); twice <- unique(used[duplicated(used)])
  if (length(twice)) errors <- c(errors, sprintf(
    "'%s' is an indicator of more than one construct. In PLS-SEM each indicator belongs to one block only.", twice))
  for (b in seq_along(blocks)) if (blocks[[b]]$type == "=~" && length(inds[[b]]) == 1)
    notes <- c(notes, sprintf("'%s' has a single indicator: its reliability is fixed at 1 and cannot be evaluated.", labels[b]))

  # Data checks on the indicators.
  # ES: Comprobaciones de los datos de los indicadores.
  for (v in unique(used)) {
    x <- data[[v]]
    if (is.null(x)) { errors <- c(errors, sprintf("Variable '%s' is not in the data set.", v)); next }
    if (!is.numeric(x)) errors <- c(errors, sprintf("'%s' is not numeric.", v))
    else {
      if (isTRUE(sd(x, na.rm = TRUE) == 0)) errors <- c(errors, sprintf("'%s' is constant: it carries no information.", v))
      sentinel <- x %in% c(-99, -999, -9, 99, 999, 9999) & (x < quantile(x, .01, na.rm = TRUE) | x > quantile(x, .99, na.rm = TRUE))
      if (any(sentinel, na.rm = TRUE)) errors <- c(errors, sprintf(
        "'%s' has %d value(s) of %s, far outside its other values. If this is a missing-data code, declare it as missing in jamovi's data view.",
        v, sum(sentinel, na.rm = TRUE), paste(unique(x[sentinel %in% TRUE]), collapse = ", ")))
    }
  }

  # Structural model checks.
  # ES: Comprobaciones del modelo estructural.
  edges <- do.call(rbind, lapply(opt$structural, function(s)
    if (length(s$predictors)) data.frame(from = s$predictors, to = s$outcome) else NULL))
  if (is.null(edges) || !nrow(edges)) errors <- c(errors, "The structural model is empty: add at least one outcome with its predictors.")
  else {
    unknown <- setdiff(c(edges$from, edges$to), all_labels)
    if (length(unknown)) errors <- c(errors, sprintf("'%s' appears in the structural model but is not defined as a construct.", unknown))
    self <- edges$from[edges$from == edges$to]
    if (length(self)) errors <- c(errors, sprintf("'%s' cannot predict itself.", self))
    # Cycle detection (PLS-PM with OLS needs a recursive model).
    # ES: Detección de ciclos (PLS-PM con MCO necesita un modelo recursivo).
    adj <- split(edges$to, edges$from); state <- list()
    visit <- function(n) { if (identical(state[[n]], 1)) return(TRUE); if (identical(state[[n]], 2)) return(FALSE)
      state[[n]] <<- 1; for (m in adj[[n]]) if (visit(m)) return(TRUE); state[[n]] <<- 2; FALSE }
    if (any(vapply(unique(edges$from), visit, TRUE))) errors <- c(errors,
      "The structural model has a loop (A -> B -> ... -> A). PLS-SEM estimates recursive models only: remove one path of the loop.")
    isolated <- setdiff(all_labels, c(edges$from, edges$to, unlist(lapply(opt$higher, `[[`, "vars"))))
    if (length(isolated)) errors <- c(errors, sprintf("'%s' is not connected to any other construct in the structural model.", isolated))
  }
  if (length(errors)) return(list(ok = FALSE, errors = errors, notes = notes))

  # Safe internal names and the syntax.
  # ES: Nombres internos seguros y la sintaxis.
  cmap <- setNames(paste0("C", seq_along(all_labels)), all_labels)
  vmap <- setNames(paste0("V", seq_along(unique(used))), unique(used))
  lines <- vapply(seq_along(blocks), function(b)
    sprintf("%s %s %s", cmap[[labels[b]]], blocks[[b]]$type, paste(vmap[inds[[b]]], collapse = " + ")), "")
  hlines <- vapply(opt$higher, function(h) sprintf("%s =~ %s", cmap[[h$label]], paste(cmap[h$vars], collapse = " + ")), "")
  slines <- vapply(opt$structural, function(s)
    sprintf("%s ~ %s", cmap[[s$outcome]], paste(cmap[s$predictors], collapse = " + ")), "")
  d <- data[, names(vmap), drop = FALSE]; names(d) <- unname(vmap)
  list(ok = TRUE, notes = notes, syntax = paste(c(lines, hlines, slines), collapse = "\n"),
       data = d, cmap = cmap, vmap = vmap,
       # Readable syntax for the reproducibility record (MokkenLab pattern).
       # ES: Sintaxis legible para el registro de reproducibilidad.
       readable = paste(c(
         vapply(seq_along(blocks), function(b) sprintf("`%s` %s %s", labels[b], blocks[[b]]$type, paste(inds[[b]], collapse = " + ")), ""),
         vapply(opt$structural, function(s) sprintf("`%s` ~ %s", s$outcome, paste(sprintf("`%s`", s$predictors), collapse = " + ")), "")), collapse = "\n"))
}

# A. Correct model with accented names and spaces, on data that still has -99.
# ES: A. Modelo correcto con acentos y espacios, sobre datos que aún tienen -99.
cat("== A1: raw data (with -99 codes)\n")
m <- build_model(options_ok, corp_raw); print(m$errors)
cat("\n== A2: cleaned data\n")
m <- build_model(options_ok, corp_meanrep)
cat("ok:", m$ok, "\nnotes:", m$notes, "\nsyntax:\n", m$syntax, "\n")
f <- run_case("csem() on built model", csem(m$data, m$syntax, .disattenuate = FALSE))
if (f$ok) {
  p <- f$value$Estimates$Path_estimates
  inv <- setNames(names(m$cmap), m$cmap)
  idx <- which(p != 0, arr.ind = TRUE)
  out <- data.frame(path = sprintf("%s -> %s", inv[colnames(p)[idx[, 2]]], inv[rownames(p)[idx[, 1]]]),
                    estimate = round(p[idx], 3))
  print(out, row.names = FALSE)
}

# B. Common mistakes: what cSEM says alone vs what the validator says.
# ES: B. Errores comunes: qué dice cSEM solo frente a qué dice el validador.
mistakes <- list(
  "empty construct"      = function(o) { o$reflective[[1]]$vars <- character(0); o },
  "indicator twice"      = function(o) { o$reflective[[2]]$vars <- c("like_1", "like_2", "comp_1"); o },
  "duplicated name"      = function(o) { o$reflective[[2]]$label <- "Competencia"; o },
  "loop"                 = function(o) { o$structural[[5]] <- list(outcome = "Calidad", predictors = "Lealtad"); o },
  "isolated construct"   = function(o) { o$composite[[2]] <- list(label = "Desempeño", vars = paste0("perf_", 1:5)); o },
  "unknown in paths"     = function(o) { o$structural[[1]]$predictors <- c("Calidad", "Atractivo"); o },
  "no structural model"  = function(o) { o$structural <- list(); o })
# ASCII names here, so that cSEM's own message is about the mistake and not
# about the accents (section C tests accents on their own).
# ES: Nombres ASCII aquí, para que el mensaje de cSEM trate del error y no de
# los acentos (la sección C prueba los acentos por separado).
asc <- function(x) gsub("[^A-Za-z0-9_]", "_", iconv(x, "UTF-8", "ASCII//TRANSLIT"))
raw_syntax <- function(o) { # naive syntax as a user would type it in JASP
  b <- c(lapply(o$reflective, c, list(type = "=~")), lapply(o$composite, c, list(type = "<~")))
  paste(c(vapply(b, function(x) sprintf("%s %s %s", asc(x$label), x$type,
                                        paste(x$vars, collapse = " + ")), ""),
          vapply(o$structural, function(s) sprintf("%s ~ %s", asc(s$outcome),
                                                   paste(asc(s$predictors), collapse = " + ")), "")),
        collapse = "\n")
}
for (nm in names(mistakes)) {
  o <- mistakes[[nm]](options_ok)
  cat("\n==", nm, "\n")
  r <- run_case("  cSEM alone", csem(corp_meanrep, raw_syntax(o), .disattenuate = FALSE))
  v <- build_model(o, corp_meanrep)
  cat("  validator:", if (v$ok) "accepted" else paste(v$errors, collapse = " | "), "\n")
}

# C. Variable names with spaces and accents go straight to cSEM?
# ES: C. ¿Nombres de variables con espacios y acentos directo a cSEM?
d2 <- corp_meanrep; names(d2)[names(d2) == "comp_1"] <- "competencia 1"
run_case("cSEM with a variable named 'competencia 1'",
         csem(d2, "COMP =~ `competencia 1` + comp_2 + comp_3\nLIKE =~ like_1 + like_2 + like_3\nLIKE ~ COMP"))
run_case("cSEM with an accented construct name (Simpatía)",
         csem(corp_meanrep, "Competencia =~ comp_1 + comp_2 + comp_3\nSimpatía =~ like_1 + like_2 + like_3\nSimpatía ~ Competencia"))
o3 <- options_ok; o3$reflective[[1]]$vars[1] <- "competencia 1"
m3 <- build_model(o3, d2)
run_case("Same through the builder (V1..Vp)", csem(m3$data, m3$syntax))
