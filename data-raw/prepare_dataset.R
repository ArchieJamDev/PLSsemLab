# -----------------------------------------------------------------------------
# PLSsemLab
# A jamovi module for partial least squares structural equation modeling and
# evidence-based methodological decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of PLSsemLab.
#
# PLSsemLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# PLSsemLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PLSsemLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Build the bundled example datasets.
# ES: Construir los conjuntos de datos de ejemplo incluidos.
#
# Source: two data sets shipped with the cSEM R package (GPL-3), so
# PLSsemLab's results can be checked against published PLS-SEM examples.
# Run from the package root: Rscript data-raw/prepare_dataset.R
#
# ES: Fuente: dos conjuntos incluidos en el paquete de R cSEM (GPL-3), para
# que los resultados de PLSsemLab puedan contrastarse con ejemplos
# publicados de PLS-SEM. Ejecutar desde la raíz del paquete:
# Rscript data-raw/prepare_dataset.R
#
# The corporate reputation data code missing answers as -99. They are written
# as empty cells, which jamovi reads as missing, so the example does not
# carry the trap the design experiments found (experiments/design-v1, E1).
# Nothing else is recoded, imputed or removed.
#
# ES: Los datos de reputación corporativa codifican las respuestas faltantes
# como -99. Se escriben como celdas vacías, que jamovi lee como perdidos,
# para que el ejemplo no arrastre la trampa que encontraron los experimentos
# de diseño (E1). No se recodifica, imputa ni elimina nada más.
# -----------------------------------------------------------------------------

utils::data("corp_rep_data", "satisfaction", package = "cSEM", envir = environment())

# Corporate reputation (Hair et al.): 344 mobile phone customers.
# ES: Reputación corporativa (Hair et al.): 344 clientes de telefonía móvil.
corp <- corp_rep_data
corp[corp == -99] <- NA
corp$servicetype <- factor(corp$servicetype, levels = 1:2, labels = c("Prepaid", "Contract"))
utils::write.csv(corp, "data/plssemlab_corp_rep.csv", row.names = FALSE, na = "")

# European Customer Satisfaction Index (ECSI), mobile phone industry: 250
# customers (the satisfaction data of cSEM and plspm).
# ES: Índice Europeo de Satisfacción del Cliente (ECSI), telefonía móvil: 250
# clientes.
utils::write.csv(satisfaction, "data/plssemlab_satisfaction.csv", row.names = FALSE, na = "")

cat(sprintf("corp_rep: %d x %d, %d missing cells | satisfaction: %d x %d\n",
            nrow(corp), ncol(corp), sum(is.na(corp)), nrow(satisfaction), ncol(satisfaction)))
