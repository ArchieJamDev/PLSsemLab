// -----------------------------------------------------------------------------
// PLSsemLab
// A jamovi module for partial least squares structural equation modeling and
// evidence-based methodological decision support.
//
// Copyright (C) 2026 Arquímedes De León Chacón Chacón
//
// This file is part of PLSsemLab, licensed under the GNU General Public
// License, version 3 or later. See https://www.gnu.org/licenses/.
// -----------------------------------------------------------------------------

// Keeps the structural model in step with the construct boxes.
// ES: Mantiene el modelo estructural sincronizado con las cajas de constructos.
//
// 1. Every new construct gets a default name ("Construct 2"), as jamovi's CFA
//    and EGAnetLab do.
// 2. The constructs that have a name and at least one indicator are offered
//    in the "Constructs" supplier, from which predictors are dragged.
// 3. The structural model has exactly one row per such construct, labelled
//    with its name. Rows are matched by name, so removing or reordering
//    constructs never moves predictors to another construct; a renamed
//    construct keeps its row, and its new name replaces the old one in the
//    other rows.
// 4. A construct cannot predict itself, and predictors that no longer exist
//    are removed.
// Same approach as SEMLj (semljgui.u.yaml + gui.events.js, GPL), with rows
// matched by name instead of by position.
//
// ES: 1. Cada constructo nuevo recibe un nombre por defecto ("Construct 2").
// 2. Los constructos con nombre y al menos un indicador se ofrecen en el
//    supplier "Constructs", desde donde se arrastran los predictores.
// 3. El modelo estructural tiene una fila por cada uno, con su nombre. Las
//    filas se emparejan por nombre: quitar o reordenar constructos nunca pasa
//    predictores a otro constructo; uno renombrado conserva su fila y su
//    nombre nuevo reemplaza al viejo en las demás filas.
// 4. Un constructo no puede predecirse a sí mismo, y se quitan los
//    predictores que ya no existen.

const events = {

    update: function(ui) {
        sync(ui, this);
    },

    onConstructsChanged: function(ui) {
        sync(ui, this);
    },

    onPredictorsChanged: function(ui) {
        sync(ui, this);
    },

    // jamovi requires both handlers on a Supplier: "update" asks for its
    // items (refilled from the boxes), "change" fires when they change.
    // ES: jamovi exige ambos manejadores en un Supplier: "update" pide sus
    // ítems (se vuelven a llenar desde las cajas), "change" avisa del cambio.
    onUpdateConstructSupplier: function(ui) {
        sync(ui, this);
    },

    onChangeConstructSupplier: function(ui) {
    }
};

// Default names for unnamed constructs, numbered across both boxes.
// ES: Nombres por defecto para constructos sin nombre, numerados en ambas cajas.
const defaultNames = function(ui) {
    let nReflective = 0;
    ui.reflective.applyToItems(0, (item, index) => {
        nReflective = index + 1;
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue('Construct ' + (index + 1));
    });
    ui.formative.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue('Construct ' + (nReflective + index + 1));
    });
};

// Names of the constructs that can enter the structural model.
// ES: Nombres de los constructos que pueden entrar al modelo estructural.
const constructNames = function(ui, context) {
    let blocks = context.cloneArray(ui.reflective.value(), [])
        .concat(context.cloneArray(ui.formative.value(), []));
    let names = [];
    for (let block of blocks) {
        if ( ! block || ! block.label)
            continue;
        let label = block.label.trim();
        if (label === '' || ! block.vars || block.vars.length === 0)
            continue;
        if (names.indexOf(label) === -1)
            names.push(label);
    }
    return names;
};

// A term is a list of construct names (one name, or two for an interaction).
// ES: Un término es una lista de nombres (uno, o dos para una interacción).
const renameInTerms = function(terms, from, to) {
    return terms.map(term => term.map(name => name === from ? to : name));
};

const cleanTerms = function(terms, self, names) {
    let out = [];
    let seen = {};
    for (let term of terms) {
        if ( ! Array.isArray(term) || term.length === 0)
            continue;
        if (term.indexOf(self) !== -1)
            continue;
        if ( ! term.every(name => names.indexOf(name) !== -1))
            continue;
        let key = term.slice().sort().join(':');
        if (seen[key])
            continue;
        seen[key] = true;
        out.push(term);
    }
    return out;
};

const sync = function(ui, context) {

    defaultNames(ui);
    let names = constructNames(ui, context);

    ui.constructSupplier.setValue(context.valuesToItems(names, FormatDef.variable));

    let rows = context.cloneArray(ui.structural.value(), []);
    let byLabel = {};
    for (let row of rows) {
        if (row && row.label)
            byLabel[row.label] = context.cloneArray(row.predictors, []);
    }

    // A rename: same number of constructs, one position whose name changed.
    // ES: Un renombrado: mismo número de constructos, una posición cambiada.
    let previous = context.workspace.constructNames;
    if (previous && previous.length === names.length) {
        for (let i = 0; i < names.length; i++) {
            let from = previous[i];
            let to = names[i];
            if (from === to || byLabel[to] !== undefined || byLabel[from] === undefined)
                continue;
            byLabel[to] = byLabel[from];
            delete byLabel[from];
            for (let label in byLabel)
                byLabel[label] = renameInTerms(byLabel[label], from, to);
        }
    }

    let newRows = names.map(name => ({
        label: name,
        predictors: cleanTerms(byLabel[name] || [], name, names)
    }));

    if (JSON.stringify(newRows) !== JSON.stringify(rows))
        ui.structural.setValue(newRows);

    context.workspace.constructNames = names;
};

module.exports = events;
