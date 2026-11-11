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

// Keeps the structural model in step with the construct boxes. It follows
// SEMLj (semljgui.u.yaml + gui.events.js, GPL), whose interface works in
// jamovi, piece by piece:
//   - the structural option is an Array of Terms, one row per construct, in
//     the order of the constructs (SEMLj's endogenousTerms);
//   - each row's header is set with setPropertyValue("label") on its inner
//     list (SEMLj's labelize);
//   - construct names are declared as custom variables before they are put
//     in the supplier (SEMLj's calcCustomVariables / updateSuppliers);
//   - the supplier's "update" and "change" handlers are empty (as SEMLj's):
//     declaring custom variables refreshes the supplier, so doing work there
//     loops and freezes the boxes;
//   - indicator boxes listen to listItemAdded / listItemRemoved, names to
//     "change" (SEMLj's onChange_items_changed / onChange_latentName).
// One addition: rows follow their construct by name, so removing, reordering
// or renaming a construct never hands its predictors to another one.
//
// ES: Mantiene el modelo estructural sincronizado con las cajas de
// constructos. Sigue a SEMLj, cuya interfaz funciona en jamovi, pieza por
// pieza: la opción estructural es un Array de Terms con una fila por
// constructo en su orden; el encabezado de cada fila se pone con
// setPropertyValue("label"); los nombres se declaran como variables
// personalizadas antes de ponerlos en el supplier; los manejadores "update" y
// "change" del supplier están vacíos (declarar variables refresca el supplier
// y hacer trabajo ahí entra en bucle y congela las cajas); las cajas de
// indicadores escuchan listItemAdded / listItemRemoved y los nombres
// "change". Una mejora: las filas siguen a su constructo por nombre.

const events = {

    update: function(ui) {
        sync(ui, this);
    },

    onConstructsChanged: function(ui) {
        sync(ui, this);
    },

    onPredictorsChanged: function(ui) {
        cleanRows(ui, this);
    },

    onUpdateConstructSupplier: function(ui) {
    },

    onChangeConstructSupplier: function(ui) {
    }
};

// Default names for unnamed constructs, numbered across both boxes (SEMLj's
// updateModelLabels).
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

// Constructs that can enter the structural model: a name and at least one
// indicator, reflective first, then formative, without repeated names. The
// R side (.pl_structural_labels) applies the same rule to read the rows.
// ES: Constructos que pueden entrar al modelo estructural: nombre y al menos
// un indicador, reflectivos primero y luego formativos, sin nombres
// repetidos. El lado R aplica la misma regla para leer las filas.
const constructNames = function(ui, context) {
    let blocks = context.cloneArray(ui.reflective.value(), [])
        .concat(context.cloneArray(ui.formative.value(), []));
    let names = [];
    for (let block of blocks) {
        if ( ! block || typeof block.label !== 'string')
            continue;
        let label = block.label.trim();
        if (label === '' || ! Array.isArray(block.vars) || block.vars.length === 0)
            continue;
        if (names.indexOf(label) === -1)
            names.push(label);
    }
    return names;
};

// A term is a list of construct names (one name, or two for an interaction).
// ES: Un término es una lista de nombres (uno, o dos para una interacción).
const asTerm = function(term) {
    if (Array.isArray(term))
        return term;
    if (typeof term === 'string')
        return [term];
    return null;
};

const cleanTerms = function(terms, self, names) {
    let out = [];
    let seen = {};
    for (let raw of (Array.isArray(terms) ? terms : [])) {
        let term = asTerm(raw);
        if (term === null || term.length === 0)
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

// Row headers: the construct name on each row, as SEMLj's labelize.
// ES: Encabezados de fila, como el labelize de SEMLj.
const labelize = function(ui, names) {
    ui.structural.applyToItems(0, (item, index) => {
        if (index < names.length)
            item.controls[0].setPropertyValue('label', names[index]);
    });
};

const sync = function(ui, context) {

    defaultNames(ui);
    let names = constructNames(ui, context);

    // Custom variables only when the names change (SEMLj declares them on
    // every change; once per change of names is enough and avoids
    // refreshing the panel on every edit).
    // ES: Variables personalizadas solo cuando cambian los nombres.
    let key = names.join('\u0001');
    if (context.workspace.customKey !== key) {
        context.workspace.customKey = key;
        context.setCustomVariables(names.map(name => (
            { name: name, measureType: 'none', dataType: 'none', levels: [] })));
    }
    ui.constructSupplier.setValue(context.valuesToItems(names, FormatDef.variable));

    // The stored rows belong to the names of the previous call (or, when the
    // analysis is opened, to the current names, in the same order).
    // ES: Las filas guardadas pertenecen a los nombres de la llamada anterior
    // (o, al abrir el análisis, a los nombres actuales, en el mismo orden).
    let rows = context.cloneArray(ui.structural.value(), []);
    let previous = context.workspace.constructNames || names;
    let byName = {};
    for (let i = 0; i < previous.length; i++)
        byName[previous[i]] = Array.isArray(rows[i]) ? rows[i] : [];

    // A rename: same number of constructs, one position whose name changed.
    // ES: Un renombrado: mismo número de constructos, una posición cambiada.
    if (previous.length === names.length) {
        for (let i = 0; i < names.length; i++) {
            let from = previous[i];
            let to = names[i];
            if (from === to || byName[to] !== undefined || byName[from] === undefined)
                continue;
            byName[to] = byName[from];
            delete byName[from];
            for (let name in byName)
                byName[name] = byName[name].map(term => {
                    let t = asTerm(term);
                    return t === null ? term : t.map(x => x === from ? to : x);
                });
        }
    }

    let newRows = names.map(name => cleanTerms(byName[name], name, names));
    if (JSON.stringify(newRows) !== JSON.stringify(rows))
        ui.structural.setValue(newRows);
    labelize(ui, names);

    context.workspace.constructNames = names;
};

// After a drag into the structural rows: remove a construct from its own row,
// unknown names and repeated terms (SEMLj's cleanSelfEndogenous).
// ES: Tras arrastrar a las filas: quitar al constructo de su propia fila, los
// nombres desconocidos y los términos repetidos.
const cleanRows = function(ui, context) {
    let names = context.workspace.constructNames || constructNames(ui, context);
    let rows = context.cloneArray(ui.structural.value(), []);
    let newRows = names.map((name, i) => cleanTerms(rows[i], name, names));
    if (JSON.stringify(newRows) !== JSON.stringify(rows))
        ui.structural.setValue(newRows);
};

module.exports = events;
