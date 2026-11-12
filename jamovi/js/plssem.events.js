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

// jus 3.0 modules reach jamovi's helpers through the global `utils` (as
// jmv's own anova.js does); the event context (`this`) only carries them for
// jus 2.0 modules such as SEMLj, and cloneArray / valuesToItems on `this`
// are missing in jamovi 2.7.4 ("context.cloneArray is not a function", seen
// with a headless browser in the official jamovi 2.7.4 docker image). These
// two helpers use `utils` when it exists and fall back to plain code.
// ES: Los módulos jus 3.0 usan las utilidades de jamovi a través del global
// `utils` (como el anova.js de jmv); el contexto `this` solo las trae para
// módulos jus 2.0 como SEMLj, y en jamovi 2.7.4 no existen
// ("context.cloneArray is not a function"). Estos dos ayudantes usan `utils`
// si existe y si no, código propio.
const jmvUtils = function() {
    return typeof utils !== 'undefined' ? utils : null;
};

const clone = function(value, ifNull) {
    if (value === undefined || value === null)
        return ifNull;
    return JSON.parse(JSON.stringify(value));
};

const toItems = function(context, values) {
    let u = jmvUtils();
    if (u && typeof u.valuesToItems === 'function')
        return u.valuesToItems(values, FormatDef.variable);
    return context.valuesToItems(values, FormatDef.variable);
};

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

    // The supplier asks for its items: give it the current construct names
    // (jmv's anova.js fills modelSupplier the same way). No custom variables
    // here: declaring them refreshes the supplier and would loop.
    // ES: El supplier pide sus ítems: se le dan los nombres actuales (como el
    // anova.js de jmv). Sin variables personalizadas aquí: declararlas
    // refresca el supplier y entraría en bucle.
    onUpdateConstructSupplier: function(ui) {
        ui.constructSupplier.setValue(toItems(this, constructNames(ui, this)));
    },

    // The higher-order box offers only first-order constructs (phase 4).
    // ES: La caja de orden superior ofrece solo constructos de primer orden.
    onUpdateHocSupplier: function(ui) {
        ui.hocSupplier.setValue(toItems(this, firstOrderNames(ui, this)));
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
    let nHoc = 0;
    for (let box of [ui.reflectiveHoc, ui.formativeHoc]) {
        if ( ! box) continue;
        box.applyToItems(0, (item) => {
            nHoc += 1;
            let value = item.controls[0].value();
            if ( ! value || value.trim() === '')
                item.controls[0].setValue('Higher-order ' + nHoc);
        });
    }
};

// Constructs that can enter the structural model: a name and at least one
// indicator, reflective first, then formative, without repeated names. The
// R side (.pl_structural_labels) applies the same rule to read the rows.
// ES: Constructos que pueden entrar al modelo estructural: nombre y al menos
// un indicador, reflectivos primero y luego formativos, sin nombres
// repetidos. El lado R aplica la misma regla para leer las filas.
const firstOrderNames = function(ui, context) {
    let blocks = clone(ui.reflective.value(), [])
        .concat(clone(ui.formative.value(), []));
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

// Higher-order constructs: a name and at least one component, reflective
// first (the R side, .pl_structural_labels, reads them in this order).
// ES: Constructos de orden superior: nombre y al menos un componente,
// reflectivos primero.
const hocNames = function(ui) {
    let names = [];
    for (let box of [ui.reflectiveHoc, ui.formativeHoc]) {
        if ( ! box) continue;
        for (let h of clone(box.value(), [])) {
            if ( ! h || typeof h.label !== 'string') continue;
            let label = h.label.trim();
            if (label === '' || ! Array.isArray(h.components) || h.components.length === 0)
                continue;
            if (names.indexOf(label) === -1)
                names.push(label);
        }
    }
    return names;
};

// Constructs of the structural model: first-order, then higher-order.
// ES: Constructos del modelo estructural: primer orden y luego superior.
const constructNames = function(ui, context) {
    let names = firstOrderNames(ui, context);
    for (let h of hocNames(ui))
        if (names.indexOf(h) === -1)
            names.push(h);
    return names;
};

// Components of each higher-order construct: first-order names only, once
// each; renames of first-order constructs follow (map from old to new).
// ES: Componentes de cada constructo de orden superior: solo nombres de
// primer orden, una vez cada uno; los renombrados se siguen.
const cleanHocs = function(ui, first, rename) {
    for (let box of [ui.reflectiveHoc, ui.formativeHoc]) {
        if ( ! box) continue;
        let value = clone(box.value(), []);
        let changed = false;
        let out = value.map(h => {
            if ( ! h) return h;
            let comps = Array.isArray(h.components) ? h.components : [];
            let clean = [];
            for (let raw of comps) {
                let t = asTerm(raw);
                if (t === null || t.length !== 1) continue;
                let name = rename[t[0]] || t[0];
                if (first.indexOf(name) === -1) continue;
                if (clean.some(c => c[0] === name)) continue;
                clean.push([name]);
            }
            if (JSON.stringify(clean) !== JSON.stringify(comps)) changed = true;
            return { label: h.label, components: clean };
        });
        if (changed)
            box.setValue(out);
    }
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

// Row headers: the construct name on each row followed by an arrow (the
// row lists what predicts it), as SEMLj's labelize.
// ES: Encabezados de fila: el nombre del constructo y una flecha (la fila
// lista lo que lo predice), como el labelize de SEMLj.
const labelize = function(ui, names) {
    ui.structural.applyToItems(0, (item, index) => {
        if (index < names.length)
            item.controls[0].setPropertyValue('label', names[index] + ' \u2190');
    });
};

const sync = function(ui, context) {

    defaultNames(ui);
    let first = firstOrderNames(ui, context);
    let previousFirst = context.workspace.firstNames || first;
    let rename = {};
    if (previousFirst.length === first.length)
        for (let i = 0; i < first.length; i++)
            if (previousFirst[i] !== first[i] && first.indexOf(previousFirst[i]) === -1)
                rename[previousFirst[i]] = first[i];
    cleanHocs(ui, first, rename);
    context.workspace.firstNames = first;
    if (ui.hocSupplier)
        ui.hocSupplier.setValue(toItems(context, first));
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
    ui.constructSupplier.setValue(toItems(context, names));

    // The stored rows belong to the names of the previous call (or, when the
    // analysis is opened, to the current names, in the same order).
    // ES: Las filas guardadas pertenecen a los nombres de la llamada anterior
    // (o, al abrir el análisis, a los nombres actuales, en el mismo orden).
    let rows = clone(ui.structural.value(), []);
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
    let rows = clone(ui.structural.value(), []);
    let newRows = names.map((name, i) => cleanTerms(rows[i], name, names));
    if (JSON.stringify(newRows) !== JSON.stringify(rows))
        ui.structural.setValue(newRows);
    // Setting the value rebuilds the rows, which drops their headers.
    // ES: Fijar el valor reconstruye las filas y borra sus encabezados.
    labelize(ui, names);
};

module.exports = events;
