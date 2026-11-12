// -----------------------------------------------------------------------------
// PLSsemLab: tests of jamovi/js/plssem.events.js with a stand-in for the
// jamovi panel API (the calls the file uses: value, setValue, applyToItems,
// setPropertyValue, setCustomVariables, valuesToItems, cloneArray). As in
// jamovi, declaring custom variables fires the supplier's "update" event,
// so a handler that does work there would loop: the test fails if it does.
// ES: Pruebas del JS del panel con un sustituto de la API de jamovi. Como en
// jamovi, declarar variables personalizadas dispara el evento "update" del
// supplier; si un manejador hiciera trabajo ahí entraría en bucle y la
// prueba falla.
// Run: node tests/js/plssem.events.test.js
// -----------------------------------------------------------------------------
const assert = require('assert');
global.FormatDef = { variable: 'variable' };
// As in jamovi 2.7 for jus 3.0 modules: helpers in the global `utils`, none
// on the event context (cloneArray / valuesToItems on `this` do not exist).
// ES: Como en jamovi 2.7 para módulos jus 3.0: utilidades en el global.
global.utils = { valuesToItems: (vals) => vals.map(v => ({ value: v })) };
const ev = require('../../jamovi/js/plssem.events.js');

let depth = 0, maxDepth = 0, headers = {};
const box = (initial) => {
    let v = JSON.parse(JSON.stringify(initial));
    return {
        value: () => v,
        setValue: (x) => { v = JSON.parse(JSON.stringify(x)); },
        applyToItems: (start, cb) => v.forEach((it, i) => cb({ controls: [{
            value: () => it.label, setValue: (s) => { it.label = s; },
            setPropertyValue: (k, s) => { headers[i] = s; } }] }, i))
    };
};
const ui = {
    reflective: box([{ label: 'COMP', vars: ['c1', 'c2'] }, { label: '', vars: ['l1'] }]),
    formative: box([]),
    structural: box([]),
    constructSupplier: { items: [], setValue(x) { this.items = x; } }
};
const ctx = {
    workspace: {},
    setCustomVariables: function() {
        depth++; maxDepth = Math.max(maxDepth, depth);
        if (depth > 5) throw new Error('supplier update loop');
        ev.onUpdateConstructSupplier.call(ctx, ui);
        depth--;
    }
};
const supplier = () => ui.constructSupplier.items.map(i => i.value);
const rows = () => ui.structural.value();

ev.update.call(ctx, ui);
assert.deepStrictEqual(supplier(), ['COMP', 'Construct 2'], 'default name and supplier');
assert.deepStrictEqual(rows(), [[], []], 'one row per construct');

ui.structural.setValue([[], [['COMP'], ['Construct 2'], ['COMP']]]);
ev.onPredictorsChanged.call(ctx, ui);
assert.deepStrictEqual(rows(), [[], [['COMP']]], 'self and duplicate predictors removed');

ui.reflective.value().push({ label: '', vars: ['x1'] });
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(supplier(), ['COMP', 'Construct 2', 'Construct 3']);
assert.deepStrictEqual(rows(), [[], [['COMP']], []]);

ui.structural.setValue([[], [['COMP']], [['Construct 2'], ['COMP']]]);
ev.onPredictorsChanged.call(ctx, ui);
ui.reflective.value()[0].label = 'Competence';
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(rows(), [[], [['Competence']], [['Construct 2'], ['Competence']]], 'rename follows');
assert.strictEqual(headers[0], 'Competence \u2190', 'row header');

// Removing a predictor rebuilds the rows: the headers must come back.
// ES: Quitar un predictor reconstruye las filas: los encabezados vuelven.
for (let i = 0; i < headers.length; i++) headers[i] = undefined;
ui.structural.setValue([[], [], [['Construct 2'], ['Competence']]]);
ev.onPredictorsChanged.call(ctx, ui);
assert.strictEqual(headers[1], 'Construct 2 \u2190', 'headers after a removal');

ui.reflective.value().splice(1, 1);
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(rows(), [[], [['Competence']]], 'removed construct takes its row and predictors');

const saved = JSON.stringify(rows());
ctx.workspace = {};
ev.update.call(ctx, ui);
assert.strictEqual(JSON.stringify(rows()), saved, 'a reopened analysis keeps its rows');
assert.strictEqual(maxDepth, 1, 'no supplier update loop');
// Higher-order constructs (phase 4): the second supplier offers the
// first-order names; a higher-order construct with components joins the
// structural rows after them; unknown and repeated components are removed
// and renames of first-order constructs follow.
// ES: Constructos de orden superior (fase 4).
ui.reflectiveHoc = box([{ label: 'CAPSI', components: [['Competence'], ['Construct 3'], ['Nope'], ['Competence']] }]);
ui.formativeHoc = box([]);
ui.hocSupplier = { items: [], setValue(x) { this.items = x; } };
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(ui.hocSupplier.items.map(i => i.value), ['Competence', 'Construct 3'], 'hoc supplier: first-order only');
assert.deepStrictEqual(ui.reflectiveHoc.value()[0].components, [['Competence'], ['Construct 3']], 'components cleaned');
assert.deepStrictEqual(supplier(), ['Competence', 'Construct 3', 'CAPSI'], 'hoc in the structural supplier');
assert.strictEqual(rows().length, 3, 'a structural row for the hoc');
ui.reflective.value()[0].label = 'COMPET';
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(ui.reflectiveHoc.value()[0].components, [['COMPET'], ['Construct 3']], 'rename followed');
ui.formativeHoc.value().push({ label: '', components: [] });
ev.onConstructsChanged.call(ctx, ui);
assert.strictEqual(ui.formativeHoc.value()[0].label, 'Higher-order 2', 'default hoc name');

console.log('plssem.events.js: all checks passed');
