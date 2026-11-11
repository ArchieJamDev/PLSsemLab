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
    cloneArray: (a, d) => a === undefined || a === null ? d : JSON.parse(JSON.stringify(a)),
    valuesToItems: (vals) => vals.map(v => ({ value: v })),
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
assert.strictEqual(headers[0], 'Competence', 'row header');

ui.reflective.value().splice(1, 1);
ev.onConstructsChanged.call(ctx, ui);
assert.deepStrictEqual(rows(), [[], [['Competence']]], 'removed construct takes its row and predictors');

const saved = JSON.stringify(rows());
ctx.workspace = {};
ev.update.call(ctx, ui);
assert.strictEqual(JSON.stringify(rows()), saved, 'a reopened analysis keeps its rows');
assert.strictEqual(maxDepth, 1, 'no supplier update loop');
console.log('plssem.events.js: all checks passed');
