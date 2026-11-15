# Interactions in the Structural Model box of a real jamovi (docker): three
# reflective constructs; select Construct 1 and Construct 2 in the
# structural supplier and transfer them to the predictors of Construct 3:
# what does the transfer offer (jamovi's "interactions" transfer action)?
# ES: Interacciones en la caja Modelo estructural de un jamovi real: tres
# constructos; se seleccionan Construct 1 y Construct 2 y se transfieren a
# los predictores de Construct 3: ¿qué ofrece la transferencia?
import json, re, sys, time
from playwright.sync_api import sync_playwright

OUT = sys.argv[1] if len(sys.argv) > 1 else "probe-out"
log = []

def step(desc, fn):
    try:
        fn()
        log.append(f"OK   {desc}")
    except Exception as e:
        log.append(f"FAIL {desc}: {e.__class__.__name__} {str(e)[:200]}")
    time.sleep(2.5)

def frame_by_port(page, port):
    for f in page.frames:
        if f":{port}/" in f.url:
            return f
    return None

def item(opts, text, nth=0):
    return opts.locator(".silky-list-item-value").filter(has_text=re.compile("^" + re.escape(text) + "$")).nth(nth)

def visible_item(opts, text):
    loc = opts.locator(".silky-list-item-value").filter(has_text=re.compile("^" + re.escape(text) + "$"))
    for i in range(loc.count()):
        if loc.nth(i).is_visible():
            return loc.nth(i)
    raise Exception("no visible item " + text)

def state(opts, page, name):
    try:
        page.screenshot(path=f"{OUT}/{name}.png", timeout=15000)
    except Exception as e:
        log.append(f"screenshot {name} failed: {e.__class__.__name__}")
    info = opts.evaluate("""() => {
        const sm = [...document.querySelectorAll('.silky-control-label, .silky-list-item-value, .silky-option-listbox-ghost-text, [class*=ghost]')]
            .map(e => (e.className.split(' ')[0] + ':' + e.textContent.trim())).filter(s => !s.endsWith(':'));
        return sm.slice(0, 80);
    }""")
    log.append(f"--- {name}: " + json.dumps(info, ensure_ascii=False)[:2500])

with sync_playwright() as p:
    browser = p.chromium.launch()
    page = browser.new_page(viewport={"width": 1500, "height": 1300})
    page.set_default_timeout(15000)
    page.on("console", lambda m: log.append(f"[console.{m.type}] {m.text[:300]}") if m.type in ("error", "warning") else None)
    page.on("pageerror", lambda e: log.append(f"[pageerror] {e}"))
    page.goto("http://127.0.0.1:41337/", wait_until="load")
    time.sleep(20)
    step("Analyses tab", lambda: page.get_by_text("Analyses", exact=True).first.click(timeout=8000))
    step("PLS-SEM Lab ribbon", lambda: page.get_by_text("PLS-SEM Lab", exact=True).first.click(timeout=8000))
    items = page.get_by_text("PLS-SEM", exact=True)
    for i in range(items.count()):
        try:
            items.nth(i).click(timeout=3000); log.append(f"OK   menu item {i}"); break
        except Exception:
            pass
    time.sleep(15)
    opts = frame_by_port(page, 41338)
    transfer = opts.locator("button.jmv-variable-transfer")
    log.append(f"transfer buttons: {transfer.count()}")
    state(opts, page, "s0-opened")
    for v, k in (("A", 1), ("B", 2), ("C", 3)):
        if k > 1:
            step(f"add construct {k}", lambda: opts.get_by_text("Add New Construct", exact=True).first.click(timeout=5000))
            step(f"focus construct {k}", lambda: opts.get_by_text("drag indicators here", exact=True).last.click(timeout=5000))
        step(f"select {v}", lambda v=v: item(opts, v).click(timeout=5000))
        step(f"{v} -> construct {k}", lambda: transfer.nth(0).click(timeout=5000))
        time.sleep(4)
    time.sleep(6)
    state(opts, page, "i1-constructs")
    def menu_texts():
        return opts.evaluate("""() => [...document.querySelectorAll('*')].filter(e => e.offsetParent !== null && e.children.length === 0 && /Interaction|Main Effects|All .* way/i.test(e.textContent)).map(e => e.className + ':' + e.textContent.trim()).slice(0, 20)""")
    for attempt in (1, 2):
        step(f"focus Construct 3 row ({attempt})", lambda: opts.get_by_text("drag predictors here", exact=True).last.click(timeout=5000))
        step(f"select Construct 1 ({attempt})", lambda: visible_item(opts, "Construct 1").click(timeout=5000))
        step(f"ctrl-select Construct 2 ({attempt})", lambda: visible_item(opts, "Construct 2").click(modifiers=["Control"], timeout=5000))
        time.sleep(2)
        state(opts, page, f"i2-selected-{attempt}")
        vis = [i for i in range(transfer.count()) if transfer.nth(i).is_visible()]
        log.append(f"visible transfers: {vis}")
        log.append("menu before click: " + json.dumps(menu_texts(), ensure_ascii=False))
        step(f"click transfer ({attempt})", lambda: transfer.nth(vis[-1]).click(timeout=5000))
        time.sleep(2)
        state(opts, page, f"i3-after-click-{attempt}")
        log.append("menu after click: " + json.dumps(menu_texts(), ensure_ascii=False))
        hit = opts.get_by_text(re.compile("^Interaction$"))
        if hit.count():
            step("choose Interaction", lambda: hit.first.click(timeout=5000))
            time.sleep(5)
            state(opts, page, "i4-interaction-chosen")
            break
        time.sleep(5)
    log.append("options: " + json.dumps(opts.evaluate("""() => [...document.querySelectorAll('.silky-option-listbox')].map(e => e.innerText.replace(/\\n+/g, ' / ').slice(0, 300))"""), ensure_ascii=False)[:3000])
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
