# Multigroup Analysis box in a real jamovi (docker): the box must open, its
# own variable supplier must list the data variables, a variable must move
# into Grouping Variable (one at most) and Run multigroup analysis must
# enable the other options.
# ES: Caja Análisis multigrupo en un jamovi real: la caja debe abrirse, su
# propio supplier de variables debe listar las variables, una variable debe
# pasar a Variable de agrupación (una como máximo) y Ejecutar debe habilitar
# las demás opciones.
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
    state(opts, page, "m0-opened")
    step("open Multigroup Analysis", lambda: opts.get_by_text("Multigroup Analysis", exact=True).first.click(timeout=5000))
    time.sleep(4)
    state(opts, page, "m1-box-open")
    step("select C in the group supplier", lambda: visible_item(opts, "C").click(timeout=5000))
    vis = [i for i in range(transfer.count()) if transfer.nth(i).is_visible()]
    log.append(f"visible transfers: {vis}")
    step("C -> Grouping Variable", lambda: transfer.nth(vis[-1]).click(timeout=5000))
    time.sleep(4)
    step("tick Run multigroup analysis", lambda: opts.get_by_text("Run multigroup analysis", exact=True).first.click(timeout=5000))
    time.sleep(6)
    state(opts, page, "m2-group-and-run")
    log.append("options: " + json.dumps(opts.evaluate("""() => [...document.querySelectorAll('.silky-option-listbox')].map(e => e.innerText.replace(/\\n+/g, ' / ').slice(0, 200))"""), ensure_ascii=False)[:2500])
    log.append("disabled: " + json.dumps(opts.evaluate("""() => [...document.querySelectorAll('.silky-control-label, label')].filter(e => e.closest('.disabled-text, .disabled, [disabled]')).map(e => e.textContent.trim()).slice(0, 40)"""), ensure_ascii=False))
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
