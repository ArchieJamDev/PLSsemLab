# Opens PLS-SEM in a real jamovi (docker) with a headless browser: records
# console errors, drags two indicators of the blank data set (columns A, B)
# into a reflective construct, and checks that the options reach R (the
# results stop asking for constructs). Screenshots and logs go to OUT.
# ES: Abre PLS-SEM en un jamovi real (docker) con un navegador sin pantalla:
# guarda errores de consola, arrastra dos indicadores (columnas A y B del
# conjunto vacío) a un constructo reflectivo y comprueba que las opciones
# llegan a R. Capturas y registros en OUT.
import json, sys, time
from playwright.sync_api import sync_playwright

OUT = sys.argv[1] if len(sys.argv) > 1 else "probe-out"
log = []

def shot(page, name):
    page.screenshot(path=f"{OUT}/{name}.png", full_page=True)

def frame_by_port(page, port):
    for f in page.frames:
        if f":{port}/" in f.url:
            return f
    return None

def try_click(scope, desc, fn):
    try:
        fn()
        log.append(f"OK   {desc}")
        return True
    except Exception as e:
        log.append(f"FAIL {desc}: {e.__class__.__name__} {str(e)[:160]}")
        return False

with sync_playwright() as p:
    browser = p.chromium.launch()
    page = browser.new_page(viewport={"width": 1500, "height": 1100})
    page.on("console", lambda m: log.append(f"[console.{m.type}] {m.text[:300]}") if m.type in ("error", "warning") else None)
    page.on("pageerror", lambda e: log.append(f"[pageerror] {e}"))
    page.goto("http://127.0.0.1:41337/", wait_until="load")
    time.sleep(20)
    try_click(page, "Analyses tab", lambda: page.get_by_text("Analyses", exact=True).first.click(timeout=8000))
    time.sleep(2)
    try_click(page, "PLS-SEM Lab ribbon", lambda: page.get_by_text("PLS-SEM Lab", exact=True).first.click(timeout=8000))
    time.sleep(2)
    items = page.get_by_text("PLS-SEM", exact=True)
    for i in range(items.count()):
        if try_click(page, f"PLS-SEM menu item {i}", lambda: items.nth(i).click(timeout=3000)):
            break
    time.sleep(15)
    shot(page, "01-opened")

    opts = frame_by_port(page, 41338)
    log.append(f"options frame: {opts.url if opts else None}")
    if opts:
        with open(f"{OUT}/options.html", "w") as fh:
            fh.write(opts.content())
        for var in ("A", "B"):
            try_click(opts, f"select variable {var}",
                      lambda: opts.locator(".jmv-variable-label, .jmv-list-item").get_by_text(var, exact=True).first.click(timeout=5000))
            try_click(opts, f"transfer {var} to reflective",
                      lambda: opts.locator("button.jmv-variable-transfer, .jmv-variable-transfer").first.click(timeout=5000))
            time.sleep(3)
        time.sleep(10)
        shot(page, "02-indicators")
        # select an indicator inside the construct: the transfer arrow should turn back
        try_click(opts, "select indicator A inside the construct",
                  lambda: opts.locator(".jmv-variable-label, .jmv-list-item").get_by_text("A", exact=True).last.click(timeout=5000))
        time.sleep(2)
        shot(page, "03-selected")
        with open(f"{OUT}/options-after.html", "w") as fh:
            fh.write(opts.content())
        sup = opts.get_by_text("Construct 1", exact=True)
        log.append(f"'Construct 1' occurrences in panel: {sup.count()}")

    res = None
    for f in page.frames:
        try:
            if "PLS-SEM" in f.content() and ":41339/" in f.url:
                res = f
        except Exception:
            pass
    if res:
        text = res.inner_text("body")
        log.append("results: " + " | ".join(t.strip() for t in text.splitlines() if t.strip())[:900])
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
