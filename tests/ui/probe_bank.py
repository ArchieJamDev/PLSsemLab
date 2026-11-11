# The model of a user report, built with clicks in a real jamovi (docker) on
# the bundled bank data: reflective Like, Comp, Loy; formative Qual;
# Qual -> Like, Like -> Loy, Comp -> Qual; bootstrap with 100 resamples.
# Then a predictor is removed and added again. Logs row headers, results
# and engine errors at each step; screenshots go to OUT.
# ES: El modelo de un reporte de usuario, armado con clics en un jamovi real
# sobre los datos del banco, con bootstrap de 100 réplicas; luego se quita
# y se vuelve a agregar un predictor.
import json, re, sys, time
from playwright.sync_api import sync_playwright

OUT = sys.argv[1] if len(sys.argv) > 1 else "probe-out"
BASE = "http://127.0.0.1:41337/"
log = []

def step(desc, fn, wait=2.0):
    try:
        fn()
        log.append(f"OK   {desc}")
    except Exception as e:
        log.append(f"FAIL {desc}: {e.__class__.__name__} {str(e)[:200]}")
    time.sleep(wait)

def frame_by_port(page, port):
    for f in page.frames:
        if f":{port}/" in f.url:
            return f
    return None

def item(opts, text, nth=0):
    return opts.locator(".silky-list-item-value").filter(has_text=re.compile("^" + re.escape(text) + "$")).nth(nth)

def snap(page, opts, name):
    try:
        page.screenshot(path=f"{OUT}/{name}.png", timeout=15000)
    except Exception as e:
        log.append(f"screenshot {name} failed: {e.__class__.__name__}")
    try:
        heads = opts.evaluate("""() => [...document.querySelectorAll('.silky-target-list-header, .silky-list-item-value')]
            .map(e => e.textContent.trim()).filter(t => t.length && t.length < 40)""")
        log.append(f"--- {name} panel: " + json.dumps(heads, ensure_ascii=False)[:1800])
    except Exception as e:
        log.append(f"--- {name} panel: {e.__class__.__name__}")
    res = frame_by_port(page, 41339)
    if res:
        try:
            text = " | ".join(t.strip() for t in res.inner_text("body").splitlines() if t.strip())
            log.append(f"--- {name} results: " + text[:1500])
        except Exception as e:
            log.append(f"--- {name} results: {e.__class__.__name__}")

with sync_playwright() as p:
    browser = p.chromium.launch()
    page = browser.new_page(viewport={"width": 1500, "height": 1400})
    page.set_default_timeout(15000)
    page.on("console", lambda m: log.append(f"[console.{m.type}] {m.text[:300]}") if m.type == "error" else None)
    page.on("pageerror", lambda e: log.append(f"[pageerror] {e}"))
    r = page.request.get(BASE + "open?url=/tmp/plssemlab_bank.csv", timeout=120000)
    body = r.text()
    log.append("open: " + body[-300:])
    m = re.search(r'([a-f0-9]{8}-[a-f0-9-]{27})', body)
    page.goto(BASE + (m.group(1) + "/" if m else ""), wait_until="load")
    time.sleep(20)
    step("Analyses tab", lambda: page.get_by_text("Analyses", exact=True).first.click())
    step("PLS-SEM Lab ribbon", lambda: page.get_by_text("PLS-SEM Lab", exact=True).first.click())
    items = page.get_by_text("PLS-SEM", exact=True)
    for i in range(items.count()):
        try:
            items.nth(i).click(timeout=3000); log.append(f"OK   menu item {i}"); break
        except Exception:
            pass
    time.sleep(15)
    opts = frame_by_port(page, 41338)
    transfer = opts.locator("button.jmv-variable-transfer")
    names = opts.locator("input.frameless-textbox")

    def construct(box, name, vars, first=False):
        if not first:
            step(f"add construct {name}", lambda: opts.get_by_text("Add New Construct", exact=True).nth(box).click())
        step(f"name {name}", lambda: (names.last.fill(name), names.last.press("Tab")) if not first else (names.first.fill(name), names.first.press("Tab")))
        step(f"focus {name} indicators", lambda: opts.get_by_text("drag indicators here", exact=True).last.click())
        for k, v in enumerate(vars):
            step(f"select {v}", lambda: item(opts, v).click(modifiers=["Control"] if k else []), wait=0.5)
        step(f"transfer to {name}", lambda: transfer.nth(box).click(), wait=4)

    construct(0, "Like", ["like_1", "like_2"], first=True)
    construct(0, "Comp", ["comp_1", "comp_2", "comp_3"])
    construct(0, "Loy", ["loy_1", "loy_2", "loy_3"])
    construct(1, "Qual", ["qual_1", "qual_2", "qual_4", "qual_5", "qual_6", "qual_7"])
    time.sleep(8)
    snap(page, opts, "b1-constructs")

    # rows: Like, Comp, Loy, Qual; empty rows show the ghost text, so fill
    # from the last row up
    for row, pred in ((3, "Comp"), (2, "Like"), (0, "Qual")):
        step(f"focus row {row}", lambda: opts.get_by_text("drag predictors here", exact=True).nth(row).click())
        step(f"select {pred} in supplier", lambda: item(opts, pred).click())
        step(f"{pred} -> row {row}", lambda: transfer.nth(2).click(), wait=4)
    time.sleep(20)
    snap(page, opts, "b2-paths")

    step("open Bootstrap box", lambda: opts.get_by_text("Bootstrap", exact=True).first.click())
    reps = opts.get_by_label("Resamples")
    step("resamples 100", lambda: (reps.first.fill("100"), reps.first.press("Tab")))
    step("run bootstrap", lambda: opts.get_by_text("Run bootstrap", exact=True).first.click(), wait=1)
    for t in range(12):
        time.sleep(10)
        res = frame_by_port(page, 41339)
        txt = res.inner_text("body") if res else ""
        if "resamples drawn" in txt or "terminated" in txt or "resource limit" in txt:
            break
    snap(page, opts, "b3-bootstrap")

    step("select Qual inside row of Like", lambda: item(opts, "Qual", 1).click())
    step("remove it", lambda: transfer.nth(2).click(), wait=5)
    snap(page, opts, "b4-removed")
    step("focus row of Like", lambda: opts.get_by_text("drag predictors here", exact=True).first.click())
    step("select Qual in supplier", lambda: item(opts, "Qual").click())
    step("Qual -> row of Like again", lambda: transfer.nth(2).click(), wait=5)
    time.sleep(15)
    snap(page, opts, "b5-readded")
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
