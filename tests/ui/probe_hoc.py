# Higher-order box in a real jamovi (docker): two reflective constructs (A, B),
# then both are moved into the first reflective higher-order construct.
# ES: Caja de orden superior en un jamovi real: dos constructos reflectivos
# (A, B) que se pasan al primer constructo de orden superior reflectivo.
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
    step("select A", lambda: item(opts, "A").click(timeout=5000))
    step("A -> Construct 1", lambda: transfer.nth(0).click(timeout=5000))
    step("add reflective construct", lambda: opts.get_by_text("Add New Construct", exact=True).first.click(timeout=5000))
    step("focus Construct 2 indicators", lambda: opts.get_by_text("drag indicators here", exact=True).last.click(timeout=5000))
    step("select B", lambda: item(opts, "B").click(timeout=5000))
    step("B -> Construct 2", lambda: transfer.nth(0).click(timeout=5000))
    time.sleep(5)
    state(opts, page, "h1-constructs")
    step("open Higher-Order Constructs", lambda: opts.get_by_text("Higher-Order Constructs", exact=True).first.click(timeout=5000))
    time.sleep(3)
    state(opts, page, "h2-box-open")
    step("select Construct 1 in the higher-order supplier", lambda: item(opts, "Construct 1").click(timeout=5000))
    step("Construct 1 -> higher-order (no focus)", lambda: transfer.nth(2).click(timeout=5000))
    state(opts, page, "h3-first-moved")
    step("select Construct 2 in the higher-order supplier", lambda: item(opts, "Construct 2").click(timeout=5000))
    step("Construct 2 -> higher-order", lambda: transfer.nth(2).click(timeout=5000))
    time.sleep(5)
    state(opts, page, "h4-both-moved")
    log.append("options: " + json.dumps(opts.evaluate("""() => [...document.querySelectorAll('.silky-option-listbox')].map(e => e.innerText.replace(/\\n+/g, ' / ').slice(0, 200))"""), ensure_ascii=False)[:2500])
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
