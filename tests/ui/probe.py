# Opens PLS-SEM in a real jamovi (docker) with a headless browser and records
# console messages, screenshots and the options panel HTML, to see how the
# panel behaves without a desktop jamovi.
# ES: Abre PLS-SEM en un jamovi real (docker) con un navegador sin pantalla y
# guarda la consola, capturas y el HTML del panel de opciones.
import json, sys, time
from playwright.sync_api import sync_playwright

OUT = sys.argv[1] if len(sys.argv) > 1 else "probe-out"
log = []

def shot(page, name):
    page.screenshot(path=f"{OUT}/{name}.png", full_page=True)

def frames_dump(page, name):
    for i, f in enumerate(page.frames):
        try:
            html = f.content()
        except Exception as e:
            html = f"<!-- {e} -->"
        with open(f"{OUT}/{name}-frame{i}.html", "w") as fh:
            fh.write(f"<!-- {f.url} -->\n" + html)

def click_text(scope, texts, timeout=8000):
    for t in texts:
        loc = scope.get_by_text(t, exact=True)
        try:
            loc.first.click(timeout=timeout)
            log.append(f"clicked {t!r}")
            return True
        except Exception as e:
            log.append(f"could not click {t!r}: {e.__class__.__name__}")
    return False

with sync_playwright() as p:
    browser = p.chromium.launch()
    page = browser.new_page(viewport={"width": 1400, "height": 1000})
    page.on("console", lambda m: log.append(f"[console.{m.type}] {m.text}"))
    page.on("pageerror", lambda e: log.append(f"[pageerror] {e}"))
    page.goto("http://127.0.0.1:41337/", wait_until="load")
    time.sleep(20)
    shot(page, "01-start")
    click_text(page, ["Analyses", "Análisis"])
    time.sleep(2)
    click_text(page, ["PLS-SEM Lab"])
    time.sleep(2)
    shot(page, "02-menu")
    # the menu item of the analysis
    items = page.get_by_text("PLS-SEM", exact=True)
    log.append(f"PLS-SEM items: {items.count()}")
    for i in range(items.count()):
        try:
            items.nth(i).click(timeout=3000)
            log.append(f"clicked PLS-SEM item {i}")
            break
        except Exception as e:
            log.append(f"item {i}: {e.__class__.__name__}")
    time.sleep(15)
    shot(page, "03-analysis")
    frames_dump(page, "03")
    log.append("frames: " + json.dumps([f.url for f in page.frames]))
    browser.close()

with open(f"{OUT}/console.txt", "w") as fh:
    fh.write("\n".join(log))
print("\n".join(log))
