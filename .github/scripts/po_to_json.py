# Compile jamovi/i18n/<lang>.po into inst/i18n/<lang>.json (jed format, as
# shipped in this repository: sorted keys, indent 4, no trailing newline).
# ES: Compila jamovi/i18n/<lang>.po en inst/i18n/<lang>.json.
import json, sys

lang = sys.argv[1] if len(sys.argv) > 1 else "es"
text = open(f"jamovi/i18n/{lang}.po", encoding="utf-8").read()
entries, msgid, msgstr, cur, fuzzy = {}, [], [], None, False

def flush():
    global msgid, msgstr, fuzzy
    k, v = "".join(msgid), "".join(msgstr)
    if k and v and not fuzzy:
        entries[k] = [v]
    msgid, msgstr, fuzzy = [], [], False

for line in text.split("\n") + [""]:
    if line.startswith("#,") and "fuzzy" in line:
        fuzzy = True
    elif line.startswith("msgid "):
        if msgid or msgstr:
            flush()
        cur = msgid; msgid.append(json.loads(line[6:]))
    elif line.startswith("msgstr "):
        cur = msgstr; msgstr.append(json.loads(line[7:]))
    elif line.startswith('"') and cur is not None:
        cur.append(json.loads(line))
    elif not line.strip():
        if msgid or msgstr:
            flush()
        cur = None
flush()

m = {"": {"domain": "messages", "lang": lang, "plural_forms": "nplurals=2; plural=(n!=1);"}}
m.update(entries)
out = {"domain": "messages", "locale_data": {"messages": dict(sorted(m.items()))}}
with open(f"inst/i18n/{lang}.json", "w", encoding="utf-8") as f:
    f.write(json.dumps(out, ensure_ascii=False, indent=4))
print(f"{lang}.json: {len(entries)} entries")
