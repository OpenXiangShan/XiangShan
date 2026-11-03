"""Embed font subsets covering the current document and printable ASCII."""
from pathlib import Path
import io
import os
import sys
import uuid
import zipfile
import xml.etree.ElementTree as E

# Keep the document export dependency separate from the system Python.
sys.path.append(str(Path.home() / '.local/share/frontend-plan-python'))
from fontTools import subset
from fontTools.ttLib import TTFont

assets = Path(__file__).resolve().parent
p = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else assets.parent / 'Frontend_BT_验证方案_v1.2.docx'
with zipfile.ZipFile(p) as z:
    parts = {n: z.read(n) for n in z.namelist()}

ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
w = '{' + ns['w'] + '}'
required = set(range(0x20, 0x7f))
for name, data in parts.items():
    if name.startswith('word/') and name.endswith('.xml'):
        for t in E.fromstring(data).findall('.//w:t', ns):
            required.update(ord(c) for c in (t.text or '') if not c.isspace())

font_root = Path(os.environ.get(
    'FRONTEND_PLAN_FONT_DIR', str(Path.home() / '.local/share/frontend-plan-fonts')))
font_table = E.fromstring((assets / 'embedded-fontTable.xml').read_bytes())
for entry in font_table:
    family = 'NotoSerifCJK' if 'Serif' in entry.get(w + 'name') else 'NotoSansCJK'
    for tag, style in [('embedRegular', 'Regular'), ('embedBold', 'Bold')]:
        embedding = entry.find(w + tag)
        key = uuid.UUID(embedding.get(w + 'fontKey')).bytes[::-1]
        target = assets / 'fonts' / f'{family}-{style}.odttf'
        data = bytearray(target.read_bytes())
        for i in range(32):
            data[i] ^= key[i % 16]
        with TTFont(io.BytesIO(data)) as cached:
            missing = required - set(cached.getBestCmap())
        if not missing:
            continue
        source = font_root / f'{family}-{style}.ttc'
        if not source.exists():
            source = Path('/usr/share/fonts/opentype/noto') / source.name
        # Collection face 2 is Simplified Chinese in the Noto CJK collections.
        with TTFont(source, fontNumber=2) as font:
            unsupported = required - set(font.getBestCmap())
            if unsupported:
                raise RuntimeError(f'{source}: unsupported characters {sorted(unsupported)}')
            options = subset.Options()
            options.name_IDs = ['*']
            options.name_languages = ['*']
            options.notdef_glyph = True
            options.notdef_outline = True
            subsetter = subset.Subsetter(options=options)
            subsetter.populate(unicodes=required)
            subsetter.subset(font)
            output = io.BytesIO()
            font.save(output)
        data = bytearray(output.getvalue())
        for i in range(32):
            data[i] ^= key[i % 16]
        target.write_bytes(data)
        print(f'Updated {family}-{style}: added {len(missing)} missing characters')

for name, data in list(parts.items()):
    if name.startswith('word/') and name.endswith('.xml'):
        parts[name] = data.decode().replace('="宋体"', '="Noto Serif CJK SC"').replace('="黑体"', '="Noto Sans CJK SC"').encode()
parts['word/fontTable.xml'] = (assets / 'embedded-fontTable.xml').read_bytes()
parts['word/_rels/fontTable.xml.rels'] = (assets / 'embedded-fontTable.xml.rels').read_bytes()
for f in (assets / 'fonts').glob('*.odttf'):
    parts['word/fonts/' + f.name] = f.read_bytes()
s = parts['[Content_Types].xml'].decode()
if 'Extension="odttf"' not in s:
    s = s.replace('</Types>', '<Default Extension="odttf" ContentType="application/vnd.openxmlformats-officedocument.obfuscatedFont"/></Types>')
parts['[Content_Types].xml'] = s.encode()
s = parts['word/settings.xml'].decode()
if '<w:embedTrueTypeFonts' not in s:
    s = s.replace('</w:settings>', '<w:embedTrueTypeFonts/><w:saveSubsetFonts/></w:settings>')
parts['word/settings.xml'] = s.encode()
with zipfile.ZipFile(p, 'w', zipfile.ZIP_DEFLATED) as z:
    for name, data in parts.items():
        z.writestr(name, data)
