from pathlib import Path
from copy import deepcopy
import sys,zipfile,xml.etree.ElementTree as E
root=Path(__file__).resolve().parent.parent
w='http://schemas.openxmlformats.org/wordprocessingml/2006/main';ns={'w':w};q=lambda t:'{'+w+'}'+t
refzip=zipfile.ZipFile(root.parent/'references'/'memblock验证方案.docx');ref=E.fromstring(refzip.read('word/document.xml'))
p=Path(sys.argv[1]).resolve() if len(sys.argv)>1 else root/'Frontend_BT_验证方案_v1.2.docx'
z=zipfile.ZipFile(p);parts={n:z.read(n) for n in z.namelist()};z.close();doc=E.fromstring(parts['word/document.xml']);body=doc.find('w:body',ns)
def text(el):return ''.join(t.text or '' for t in el.findall('.//w:t',ns))
def assign(el,name,source):
 old=el.find('w:'+name,ns)
 if old is not None:el.remove(old)
 if source is not None:el.insert(0,deepcopy(source))
refparas=ref.findall('w:body/w:p',ns)
normal=next(p for p in refparas if text(p).startswith('MemBlock是'))
head={}
for p0 in refparas:
 st=p0.find('w:pPr/w:pStyle',ns)
 if st is not None:head.setdefault(st.get(q('val')),p0)
meta=next(p for p in refparas if text(p).startswith('文档版本'))
for para in list(body.findall('w:p',ns)):
 if text(para).startswith('文档版本'):
  segs=[[]]
  for run in para.findall('w:r',ns):
   if run.find('w:br',ns) is not None:segs.append([])
   else:segs[-1].append(run)
  idx=list(body).index(para);body.remove(para)
  for seg in segs:
   pn=E.Element(q('p'))
   assign(pn,'pPr',meta.find('w:pPr',ns))
   for run in seg:pn.append(run)
   body.insert(idx,pn);idx+=1
for i,para in enumerate(body.findall('w:p',ns)):
 st=para.find('w:pPr/w:pStyle',ns);sid=st.get(q('val')) if st is not None else '';t=text(para)
 if i==0:template=refparas[0]
 elif sid in ['3','4','5','6','7']:template=head.get(str(int(sid)-1),head['4'])
 elif para.find('.//w:drawing',ns) is not None:
  pr=E.Element(q('pPr'));E.SubElement(pr,q('jc'),{q('val'):'center'});assign(para,'pPr',pr);continue
 elif t.startswith(('文档版本','创建日期','修订日期','适用对象','验证层级','验证方法学','维护人')):template=meta
 elif para.find('w:pPr/w:numPr',ns) is not None:
  # Preserve numbering indents; inherit the reference paragraph spacing.
  oldpr=para.find('w:pPr',ns)
  assign(oldpr,'spacing',normal.find('w:pPr/w:spacing',ns));continue
 else:template=normal
 assign(para,'pPr',template.find('w:pPr',ns))
 source_format=template.find('w:r/w:rPr',ns)
 for run in para.findall('.//w:r',ns):
  old=run.find('w:rPr',ns)
  bold=old is not None and old.find('w:b',ns) is not None
  italic=old is not None and old.find('w:i',ns) is not None
  assign(run,'rPr',source_format)
  if template is normal:
   rp=run.find('w:rPr',ns)
   if bold:
    b=rp.find('w:b',ns)
    if b is not None:b.set(q('val'),'1')
   if italic:
    b=rp.find('w:i',ns)
    if b is not None:b.set(q('val'),'1')
 pr=para.find('w:pPr',ns)
 if template is normal:
  st=pr.find('w:pStyle',ns)
  if st is not None:pr.remove(st)
 if sid in ['3','4','5','6','7']:
  if pr.find('w:keepNext',ns) is None:E.SubElement(pr,q('keepNext'))
# Match the reference's separate module labels and scenario labels.
label_templates={text(x):x for x in refparas if text(x) in ('模块功能','测试场景','接口交互')}
for para in list(body.findall('w:p',ns)):
 t=text(para)
 if t.startswith('模块功能：'):
  idx=list(body).index(para)
  body.insert(idx,deepcopy(label_templates['模块功能']))
  remaining=t[len('模块功能：'):]
  for node in list(para):
   if node.tag!=q('pPr'):para.remove(node)
  assign(para,'pPr',normal.find('w:pPr',ns))
  run=E.SubElement(para,q('r'));assign(run,'rPr',normal.find('w:r/w:rPr',ns))
  E.SubElement(run,q('t')).text=remaining
 elif t=='接口交互':
  idx=list(body).index(para);body.remove(para);body.insert(idx,deepcopy(label_templates['接口交互']))
for table in list(body.findall('w:tbl',ns)):
 first=table.find('w:tr/w:tc',ns)
 if first is not None and text(first)=='场景编号':
  body.insert(list(body).index(table),deepcopy(label_templates['测试场景']))

# Use actual table formatting and column widths from the reference.
reftables=ref.findall('.//w:tbl',ns)
for table in doc.findall('.//w:tbl',ns):
 rows=table.findall('w:tr',ns);cols=len(rows[0].findall('w:tc',ns))
 candidates=[t for t in reftables if len(t.find('w:tr',ns).findall('w:tc',ns))==cols]
 targetheaders=[text(c) for c in rows[0].findall('w:tc',ns)]
 template=max(candidates,key=lambda t:sum(a==b for a,b in zip(targetheaders,[text(c) for c in t.find('w:tr',ns).findall('w:tc',ns)]))) if candidates else reftables[0]
 assign(table,'tblPr',template.find('w:tblPr',ns))
 if candidates:widths=[int(c.get(q('w'))) for c in template.find('w:tblGrid',ns)]
 else:widths=[9638//cols]*cols;widths[-1]+=9638-sum(widths)
 # Long descriptions receive most width rather than equal-width columns.

 grid=E.Element(q('tblGrid'))
 for width in widths:E.SubElement(grid,q('gridCol'),{q('w'):str(width)})
 assign(table,'tblGrid',grid)
 # OOXML order is tblPr then tblGrid.
 table.remove(grid) if grid in list(table) else None
 current=table.find('w:tblGrid',ns);table.remove(current);table.insert(1,current)
 refrows=template.findall('w:tr',ns)
 for ri,row in enumerate(rows):
  assign(row,'trPr',refrows[min(ri,1)].find('w:trPr',ns))
  rp=row.find('w:trPr',ns)
  if rp is None:rp=E.Element(q('trPr'));row.insert(0,rp)
  if ri==0 and rp.find('w:tblHeader',ns) is None:E.SubElement(rp,q('tblHeader'))
  cells=row.findall('w:tc',ns);refcells=refrows[min(ri,1)].findall('w:tc',ns)
  for ci,cell in enumerate(cells):
   source=refcells[min(ci,len(refcells)-1)]
   assign(cell,'tcPr',source.find('w:tcPr',ns));cp=cell.find('w:tcPr',ns)
   tw=cp.find('w:tcW',ns)
   if tw is None:tw=E.SubElement(cp,q('tcW'))
   tw.set(q('type'),'dxa');tw.set(q('w'),str(widths[ci]))
   for para in cell.findall('w:p',ns):
    sourcep=source.find('w:p',ns);assign(para,'pPr',sourcep.find('w:pPr',ns))
    source_run=(reftables[0].findall('w:tr',ns)[0 if ri==0 else 1].find('w:tc/w:p/w:r/w:rPr',ns))
    for run in para.findall('w:r',ns):assign(run,'rPr',source_run)
# Chapter headings now use the exact reference levels. Update TOC depth to match.
for inst in doc.findall('.//w:instrText',ns):
 if inst.text and 'TOC' in inst.text:inst.text='TOC \\o "1-3" \\h \\z \\u'
for name,data in list(parts.items()):
 if name.startswith(('word/header','word/footer')) and name.endswith('.xml'):
  node=E.fromstring(data)
  for t in node.findall('.//w:t',ns):
   if t.text:
    if name.startswith('word/header') and 'MemBlock' in t.text and p.name!='Frontend_BT_验证方案_v1.2.docx':
     t.text=text(body.find('w:p',ns))
    else:t.text=t.text.replace('MemBlock','Frontend')
  node.attrib.pop('{http://schemas.openxmlformats.org/markup-compatibility/2006}Ignorable',None)
  parts[name]=E.tostring(node,encoding='utf-8',xml_declaration=True)
doc.attrib.pop('{http://schemas.openxmlformats.org/markup-compatibility/2006}Ignorable',None)
styles=E.fromstring(parts['word/styles.xml'])
known={x.get(q('styleId')) for x in styles.findall('w:style',ns)}
for sid in ['Compact','TOCHeading']:
 if sid not in known:
  el=E.SubElement(styles,q('style'),{q('type'):'paragraph',q('styleId'):sid})
  E.SubElement(el,q('name'),{q('val'):sid})
  E.SubElement(el,q('basedOn'),{q('val'):'1'})
styles.attrib.pop('{http://schemas.openxmlformats.org/markup-compatibility/2006}Ignorable',None)
parts['word/styles.xml']=E.tostring(styles,encoding='utf-8',xml_declaration=True)
parts['word/document.xml']=E.tostring(doc,encoding='utf-8',xml_declaration=True)
tmp=p.with_suffix('.tmp.docx')
with zipfile.ZipFile(tmp,'w',zipfile.ZIP_DEFLATED) as zz:
 for n,data in parts.items():zz.writestr(n,data)
tmp.replace(p)
print('Applied reference paragraph, heading and table formats across the whole document.')
