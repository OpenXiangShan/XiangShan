from pathlib import Path
from PIL import Image,ImageDraw,ImageFont
import os,re,subprocess,sys,tempfile
root=Path(__file__).resolve().parent.parent
assets=root/'frontend_plan_v1.2_assets';assets.mkdir(exist_ok=True)
font=ImageFont.truetype('/usr/share/fonts/truetype/droid/DroidSansFallbackFull.ttf',21)
latin=ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf',21)
def glyph(c): return latin if ord(c)<128 or c in "→×" else font
def width(t): return sum(glyph(c).getlength(c) for c in t)
def write(d,xy,t):
 x,y=xy
 for c in t:
  d.text((x,y),c,font=glyph(c),fill='#172d45');x+=glyph(c).getlength(c)
def wrap(t,maxw):
 lines=[]
 for row in t.split('\n'):
  line=''
  for c in row:
   if width(line+c)>maxw:lines.append(line);line=''
   line+=c
  lines.append(line)
 return lines
def draw_diagram(name,nodes,edges):
 im=Image.new('RGB',(1500,830),'white');d=ImageDraw.Draw(im)
 for a,b,label in edges:
  ax,ay,aw,ah,_=nodes[a];bx,by,bw,bh,_=nodes[b]
  x1,y1=ax+aw/2,ay+ah;x2,y2=bx+bw/2,by
  if by<=ay:x1,y1=ax+aw,ay+ah/2;x2,y2=bx,by+bh/2
  d.line([(x1,y1),(x2,y2)],fill='#4a617a',width=3)
  import math
  ang=math.atan2(y2-y1,x2-x1); size=13
  d.polygon([(x2,y2),(x2-size*math.cos(ang-.5),y2-size*math.sin(ang-.5)),(x2-size*math.cos(ang+.5),y2-size*math.sin(ang+.5))],fill='#4a617a')
  if label:d.text(((x1+x2)/2,(y1+y2)/2),label,font=font,fill='#334155')
 for x,y,w,h,t in nodes.values():
  d.rounded_rectangle((x,y,x+w,y+h),radius=12,fill='#eef5fc',outline='#486b90',width=2)
  lines=wrap(t,w-28); yy=y+(h-len(lines)*27)/2
  for line in lines:
   tw=width(line);write(d,(x+(w-tw)/2,yy),line);yy+=27
 p=assets/name;im.save(p);return p
n={
 'case':(510,15,480,90,'pytest 定向用例 / 随机用例\n程序、配置、场景目标'),
 'env':(510,150,480,85,'FrontendEnv / sequence\n统一装载、控制和事件调度'),
 'ic':(25,305,310,110,'ICache / Uncache agent\nMemoryModel\n下游响应与时序'),
 'ptw':(385,305,310,110,'PTWAgent\nPageTableModel\n页表响应与权限'),
 'be':(745,305,310,110,'BackendModel / Agent\nFTQ 身份、反馈\nresolve / commit / redirect'),
 'ctrl':(1105,305,350,110,'CSR / PMP / PMA\nreset / SFENCE / WFI\n控制与配置'),
 'dut':(255,510,990,95,'V3 Frontend DUT\nBPU → FTQ → IFU + ICache/ITLB/InstrUncache → IBuffer'),
 'obs':(255,690,440,110,'Monitor / Checker / 局部模型\nPC、指令、身份、异常、恢复'),
 'cov':(820,690,425,110,'Coverage sampler / artifact\n测试点、有效证据、来源签名')}
a=draw_diagram('frontend_architecture.png',n,[('case','env',''),* [('env',x,'') for x in ['ic','ptw','be','ctrl']],*[(x,'dut','') for x in ['ic','ptw','be','ctrl']],('dut','obs',''),('obs','cov','')])
n={
 'program':(510,15,480,90,'ASM / 随机程序 → .bin\n启动 PC、地址布局、配置、seed'),
 'mem':(80,175,530,100,'load_program / load_program_file\nMemoryModel 指令字节与地址映射'),
 'gold':(895,175,530,100,'NEMU 执行 → golden trace\n正确执行的 PC、指令和控制流'),
 'agents':(80,350,530,105,'存储 / PTW 响应 + CSR 控制\n按 DUT 请求施加延迟、反压、错误'),
 'back':(895,350,530,105,'BackendModel / Agent\n按 FTQ 身份和参考控制流生成反馈'),
 'dut':(80,520,530,100,'Frontend DUT 自主预测与取指\nIFU 预译码、修正 → IBuffer'),
 'check':(895,520,530,100,'Monitor + BranchChecker + 局部检查\n有效消费、异常、握手和恢复'),
 'result':(485,710,560,90,'目标事件 + checker/断言 + 有效覆盖证据\n形成用例结果与可重放 artifact')}
b=draw_diagram('frontend_stimulus_flow.png',n,[('program','mem',''),('program','gold',''),('mem','agents',''),('gold','back',''),('agents','dut',''),('back','dut',''),('dut','check',''),('gold','check',''),('check','result','')])
s=(root/'Frontend_BT_验证方案_v1.2.md').read_text()
blocks=list(re.finditer(r'```mermaid\n.*?```',s,re.S));assert len(blocks)==2
for m,p in reversed(list(zip(blocks,[a,b]))):
 s=s[:m.start()]+'!['+('Frontend BT 验证架构' if p==a else '程序、golden 与接口激励流程')+']('+str(p)+'){width=6.5in}\n'+s[m.end():]
repo_root = Path(subprocess.check_output(
    ['git', '-C', str(root), 'rev-parse', '--show-toplevel'], text=True
).strip())
scratch_root = repo_root / '.cache/document-exports'
scratch_root.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(prefix='frontend-plan-word-', dir=scratch_root) as temporary:
    temp_word_md = Path(temporary) / 'frontend-plan-word.md'
    temp_word_md.write_text(s)
    subprocess.run(['pandoc',str(temp_word_md),'--from=markdown','--to=docx','--reference-doc='+str(root.parent/'references'/'memblock验证方案.docx'),'-o',str(root/'Frontend_BT_验证方案_v1.2.docx')],check=True, env={**os.environ, 'TMPDIR': str(scratch_root)})
print(root/'Frontend_BT_验证方案_v1.2.docx')



subprocess.run([sys.executable, str(assets / "apply_memblock_format.py")], check=True)

subprocess.run([sys.executable, str(assets / "embed_fonts.py")], check=True)
