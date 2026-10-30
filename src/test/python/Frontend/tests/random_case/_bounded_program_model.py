"""Test-only subset interpreter for linked generator output, not an ISA oracle.

Unknown instructions, uninitialized loads, bad fetches and budget exhaustion fail.
No DUT/NEMU, external emulator or campaign generation is used by this helper.
"""
import re


def execute(program_map, *, max_steps=200000):
    by_pc={i['pc']:i for i in program_map['instructions']}
    registers=[0]*32;memory={};pc=program_map['labels']['_start'];trace=[]
    minimum_sp=None
    mask=(1<<64)-1
    def reg(token):
        if not re.fullmatch(r'x(?:[0-9]|[12][0-9]|3[01])',token):raise ValueError('unexpected register '+token)
        return int(token[1:])
    def read(token):return registers[reg(token)]
    def write(token,value):
        index=reg(token)
        if index:registers[index]=value&mask
    def number(token):return int(token,0)
    def target(token):return int(token.split()[0],16)
    def addr(token):
        m=re.fullmatch(r'(-?\d+)\((x\d+)\)',token)
        if not m:raise ValueError('unsupported memory operand '+token)
        return (read(m[2])+int(m[1]))&mask
    for step in range(max_steps):
        if pc not in by_pc:raise ValueError('fetch outside linked instructions '+hex(pc))
        i=by_pc[pc];mn=i['mnemonic'];op=[v.strip() for v in i['operands'].split(',') if v.strip()]
        next_pc=pc+i['size'];taken=False
        trace.append({'index':step,'pc':hex(pc),'instr':hex(int(i['encoding'],16)),'size':i['size'],'taken':False})
        if int(i['encoding'],16)==0x0005006b:
            if registers[10]!=0:raise ValueError('nonzero trap code')
            return {'trace':trace,'registers':registers,'memory':memory,'terminated':True,
                    'minimum_sp':registers[2] if minimum_sp is None else min(minimum_sp,registers[2])}
        if mn in ('addi','addiw'):
            value=read(op[1])+number(op[2])
            if mn=='addiw':value=((value&0xffffffff)^0x80000000)-0x80000000
            write(op[0],value)
        elif mn=='lui':
            value=number(op[1])<<12;write(op[0],((value&0xffffffff)^0x80000000)-0x80000000)
        elif mn=='auipc':write(op[0],pc+(number(op[1])<<12))
        elif mn in ('add','xor','srl'):
            a,b=read(op[1]),read(op[2]);write(op[0],a+b if mn=='add' else a^b if mn=='xor' else a>>(b&63))
        elif mn=='andi':write(op[0],read(op[1])&number(op[2]))
        elif mn in ('slli','srli'):write(op[0],read(op[1])<<number(op[2]) if mn=='slli' else read(op[1])>>number(op[2]))
        elif mn in ('c.li','c.addi','c.addiw'):
            value=number(op[1]) if mn=='c.li' else read(op[0])+number(op[1])
            if mn=='c.addiw':value=((value&0xffffffff)^0x80000000)-0x80000000
            write(op[0],value)
        elif mn=='c.mv':write(op[0],read(op[1]))
        elif mn=='c.xor':write(op[0],read(op[0])^read(op[1]))
        elif mn=='c.add':write(op[0],read(op[0])+read(op[1]))
        elif mn in ('sd','ld'):
            # Observe actual frames, not transient values while li sp expands.
            # Generated frames use x2-relative stores after allocating their space.
            if re.fullmatch(r'-?\d+\(x2\)',op[1]):
                minimum_sp=registers[2] if minimum_sp is None else min(minimum_sp,registers[2])
            address=addr(op[1])
            if address%8:raise ValueError('unaligned stack access')
            if mn=='sd':memory[address]=read(op[0])
            else:
                if address not in memory:raise ValueError('uninitialized stack load')
                write(op[0],memory[address])
        elif mn=='jal':write(op[0],next_pc);next_pc=target(op[1]);taken=True
        elif mn=='jalr':
            new=addr(op[1])&~1;write(op[0],next_pc);next_pc=new;taken=True
        elif mn in ('beq','bne','bltu'):
            a,b=read(op[0]),read(op[1]);taken=a==b if mn=='beq' else a!=b if mn=='bne' else a<b
            if taken:next_pc=target(op[2])
        elif mn in ('c.beqz','c.bnez'):
            taken=(read(op[0])==0) if mn=='c.beqz' else (read(op[0])!=0)
            if taken:next_pc=target(op[1])
        elif mn=='c.j':next_pc=target(op[0]);taken=True
        elif mn=='c.jr':next_pc=read(op[0])&~1;taken=True
        elif mn=='c.jalr':new=read(op[0])&~1;write('x1',next_pc);next_pc=new;taken=True
        elif mn=='c.addi4spn':write(op[0],read('x2')+number(op[-1]))
        elif mn=='c.nop':pass
        else:raise ValueError('unsupported instruction '+mn+' '+i['operands'])
        trace[-1]['taken']=taken;pc=next_pc
    raise ValueError('bounded program failed to terminate within step budget')
