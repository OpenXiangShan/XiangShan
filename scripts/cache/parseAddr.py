#!/bin/python3
import argparse

blockBits = 6

# create a basic class with three variables: tagBits, setBits, bankBits
class Addr:
    def __init__(self, tagBits, setBits, bankBits):
        self.tagBits  = tagBits
        self.setBits  = setBits
        self.bankBits = bankBits
    def fullAddr(self, tag, set, bank):
        return ((tag << (self.setBits + self.bankBits)) | (set << self.bankBits) | bank) << blockBits
    def sepAddr(self, addr):
        addr = addr >> blockBits
        return (addr >> (self.setBits + self.bankBits), (addr >> self.bankBits) & ((1 << self.setBits) - 1), addr & ((1 << self.bankBits) - 1))
    def sepAddrHex(self, addr):
        tmp = self.sepAddr(addr)
        return (hex(tmp[0]), hex(tmp[1]), hex(tmp[2]))

tl_test = Addr(3, 7, 0)
sys_l2  = Addr(19, 9, 2)
sys_l3  = Addr(16, 12, 2)
seq = [None, tl_test, sys_l2, sys_l3]

### main ###
parser = argparse.ArgumentParser(description='0: fullAddr, 1: tl_test, 2: sys_l2, 3: sys_l3')
parser.add_argument('cmd', help='e.g., 02=fullAddr to l2[tag set bank] of XS')
parser.add_argument('addr', nargs='*', default=None, help='addr OR tag set bank')
args = parser.parse_args()
cmd = args.cmd
addr = args.addr

i = cmd[0] # input
o = cmd[1] # output
ii = seq[int(i)]
oo = seq[int(o)]

if i == '0':
    assert(len(addr) == 1)
    fullAddr = int(addr[0], 16)
    print(oo.sepAddrHex(fullAddr))
else:
    assert(len(addr) == 3)
    tag = int(addr[0], 16)
    set = int(addr[1], 16)
    bank = int(addr[2], 16)
    fullAddr = ii.fullAddr(tag, set, bank)
    print(hex(fullAddr))
    if o != '0':
        print(oo.sepAddrHex(fullAddr))

# examples:
# fullAddr to L2:
# > python3 parseAddr.py 02 0xc5170cc0
# ('0x628b', '0x10c', '0x3')
#
# L2 to L3:
# > python3 parseAddr.py 23 0x628b 0x10c 0x3
# 0xc5170cc0
# ('0xc51', '0x70c', '0x3')

