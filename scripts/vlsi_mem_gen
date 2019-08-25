#! /usr/bin/env python

# See LICENSE.SiFive for license details.
# See LICENSE.Berkeley for license details.

import sys
import math

use_latches = 0
blackbox = 0

def parse_line(line):
  name = ''
  width = 0
  depth = 0
  ports = ''
  mask_gran = 0
  tokens = line.split()
  i = 0
  for i in range(0,len(tokens),2):
    s = tokens[i]
    if s == 'name':
      name = tokens[i+1]
    elif s == 'width':
      width = int(tokens[i+1])
      mask_gran = width # default setting
    elif s == 'depth':
      depth = int(tokens[i+1])
    elif s == 'ports':
      ports = tokens[i+1].split(',')
    elif s == 'mask_gran':
      mask_gran = int(tokens[i+1])
    else:
      sys.exit('%s: unknown argument %s' % (sys.argv[0], a))
  return (name, width, depth, mask_gran, width//mask_gran, ports)

def gen_mem(name, width, depth, mask_gran, mask_seg, ports):
  addr_width = max(math.ceil(math.log(depth)/math.log(2)),1)
  port_spec = []
  readports = []
  writeports = []
  latchports = []
  rwports = []
  decl = []
  combinational = []
  sequential = []
  maskedports = {}
  for pid in range(len(ports)):
    ptype = ports[pid]
    if ptype[0:1] == 'm':
      ptype = ptype[1:]
      maskedports[pid] = pid

    if ptype == 'read':
      prefix = 'R%d_' % len(readports)
      port_spec.append('input %sclk' % prefix)
      port_spec.append('input [%d:0] %saddr' % (addr_width-1, prefix))
      port_spec.append('input %sen' % prefix)
      port_spec.append('output [%d:0] %sdata' % (width-1, prefix))
      readports.append(pid)
    elif ptype == 'write':
      prefix = 'W%d_' % len(writeports)
      port_spec.append('input %sclk' % prefix)
      port_spec.append('input [%d:0] %saddr' % (addr_width-1, prefix))
      port_spec.append('input %sen' % prefix)
      port_spec.append('input [%d:0] %sdata' % (width-1, prefix))
      if pid in maskedports:
        port_spec.append('input [%d:0] %smask' % (mask_seg-1, prefix))
      if not use_latches or pid in maskedports:
        writeports.append(pid)
      else:
        latchports.append(pid)
    elif ptype == 'rw':
      prefix = 'RW%d_' % len(rwports)
      port_spec.append('input %sclk' % prefix)
      port_spec.append('input [%d:0] %saddr' % (addr_width-1, prefix))
      port_spec.append('input %sen' % prefix)
      port_spec.append('input %swmode' % prefix)
      if pid in maskedports:
        port_spec.append('input [%d:0] %swmask' % (mask_seg-1, prefix))
      port_spec.append('input [%d:0] %swdata' % (width-1, prefix))
      port_spec.append('output [%d:0] %srdata' % (width-1, prefix))
      rwports.append(pid)
    else:
      sys.exit('%s: unknown port type %s' % (sys.argv[0], ptype))

  nr = len(readports)
  nw = len(writeports)
  nrw = len(rwports)
  masked = len(maskedports)>0
  tup = (depth, width, nr, nw, nrw, masked)

  def emit_read(idx, rw):
    prefix = ('RW%d_' if rw else 'R%d_') % idx
    data = ('%srdata' if rw else '%sdata') % prefix
    en = ('%sen && !%swmode' % (prefix, prefix)) if rw else ('%sen' % prefix)
    decl.append('reg reg_%sren;' % prefix)
    decl.append('reg [%d:0] reg_%saddr;' % (addr_width-1, prefix))
    sequential.append('always @(posedge %sclk)' % prefix)
    sequential.append('  reg_%sren <= %s;' % (prefix, en))
    sequential.append('always @(posedge %sclk)' % prefix)
    sequential.append('  if (%s) reg_%saddr <= %saddr;' % (en, prefix, prefix))
    combinational.append('`ifdef RANDOMIZE_GARBAGE_ASSIGN')
    combinational.append('reg [%d:0] %srandom;' % (((width-1)//32+1)*32-1, prefix))
    combinational.append('`ifdef RANDOMIZE_MEM_INIT')
    combinational.append('  initial begin')
    combinational.append('    #`RANDOMIZE_DELAY begin end')
    combinational.append('    %srandom = {%s};' % (prefix, ', '.join(['$random'] * ((width-1)//32+1))))
    combinational.append('    reg_%sren = %srandom[0];' % (prefix, prefix))
    combinational.append('  end')
    combinational.append('`endif')
    combinational.append('always @(posedge %sclk) %srandom <= {%s};' % (prefix, prefix, ', '.join(['$random'] * ((width-1)//32+1))))
    combinational.append('assign %s = reg_%sren ? ram[reg_%saddr] : %srandom[%d:0];' % (data, prefix, prefix, prefix, width-1))
    combinational.append('`else')
    combinational.append('assign %s = ram[reg_%saddr];' % (data, prefix))
    combinational.append('`endif')

  for idx in range(nr):
    emit_read(idx, False)

  for idx in range(nrw):
    emit_read(idx, True)

  for idx in range(len(latchports)):
    prefix = 'W%d_' % idx
    decl.append('reg [%d:0] latch_%saddr;' % (addr_width-1, prefix))
    decl.append('reg [%d:0] latch_%sdata;' % (width-1, prefix))
    decl.append('reg latch_%sen;' % (prefix))
    combinational.append('always @(*) begin')
    combinational.append('  if (!%sclk && %sen) latch_%saddr <= %saddr;' % (prefix, prefix, prefix, prefix))
    combinational.append('  if (!%sclk && %sen) latch_%sdata <= %sdata;' % (prefix, prefix, prefix, prefix))
    combinational.append('  if (!%sclk) latch_%sen <= %sen;' % (prefix, prefix, prefix))
    combinational.append('end')
    combinational.append('always @(*)')
    combinational.append('  if (%sclk && latch_%sen)' % (prefix, prefix))
    combinational.append('    ram[latch_%saddr] <= latch_%sdata;' % (prefix, prefix))

  decl.append('reg [%d:0] ram [%d:0];' % (width-1, depth-1))
  decl.append('`ifdef RANDOMIZE_MEM_INIT')
  decl.append('  integer initvar;')
  decl.append('  initial begin')
  decl.append('    #`RANDOMIZE_DELAY begin end')
  decl.append('    for (initvar = 0; initvar < %d; initvar = initvar+1)' % depth)
  decl.append('      ram[initvar] = {%d {$random}};' % ((width-1)//32+1))
  for idx in range(nr):
    prefix = 'R%d_' % idx
    decl.append('    reg_%saddr = {%d {$random}};' % (prefix, ((addr_width-1)//32+1)))
  for idx in range(nrw):
    prefix = 'RW%d_' % idx
    decl.append('    reg_%saddr = {%d {$random}};' % (prefix, ((addr_width-1)//32+1)))
  decl.append('  end')
  decl.append('`endif')

  decl.append("integer i;")
  for idx in range(nw):
    prefix = 'W%d_' % idx
    pid = writeports[idx]
    sequential.append('always @(posedge %sclk)' % prefix)
    sequential.append("  if (%sen) begin" % prefix)
    for i in range(mask_seg):
      mask = ('if (%smask[%d]) ' % (prefix, i)) if pid in maskedports else ''
      ram_range = '%d:%d' % ((i+1)*mask_gran-1, i*mask_gran)
      sequential.append("    %sram[%saddr][%s] <= %sdata[%s];" % (mask, prefix, ram_range, prefix, ram_range))
    sequential.append("  end")
  for idx in range(nrw):
    pid = rwports[idx]
    prefix = 'RW%d_' % idx
    sequential.append('always @(posedge %sclk)' % prefix)
    sequential.append("  if (%sen && %swmode) begin" % (prefix, prefix))
    if mask_seg > 0:
      sequential.append("    for(i=0;i<%d;i=i+1) begin" % mask_seg)
      if pid in maskedports:
        sequential.append("      if(%swmask[i]) begin" % prefix)
        sequential.append("        ram[%saddr][i*%d +: %d] <= %swdata[i*%d +: %d];" %(prefix, mask_gran, mask_gran, prefix, mask_gran, mask_gran))
        sequential.append("      end")
      else:
        sequential.append("      ram[%saddr][i*%d +: %d] <= %swdata[i*%d +: %d];" %(prefix, mask_gran, mask_gran, prefix, mask_gran, mask_gran))
      sequential.append("    end")
    sequential.append("  end")
  body = "\
  %s\n\
  %s\n\
  %s\n" % ('\n  '.join(decl), '\n  '.join(sequential), '\n  '.join(combinational))

  s = "\nmodule %s(\n\
  %s\n\
);\n\
\n\
%s\
\n\
endmodule" % (name, ',\n  '.join(port_spec), body if not blackbox else "")
  return s

def main(args):
  f = open(args.output_file, "w") if (args.output_file) else None
  conf_file = args.conf
  for line in open(conf_file):
    parsed_line = gen_mem(*parse_line(line))
    if f is not None:
        f.write(parsed_line)
    else:
        print(parsed_line)

if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser(description='Memory generator for Rocket Chip')
  parser.add_argument('conf', metavar='.conf file')
  parser.add_argument('--blackbox', '-b', action='store_true', help='set to disable output of module body')
  #parser.add_argument('--use_latches', '-l', action='store_true', help='set to enable use of latches')
  parser.add_argument('--output_file', '-o', help='name of output file, default is stdout')
  args = parser.parse_args()
  blackbox = args.blackbox
  #use_latches = args.use_latches
  main(args)
