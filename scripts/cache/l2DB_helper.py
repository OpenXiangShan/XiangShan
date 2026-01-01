#!/usr/bin/env python3
import sys
import os
import argparse

xshome = os.environ['NOOP_HOME']
assert(xshome) # NOOP_HOME is set
print('XSHOME:',xshome)

xsbuild = xshome + '/build/'

def find_lateset(suffix):
    fs = [f for f in os.listdir(xsbuild) if f.endswith(suffix)]
    fs.sort()
    return fs[-1]

def parse_db(table_name, script):
    return (f'sqlite3 {db} "select * from (' +
    f'select * from {table_name} ' +
    (f'where {sql} ' if(sql != '') else '') +
    (f'order by STAMP desc ' if(last) else '') +
    f'limit {limit}' +
    f') order by STAMP asc" | sh {script}')

parser = argparse.ArgumentParser(description='L2DB helper')
parser.add_argument('cmd', choices=['log', 'mp'], help='[Required] log for TLLOG; mp for L2 MainPipe')
parser.add_argument('sql', nargs='?', default='', help='[Optional] sql query, e.g. "STAMP > 10000" or "ADDRESS=0x80000000"')
parser.add_argument('-l', '--last', action='store_true', help='select the last N records')
parser.add_argument('-n', '--limit', type=int, default=20, help='select N records')
parser.add_argument('-p', '--path', default=None, help='path to db file (if not designated, use the latest in build)')
args = parser.parse_args()
# print(args)

cmd = args.cmd
sql = args.sql
last = args.last
limit = args.limit
db = xsbuild + find_lateset('.db') if args.path == None else args.path
print(db)

line = ''
if(cmd == 'log'):
    line = parse_db('TLLOG', xshome + '/scripts/cache/convert_tllog.sh')

elif(cmd == 'mp'):
    line = parse_db('L2MP', xshome + '/scripts/cache/convert_mp.sh')

# elif(cmd == 'clean'):
#     os.system(f'rm -rf {xsbuild}*.vcd')
#     os.system(f'rm -rf {xsbuild}*.vpd')
#     os.system(f'rm -rf {xsbuild}*.gtkw')
#     os.system(f'rm -rf {xsbuild}*.db')

else:
    print('Unknown command')
    exit()

print(line)
os.system(line)