#!/usr/bin/env python3
import sys
import os
import argparse

def find_lateset(suffix):
    fs = [f for f in os.listdir(xshome + '/build') if f.endswith(suffix)]
    fs.sort()
    return fs[-1]

def parse_db(table_name, script):
    return (f'sqlite3 {db} "select * from (' +
    f'select * from {table_name} ' +
    (f'where {sql} ' if(sql != '') else '') +
    (f'order by STAMP desc ' if(last) else '') +
    f'limit {limit}' +
    f') order by STAMP asc" | sh {script}')

parser = argparse.ArgumentParser(description='L2 helper')
parser.add_argument('cmd', choices=['log', 'mp'], help='log for TLLOG; mp for L2 MainPipe')
parser.add_argument('sql', nargs='?', default='', help='sql query')
parser.add_argument('-l', '--last', action='store_true', help='select last N records')
parser.add_argument('-n', '--limit', type=int, default=20, help='select N records')
parser.add_argument('-p', '--path', default=None, help='path to db file')
args = parser.parse_args()
# print(args)

xshome = os.environ['NOOP_HOME']
print('XSHOME:',xshome)

cmd = args.cmd
sql = args.sql
last = args.last
limit = args.limit
db = './build/' + find_lateset('.db') if args.path == None else args.path
print(db)

line = ''
if(cmd == 'log'):
    line = parse_db('TLLOG', xshome + '/scripts/utils/convert.sh')

elif(cmd == 'mp'):
    line = parse_db('L2MP', xshome + '/scripts/utils/parser.sh')

# elif(cmd == 'clean'):
#     os.system('rm -rf *.vcd')
#     os.system('rm -rf *.gtkw')
#     os.system('rm -rf *.db')

else:
    print('Unknown command')
    exit()

print(line)
os.system(line)