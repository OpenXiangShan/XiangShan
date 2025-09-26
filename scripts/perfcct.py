import sqlite3 as sql
import argparse



parser = argparse.ArgumentParser()
parser.add_argument('sqldb')
parser.add_argument('-v', '--visual', action='store_true', default=False)
parser.add_argument('-z', '--zoom', action='store', type=float, default=1)
parser.add_argument('-p', '--period', action='store', default=1)

args = parser.parse_args()

sqldb = args.sqldb

tick_per_cycle = int(args.period)
cycle_per_line = int(100 * args.zoom)

stages = ['f','d','r','D','i','a','g','e','b','w','c']


def non_stage():
    return '.'

def stage(x):
    return stages[x]

def dump_visual(pos, records):
    pos_start = pos[0] % cycle_per_line
    line = ''
    line += '[' + non_stage() * pos_start
    pos_next = pos_start
    last_index = 0
    for i in range(1, len(pos)):
        if (pos[i] == pos[last_index]) or pos[i] == 0:
            continue
        if pos[i] - pos[last_index] >= cycle_per_line - pos_next:
            diff = cycle_per_line - pos_next
            line += f'{stage(last_index)}' * diff + ']\n'
            diff_line = ((pos[i] - pos[last_index]) - diff - 1) // cycle_per_line
            if diff_line > 0:
                line += '[' + f'{stage(last_index)}' * cycle_per_line + ']\n'

            pos_next = pos[i] % cycle_per_line
            line += '[' + f'{stage(last_index)}' * pos_next
        else:
            diff = pos[i] - pos[last_index]
            pos_next = pos[i] % cycle_per_line
            line += f'{stage(last_index)}' * diff
        last_index = i
    if cycle_per_line - pos_next == 0:
        line += ']\n'
        line += f'[{stage(i)}{non_stage() * (cycle_per_line - 1)}]\n'
    else:
        line += f'{stage(i)}' + non_stage() * (cycle_per_line - pos_next - 1) + ']'
    line += str(records)
    print(line)


def dump_txt(pos, records):
    for i in range(len(pos)):
        print(f'{stage(i)}{pos[i]}', end=' ')
    print(records)


dump = dump_txt
if args.visual:
    dump = dump_visual

with sql.connect(sqldb) as con:
    cur = con.cursor()
    cur.execute("SELECT * FROM LifeTimeCommitTrace")
    col_name = [i[0] for i in cur.description]
    col_name = col_name[1:]
    col_name = [i.lower() for i in col_name]
    rows = cur.fetchall()
    for row in rows:
        row = row[1:]
        pos = []
        records = []
        i = 0
        for val in row:
            if col_name[i].startswith('at'):
                pos.append(val//tick_per_cycle)
            elif col_name[i].startswith('pc'):
                if val < 0:
                    val = val + 1 << 64
                records.append(hex(val))
            else:
                records.append(val)
            i += 1
        dump(pos, records)