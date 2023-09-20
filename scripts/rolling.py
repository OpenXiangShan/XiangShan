import sys
import argparse
import sqlite3
import matplotlib.pyplot as plt
import numpy as np


# usage: single db file
#   python3 rollingplot.py plot DB_FILE_PATH [--perf-name PERF_NAME] [--aggregate AGGREGATE] [--interval INTERVAL] [--perf-file PERF_FILE]
#
# usage: diff mutiple db files
#   python3 rollingplot.py diff MUTI_DB_FILE_PATH [--perf-name PERF_NAME] [--aggregate AGGREGATE] [--interval INTERVAL] [--perf-file PERF_FILE]
#
#  If you only observe one rolling counter, indicate the --perf-name parameter.
#  If you want to observe multiple at the same time, you can indicate the --perf-file parameter,
#  pointing to the path to a description file, each line in the file is a rolling counter,
#  and you can use the '//' comment at the beginning of the line to remove the unconcerned counter.
#
#  Note that generally speaking, when observing multiple rolling counters,
#  the meaning of the x-axis needs to be the same, then you can use the intervalBased mode.
#
#  If you want to compare multiple dbs to observe the difference between multiple runs, you can use diff mode.
#  This requires specifying the path of a description file. Each line in this description file contains a specific db path.
#  
#  eg. 
#    exec emu twice with different parameters and obtained different db files (db0, db1).
#    want to observe the changes in IPC and prefetch accuracy.
#    create a file named db.txt:
#                            path to db0
#                            path to db1
#    create a file named perf.txt:
#                            IPC
#                            L1PrefetchAccuracy
#    run python3 rolling.py diff db.txt --perf-file perf.txt -I (interval in RTL)
#  eg.
#    want to observe the IPC rolling in single db (db0).
#    run python3 rolling.py plot path-to-db0 --perf-name IPC
#


class DataSet:
    
    def __init__(self, db_path):
        self.conn = sqlite3.connect(db_path)
        self.cursor = self.conn.cursor()
        self.xdata = []
        self.ydata = []
    
    def derive(self, perf_name, aggregate, clk_itval, hart):
        sql = "SELECT xAxisPt, yAxisPt FROM {}_rolling_{}".format(perf_name, hart)
        self.cursor.execute(sql)
        result = self.cursor.fetchall()
        aggcnt = 0
        recordcnt = 0
        aggydata = 0
        aggxdata = 0
        self.xdata = []
        self.ydata = []
        if clk_itval == -1:
            # normal mode
            # db log in normal mode: (xAxis, ydata)
            # xAxis is current position in X Axis, ydata is the Increment value between this point and last point
            for row in result:
                aggcnt += 1
                aggydata += row[1]
                if aggcnt == aggregate:
                    self.xdata.append(row[0])
                    self.ydata.append(aggydata/(row[0]-aggxdata))
                    aggcnt = 0
                    aggydata = 0
                    aggxdata = row[0]
        else:
            # intervalBased mode, -I interval should be specified
            # db log in intervalBased mode: (xdata, ydata)
            # xdata, ydata in the Increment value in a certain interval
            for row in result:
                aggcnt += 1
                aggxdata += row[0]
                aggydata += row[1]
                if aggcnt == aggregate:
                    self.xdata.append((clk_itval * aggregate) * (recordcnt + 1))
                    self.ydata.append(0 if aggydata == 0 else aggxdata/aggydata)
                    aggcnt = 0
                    aggxdata = 0
                    aggydata = 0
                    recordcnt += 1
    
    def plot(self, lb='PERF'):
        plt.plot(self.xdata, self.ydata, lw=1, ls='-', label=lb)
    
    def legend():
        plt.legend()
    
    def show():
        plt.show()

def err_exit(msg):
    print(msg)
    sys.exit(1)

def check_args(args):
    if args.aggregate <= 0:
        err_exit("aggregation ratio must be no less than 1")
    if not args.perf_name and not args.perf_file:
        err_exit("should either specify perf-name or perf-file")

def plot_dataset(path, perf_name, aggregate, clk_itval, perf_file, db_id=-1):
    dataset = DataSet(path)
    label = '_' + str(db_id) if db_id != -1 else ''
    
    if perf_file:
        with open(perf_file) as fp:
            perfs = fp.readlines()
            perfs = [perf.strip() for perf in perfs]
            perfs = list(filter(lambda x: not x.startswith('//'), perfs))
            for perf in perfs:
                dataset.derive(perf, aggregate, clk_itval, 0)
                dataset.plot(perf + label)
    else:
        dataset.derive(perf_name, aggregate, clk_itval, 0)
        dataset.plot(perf_name + label)

def handle_plot(args):
    check_args(args)
    
    plot_dataset(args.db_path, args.perf_name, args.aggregate, args.interval, args.perf_file)
        
    DataSet.legend()
    DataSet.show()

def handle_diff(args):
    check_args(args)
    
    db_path = args.db_path
    
    with open(db_path) as fp:
        for (idx, db) in enumerate(fp):
            plot_dataset(db.strip(), args.perf_name, args.aggregate, args.interval, args.perf_file, idx)
    
    DataSet.legend()
    DataSet.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="performance rolling plot script for xs")
    subparsers = parser.add_subparsers(title='useful sub function', dest='subcommand', help='useful sub function')
    
    # sub function for single db file
    cmd1_parser = subparsers.add_parser('plot', help='for single db file')
    cmd1_parser.add_argument('db_path', metavar='db_path', type=str, help='path to chiseldb file')
    cmd1_parser.add_argument('--perf-name', default=None, type=str, help="name of the performance counter")
    cmd1_parser.add_argument('--aggregate', '-A', default=1, type=int, help="aggregation ratio")
    cmd1_parser.add_argument('--interval', '-I', default=-1, type=int, help="interval value in the interval based mode")
    cmd1_parser.add_argument('--perf-file', '-F', default=None, type=str, help="path to a file including all interested performance counters")
    
    # sub function for diff multiple db files
    cmd1_parser = subparsers.add_parser('diff', help='for diff multiple db files')
    cmd1_parser.add_argument('db_path', metavar='muti_db_path', type=str, help="path to a file including all path to chiseldb files")
    cmd1_parser.add_argument('--perf-name', default=None, type=str, help="name of the performance counter")
    cmd1_parser.add_argument('--aggregate', '-A', default=1, type=int, help="aggregation ratio")
    cmd1_parser.add_argument('--interval', '-I', default=-1, type=int, help="interval value in the interval based mode")
    cmd1_parser.add_argument('--perf-file', '-F', default=None, type=str, help="path to a file including all interested performance counters")

    args = parser.parse_args()
    
    if args.subcommand == 'plot':
        handle_plot(args)
    elif args.subcommand == 'diff':
        handle_diff(args)
    else:
        err_exit('invalid command')