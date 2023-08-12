import sys
import argparse
import sqlite3
import matplotlib.pyplot as plt
import numpy as np


# usage: python3 rollingplot.py DB_FILE_PATH PERF_NAME [--aggregate AGGREGATE_RATIO]


class DataSet:
    
    def __init__(self, db_path):
        self.conn = sqlite3.connect(db_path)
        self.cursor = self.conn.cursor()
        self.xdata = []
        self.ydata = []
    
    def derive(self, perf_name, aggregate, hart):
        sql = "SELECT xAxisPt, yAxisPt FROM {}_rolling_{}".format(perf_name, hart)
        self.cursor.execute(sql)
        result = self.cursor.fetchall()
        aggcnt = 0
        aggydata = 0
        aggxdata = 0
        for row in result:
            aggcnt += 1
            aggydata += row[1]
            if aggcnt == aggregate:
                self.xdata.append(row[0])
                self.ydata.append(aggydata/(row[0]-aggxdata))
                aggcnt = 0
                aggydata = 0
                aggxdata = row[0]
    
    def plot(self):
        plt.plot(self.xdata, self.ydata, lw=1, ls='-', c='black')
        plt.show()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="performance rolling plot script for xs")
    parser.add_argument('db_path', metavar='db_path', type=str, help='path to chiseldb file')
    parser.add_argument('perf_name', metavar='perf_name', type=str, help="name of the performance counter")
    parser.add_argument('--aggregate', '-A', default=1, type=int, help="aggregation ratio")
    args = parser.parse_args()

    if args.aggregate <= 0:
        print("aggregation ratio must be no less than 1")
        sys.exit(1)
    
    db_path = args.db_path
    perf_name = args.perf_name
    aggregate = args.aggregate

    dataset = DataSet(db_path)
    dataset.derive(perf_name, aggregate, 0)
    dataset.plot()