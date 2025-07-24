#coding=utf-8

from XSPython import DUTSimTop, difftest as df, xsp
from XSPdb import *

def run_sim_top():
    dut = DUTSimTop()
    XSPdb(dut, df, xsp).set_trace()
    while True:
        dut.Step(1000)

if __name__ == "__main__":
    from bdb import BdbQuit
    try:
        run_sim_top()
    except BdbQuit:
        pass
