#coding=utf-8

from pyxscore import DUTSimTop, xsp
from pydifftest import difftest as df
from xspdb import *

from bdb import BdbQuit

class XSPdbTop():
    def __init__(self):
        self.dut = DUTSimTop()

    def init_pdb(self):
        #dut = DUTSimTop()
        XSPdb(self.dut, df, xsp).set_trace()
        while True:
            dut.Step(1000)

    def run(self):
        try:
            self.init_pdb()
        except BdbQuit:
            pass
