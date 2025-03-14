#coding=utf8

import ctypes as c
import sys
import signal
from XSPdb import *
from XSIT import DUTSimTop, difftest as df

def handle_sigint(signum, frame):
    print("\nReceived SIGINT, exit.")
    sys.exit(0)
signal.signal(signal.SIGINT, handle_sigint)


def test_sim_top():
    # init
    df.InitRam("../ready-to-run/microbench.bin", 1024*1024*1024)
    df.InitFlash("")
    df.difftest_init()
    # create dut
    dut = DUTSimTop()
    df.InitPikerUart(dut.difftest_uart_out_valid.CSelf(), dut.difftest_uart_out_ch.CSelf());
    # reset dut
    dut.reset.AsImmWrite()
    dut.reset.value = 1
    dut.reset.AsRiseWrite()
    dut.InitClock("clock")
    dut.reset.value = 1
    dut.Step(1000)
    dut.reset.value = 0
    print("reset complete")
    # set output callback
    dut.StepRis(df.GetPickerUartFucPtr(), df.GetPickerUartArgPtr())
    print("Load internal signals, please wait ...")
    try:
        XSPdb(dut, df.GetDifftest(0).dut).set_trace()
        while True:
            dut.Step(10000000)
    except Exception as e:
        print("Exit.")


if __name__ == "__main__":
    test_sim_top()


