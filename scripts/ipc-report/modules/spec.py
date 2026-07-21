from typing import Literal

SPEC_WORKLOADS = {
    "06": {
        "int": [
            "400.perlbench",
            "401.bzip2",
            "403.gcc",
            "429.mcf",
            "445.gobmk",
            "456.hmmer",
            "458.sjeng",
            "462.libquantum",
            "464.h264ref",
            "471.omnetpp",
            "473.astar",
            "483.xalancbmk",
        ],
        "fp": [
            "410.bwaves",
            "416.gamess",
            "433.milc",
            "434.zeusmp",
            "435.gromacs",
            "436.cactusADM",
            "437.leslie3d",
            "444.namd",
            "447.dealII",
            "450.soplex",
            "453.povray",
            "454.calculix",
            "459.GemsFDTD",
            "465.tonto",
            "470.lbm",
            "481.wrf",
            "482.sphinx3",
        ]
    },
    "rate17": {
        "int": [
            "500.perlbench_r",
            "502.gcc_r",
            "505.mcf_r",
            "520.omnetpp_r",
            "523.xalancbmk_r",
            "525.x264_r",
            "531.deepsjeng_r",
            "541.leela_r",
            "548.exchange2_r",
            "557.xz_r",
        ],
        "fp": [
            "503.bwaves_r",
            "507.cactuBSSN_r",
            "508.namd_r",
            "510.parest_r",
            "511.povray_r",
            "519.lbm_r",
            "521.wrf_r",
            "526.blender_r",
            "527.cam4_r",
            "538.imagick_r",
            "544.nab_r",
            "549.fotonik3d_r",
            "554.roms_r",
        ]
    },
    "rate26": {
        "int": [
            "706.stockfish_r",
            "707.ntest_r",
            "708.sqlite_r",
            "710.omnetpp_r",
            "714.cpython_r",
            "721.gcc_r",
            "723.llvm_r",
            "727.cppcheck_r",
            "729.abc_r",
            "734.vpr_r",
            "735.gem5_r",
            "750.sealcrypto_r",
            "753.ns3_r",
            "777.zstd_r",
        ],
        "fp": [
            "709.cactus_r",
            "722.palm_r",
            "731.astcenc_r",
            "736.ocio_r",
            "737.gmsh_r",
            "748.flightdm_r",
            "749.fotonik3d_r",
            "765.roms_r",
            "766.femflow_r",
            "767.nest_r",
            "772.marian_r",
            "782.lbm_r",
        ]
    },
}

def is_spec(testcase: str, type: Literal["int"] | Literal["fp"] | None) -> bool:
    """Returns True if the testcase is a SPEC integer benchmark."""
    for version in SPEC_WORKLOADS.values():
        workloads = version[type] if type else version["int"] + version["fp"]
        for workload in workloads:
            if testcase == workload or testcase == workload.split(".")[1]:
                return True
    return False
