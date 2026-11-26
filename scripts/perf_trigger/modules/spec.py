from enum import Enum
import logging


class Spec:
    class Version(Enum):
        SPEC06 = "06"
        SPECRATE17 = "17"
        SPECRATE26 = "26"

    class SubTest(Enum):
        INT = "int"
        FP = "fp"
        ALL = ""

    def __init__(
        self,
        version: Version,
    ):
        self.version = version

    def __get_name(self, sub: "Spec.SubTest") -> str:
        match self.version:
            case Spec.Version.SPEC06:
                return f"SPEC{sub.value}2006"
            case Spec.Version.SPECRATE17:
                return f"SPEC{sub.value}rate2017"
            case Spec.Version.SPECRATE26:
                return f"SPEC{sub.value}rate2026"

    def get_name(self) -> str:
        return self.__get_name(Spec.SubTest.ALL)

    def get_int_name(self) -> str:
        return self.__get_name(Spec.SubTest.INT)

    def get_fp_name(self) -> str:
        return self.__get_name(Spec.SubTest.FP)

    def __get_benchmarks(
        self, sub: "Spec.SubTest", with_id: bool | None = None
    ) -> list[str]:
        ref = SPEC_REFTIME[self.version]
        if sub == Spec.SubTest.ALL:
            l = list(ref[Spec.SubTest.INT].keys()) + list(ref[Spec.SubTest.FP].keys())
        else:
            l = list(ref[sub].keys())

        if with_id is None:
            with_id = self.version != Spec.Version.SPEC06
        return [bench.split(".")[1] for bench in l] if not with_id else l

    def get_benchmarks(self, with_id: bool | None = None) -> list[str]:
        return self.__get_benchmarks(Spec.SubTest.ALL, with_id)

    def get_int_benchmarks(self, with_id: bool | None = None) -> list[str]:
        return self.__get_benchmarks(Spec.SubTest.INT, with_id)

    def get_fp_benchmarks(self, with_id: bool | None = None) -> list[str]:
        return self.__get_benchmarks(Spec.SubTest.FP, with_id)

    def get_benchmark_fullname(self, bench: str) -> str:
        for b in self.get_benchmarks(with_id=True):
            if bench in b:
                return b
        logging.warning("Benchmark %s not found in SPEC %s", bench, self.version.value)
        return bench

    def get_ref_time(self, bench: str) -> int | None:
        ref = SPEC_REFTIME[self.version]
        for sub in [Spec.SubTest.INT, Spec.SubTest.FP]:
            for b, t in ref[sub].items():
                if bench in b:
                    return t
        logging.warning(
            "Benchmark %s not found in SPEC %s, cannot get reference time",
            bench,
            self.version.value,
        )
        return None


# Reference time extracted from:
#   cpu2006v99/benchspec/CPU2006/<testcase>/data/ref/reftime
#   cpu2017/benchspec/CPU/<testcase>/data/refrate/reftime
#   cpu2026/benchspec/CPU/<testcase>/data/refrate/reftime
# Also available on SPEC official result site
#   i.e. https://www.spec.org/cpu2017/results/res2017q2/cpu2017-20161026-00001.csv
SPEC_REFTIME = {
    Spec.Version.SPEC06: {
        Spec.SubTest.INT: {
            "400.perlbench": 9770,
            "401.bzip2": 9650,
            "403.gcc": 8050,
            "429.mcf": 9120,
            "445.gobmk": 10490,
            "456.hmmer": 9330,
            "458.sjeng": 12100,
            "462.libquantum": 20720,
            "464.h264ref": 22130,
            "471.omnetpp": 6250,
            "473.astar": 7020,
            "483.xalancbmk": 6900,
        },
        Spec.SubTest.FP: {
            "410.bwaves": 13590,
            "416.gamess": 19580,
            "433.milc": 9180,
            "434.zeusmp": 9100,
            "435.gromacs": 7140,
            "436.cactusADM": 11950,
            "437.leslie3d": 9400,
            "444.namd": 8020,
            "447.dealII": 11440,
            "450.soplex": 8340,
            "453.povray": 5320,
            "454.calculix": 8250,
            "459.GemsFDTD": 10610,
            "465.tonto": 9840,
            "470.lbm": 13740,
            "481.wrf": 11170,
            "482.sphinx3": 19490,
        },
    },
    Spec.Version.SPECRATE17: {
        Spec.SubTest.INT: {
            "500.perlbench_r": 1592,
            "502.gcc_r": 1416,
            "505.mcf_r": 1616,
            "520.omnetpp_r": 1312,
            "523.xalancbmk_r": 1056,
            "525.x264_r": 1751,
            "531.deepsjeng_r": 1146,
            "541.leela_r": 1656,
            "548.exchange2_r": 2620,
            "557.xz_r": 1080,
        },
        Spec.SubTest.FP: {
            "503.bwaves_r": 10028,
            "507.cactuBSSN_r": 1266,
            "508.namd_r": 950,
            "510.parest_r": 2616,
            "511.povray_r": 2335,
            "519.lbm_r": 1054,
            "521.wrf_r": 2240,
            "526.blender_r": 1523,
            "527.cam4_r": 1749,
            "538.imagick_r": 2487,
            "544.nab_r": 1683,
            "549.fotonik3d_r": 3897,
            "554.roms_r": 1589,
        },
    },
    Spec.Version.SPECRATE26: {
        Spec.SubTest.INT: {
            "706.stockfish_r": 1260,
            "707.ntest_r": 592,
            "708.sqlite_r": 528,
            "710.omnetpp_r": 486,
            "714.cpython_r": 479,
            "721.gcc_r": 686,
            "723.llvm_r": 507,
            "727.cppcheck_r": 359,
            "729.abc_r": 459,
            "734.vpr_r": 461,
            "735.gem5_r": 487,
            "750.sealcrypto_r": 536,
            "753.ns3_r": 613,
            "777.zstd_r": 644,
        },
        Spec.SubTest.FP: {
            "709.cactus_r": 858,
            "722.palm_r": 1320,
            "731.astcenc_r": 840,
            "736.ocio_r": 875,
            "737.gmsh_r": 459,
            "748.flightdm_r": 716,
            "749.fotonik3d_r": 1156,
            "765.roms_r": 1575,
            "766.femflow_r": 1467,
            "767.nest_r": 793,
            "772.marian_r": 1579,
            "782.lbm_r": 573,
        },
    },
}


# global method for easier use
def get_int_benchmarks(version: Spec.Version, with_id: bool | None = None) -> list[str]:
    return Spec(version).get_int_benchmarks(with_id)


def get_fp_benchmarks(version: Spec.Version, with_id: bool | None = None) -> list[str]:
    return Spec(version).get_fp_benchmarks(with_id)


def get_benchmarks(version: Spec.Version, with_id: bool | None = None) -> list[str]:
    return Spec(version).get_benchmarks(with_id)
