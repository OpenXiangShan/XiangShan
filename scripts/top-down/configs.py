stats_dir = ''

CSV_PATH = 'results/results.csv'
JSON_FILE = 'resources/spec06_rv64gcb_o2_20m.json'
OUT_CSV = 'results/results-weighted.csv'
INT_ONLY = False
FP_ONLY = False

xs_coarse_rename_map = {
    'OverrideBubble': 'MergeFrontend',
    'FtqFullStall': 'MergeFrontend',
    'FtqUpdateBubble': 'MergeBadSpec',
    'TAGEMissBubble': 'MergeBadSpec',
    'SCMissBubble': 'MergeBadSpec',
    'ITTAGEMissBubble': 'MergeBadSpec',
    'RASMissBubble': 'MergeBadSpec',
    'ICacheMissBubble': 'MergeFrontend',
    'ITLBMissBubble': 'MergeFrontend',
    'BTBMissBubble': 'MergeBadSpec',
    'FetchFragBubble': 'MergeFrontend',

    'DivStall': 'MergeCore',
    'IntNotReadyStall': 'MergeCore',
    'FPNotReadyStall': 'MergeCore',

    'MemNotReadyStall': 'MergeLoad',

    'IntFlStall': 'MergeFreelistStall',
    'FpFlStall': 'MergeFreelistStall',
    'VecFlStall': 'MergeFreelistStall',
    'V0FlStall': 'MergeFreelistStall',
    'VlFlStall': 'MergeFreelistStall',
    'MultiFlStall': 'MergeFreelistStall',

    'LoadTLBStall': 'MergeLoad',
    'LoadL1Stall': 'MergeLoad',
    'LoadL2Stall': 'MergeLoad',
    'LoadL3Stall': 'MergeLoad',
    'LoadMemStall': 'MergeLoad',
    'StoreStall': 'MergeStore',

    'AtomicStall': 'MergeMisc',

    'FlushedInsts': 'MergeBadSpecInst',
    'LoadVioReplayStall': 'MergeBadSpec',

    'LoadMSHRReplayStall': 'MergeLoad',

    'ControlRecoveryStall': 'MergeBadSpec',
    'MemVioRecoveryStall': 'MergeBadSpec',
    'OtherRecoveryStall': 'MergeBadSpec',

    'OtherCoreStall': 'MergeCoreOther',
    'NoStall': 'MergeBase',

    'MemVioRedirectBubble': 'MergeBadSpec',
    'OtherRedirectBubble': 'MergeMisc',

    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',
}

xs_fine_grain_rename_map = {
    'OverrideBubble': 'MergeOtherFrontend',
    'FtqFullStall': 'MergeOtherFrontend',
    'FtqUpdateBubble': 'MergeBadSpecBubble',
    'TAGEMissBubble': 'MergeBadSpecBubble',
    'SCMissBubble': 'MergeBadSpecBubble',
    'ITTAGEMissBubble': 'MergeBadSpecBubble',
    'RASMissBubble': 'MergeBadSpecBubble',
    'ICacheMissBubble': 'ICacheBubble',
    'ITLBMissBubble': 'ITlbBubble',
    'BTBMissBubble': 'MergeBadSpecBubble',
    'FetchFragBubble': 'FragmentBubble',

    'DivStall': 'LongExecute',
    'IntNotReadyStall': 'MergeInstNotReady',
    'FPNotReadyStall': 'MergeInstNotReady',

    'MemNotReadyStall': 'MemNotReady',

    'IntFlStall': 'MergeFreelistStall',
    'FpFlStall': 'MergeFreelistStall',

    'LoadTLBStall': 'DTlbStall',
    'LoadL1Stall': 'LoadL1Bound',
    'LoadL2Stall': 'LoadL2Bound',
    'LoadL3Stall': 'LoadL3Bound',
    'LoadMemStall': 'LoadMemBound',
    'StoreStall': 'MergeStoreBound',

    'AtomicStall': 'SerializeStall',

    'FlushedInsts': 'BadSpecInst',
    'LoadVioReplayStall': None,

    'LoadMSHRReplayStall': None,

    'ControlRecoveryStall': 'MergeBadSpecWalking',
    'MemVioRecoveryStall': 'MergeBadSpecWalking',
    'OtherRecoveryStall': 'MergeBadSpecWalking',

    'OtherCoreStall': 'MergeMisc',
    'NoStall': None,

    'MemVioRedirectBubble': 'MergeBadSpecBubble',
    'OtherRedirectBubble': 'MergeMisc',

    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',
}

XS_CORE_PREFIX = r'\[PERF \]\[time=\s+\d+\] SimTop\.l_soc\.core_with_l2\.core'

targets = {
    'NoStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: NoStall,\s+(\d+)',

    'OverrideBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: OverrideBubble,\s+(\d+)',
    'FtqUpdateBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FtqUpdateBubble,\s+(\d+)',
    'TAGEMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: TAGEMissBubble,\s+(\d+)',
    'SCMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: SCMissBubble,\s+(\d+)',
    'ITTAGEMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: ITTAGEMissBubble,\s+(\d+)',
    'RASMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: RASMissBubble,\s+(\d+)',
    'MemVioRedirectBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: MemVioRedirectBubble,\s+(\d+)',
    'OtherRedirectBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: OtherRedirectBubble,\s+(\d+)',
    'FtqFullStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FtqFullStall,\s+(\d+)',

    'ICacheMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: ICacheMissBubble,\s+(\d+)',
    'ITLBMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: ITLBMissBubble,\s+(\d+)',
    'BTBMissBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: BTBMissBubble,\s+(\d+)',
    'FetchFragBubble': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FetchFragBubble,\s+(\d+)',

    'DivStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: DivStall,\s+(\d+)',
    'IntNotReadyStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: IntNotReadyStall,\s+(\d+)',
    'FPNotReadyStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FPNotReadyStall,\s+(\d+)',
    'MemNotReadyStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: MemNotReadyStall,\s+(\d+)',

    'IntFlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: IntFlStall,\s+(\d+)',
    'FpFlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FpFlStall,\s+(\d+)',
    'VecFlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: VecFlStall,\s+(\d+)',
    'V0FlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: V0FlStall,\s+(\d+)',
    'VlFlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: VlFlStall,\s+(\d+)',
    'MultiFlStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: MultiFlStall,\s+(\d+)',

    'LoadTLBStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadTLBStall,\s+(\d+)',
    'LoadL1Stall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadL1Stall,\s+(\d+)',
    'LoadL2Stall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadL2Stall,\s+(\d+)',
    'LoadL3Stall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadL3Stall,\s+(\d+)',
    'LoadMemStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadMemStall,\s+(\d+)',
    'StoreStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: StoreStall,\s+(\d+)',
    'AtomicStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: AtomicStall,\s+(\d+)',

    'LoadVioReplayStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadVioReplayStall,\s+(\d+)',
    'LoadMSHRReplayStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: LoadMSHRReplayStall,\s+(\d+)',

    'ControlRecoveryStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: ControlRecoveryStall,\s+(\d+)',
    'MemVioRecoveryStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: MemVioRecoveryStall,\s+(\d+)',
    'OtherRecoveryStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: OtherRecoveryStall,\s+(\d+)',

    'FlushedInsts': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: FlushedInsts,\s+(\d+)',
    'OtherCoreStall': fr'{XS_CORE_PREFIX}.backend.inner\.ctrlBlock\.dispatch: OtherCoreStall,\s+(\d+)',

    "commitInstr": r"\[PERF \]\[time=\s+\d+\] SimTop.l_soc.core_with_l2.core.backend.inner\.ctrlBlock.rob: commitInstr,\s+(\d+)",
    "total_cycles": r"\[PERF \]\[time=\s+\d+\] SimTop.l_soc.core_with_l2.core.backend.inner\.ctrlBlock.rob: clock_cycle,\s+(\d+)",
}


spec_bmks = {
    '06': {
        'int': [
            'perlbench',
            'bzip2',
            'gcc',
            'mcf',
            'gobmk',
            'hmmer',
            'sjeng',
            'libquantum',
            'h264ref',
            'omnetpp',
            'astar',
            'xalancbmk',
        ],
        'float': [
            'bwaves', 'gamess', 'milc', 'zeusmp', 'gromacs',
            'cactusADM', 'leslie3d', 'namd', 'dealII', 'soplex',
            'povray', 'calculix', 'GemsFDTD', 'tonto', 'lbm',
            'wrf', 'sphinx3',
        ],
        'high_squash': ['astar', 'bzip2', 'gobmk', 'sjeng'],
    },
    '17': {},
}
