stats_dir = ''

CSV_BASE = 'results/results_base.csv'
CSV_REF = 'results/results_ref.csv'
JSON_FILE = 'resources/spec06_rv64gcb_o2_20m.json'
OUT_BASE = 'results/results-weighted_base.csv'
OUT_REF = 'results/results-weighted_ref.csv'

INT_ONLY = False
FP_ONLY = False

TOTAL_ANALYSE = True
BACKEND_ANALYSE = True
FRONTEND_ANALYSE = True
MEM_ANALYSE = True
CUSTOM_ANALYSE = True

# Not benchmark_list canbe add here to specify benchmark to draw, use like:
# benchmark_list = {'libquantum','h264ref','namd', 'gamess'}

xs_custom_rename_map = {
    'IntIQFullStallAlu': 'MergeIQFullStall',
    'IntIQFullStallBrh': 'MergeIQFullStall',
    'IntIQFullStallOther': 'MergeIQFullStall',
    'FpIQFullStall': 'MergeIQFullStall',
    'VecIQFullStall': 'MergeIQFullStall',
    'LoadIQFullStall': 'MergeIQFullStallLoad',
    'StoreIQFullStall': 'MergeIQFullStallStore',
    'OtherIQFullStall': 'MergeIQFullStall',

    'IQEnqPolicyStallIssued': 'MergeIQpolicyStall',
    'IQEnqPolicyStall': 'MergeIQpolicyStall',

    'LoadDispatchPolicyStall' : 'MergeDispatchPolicyBandwidth',
    'StoreDispatchPolicyStall': 'MergeDispatchPolicyBandwidth',
    'OtherDispatchPolicyStall': 'MergeDispatchPolicyBandwidth',

    'BalanceDispatchPolicyStallAlu':  'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallBrh':  'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallInt':  'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallFp':   'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallVec':  'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallLoad': 'MergeDispatchPolicyBalance',
    'BalanceDispatchPolicyStallStore':'MergeDispatchPolicyBalance',
    'OtherBalanceDispatchPolicyStall':'MergeDispatchPolicyBalance',

    'NoStall': 'MergeBase',
    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',
}

xs_frontend_rename_map = {
    'OverrideBubble': 'MergeOverrideBubble',
    'FtqFullStall': 'MergeFtqFullStall',
    'FtqUpdateBubble': 'MergeFtqUpdateBubble',
    'TAGEMissBubble': 'MergeTAGEMissBubble',
    'SCMissBubble': 'MergeSCMissBubble',
    'ITTAGEMissBubble': 'MergeITTAGEMissBubble',
    'RASMissBubble': 'MergeRASMissBubble',
    'ICacheMissBubble': 'MergeICacheMissBubble',
    'ITLBMissBubble': 'MergeITLBMissBubble',
    'BTBMissBubble': 'MergeBTBMissBubble',
    'FetchFragBubble': 'MergeFetchFragBubble',
    'FrontendOtherCoreStall': "MergeCoreOther",

    'FlushedInsts': 'MergeBadSpecInst',

    'ControlRedirectStall': 'MergeBadSpec',

    'OtherRedirectStall': 'MergeBadSpec',
    'OtherRedirectBubble': 'MergeMisc',


    'NoStall': 'MergeBase',
    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',
}

xs_backend_rename_map = {

    'IssueCancelStall': 'MergeCancelStall',
    'DivStall': 'MergeExecStall',
    'IntNotReadyStall': 'MergeExecStall',
    'FPNotReadyStall': 'MergeExecStall',
    'OtherNotReadyStall': 'MergeExecStall',

    'RobStall': 'MergeRobStall',

    'FusionBubble' : 'MergeFusion',

    'LoadDispatchPolicyStall':  'MergeDispatchPolicy',
    'StoreDispatchPolicyStall': 'MergeDispatchPolicy',
    'OtherDispatchPolicyStall': 'MergeDispatchPolicy',

    'BalanceDispatchPolicyStallAlu':  'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallBrh':  'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallInt':  'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallFp':   'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallVec':  'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallLoad': 'MergeDispatchPolicy',
    'BalanceDispatchPolicyStallStore':'MergeDispatchPolicy',
    'OtherBalanceDispatchPolicyStall':'MergeDispatchPolicy',

    'IQEnqPolicyStallIssued': 'MergeIQpolicyStall',
    'IQEnqPolicyStall': 'MergeIQpolicyStall',

    'IntIQFullStallAlu': 'MergeIQFullStall',
    'IntIQFullStallBrh': 'MergeIQFullStall',
    'IntIQFullStallOther': 'MergeIQFullStall',
    'FpIQFullStall': 'MergeIQFullStall',
    'VecIQFullStall': 'MergeIQFullStall',
    'LoadIQFullStall': 'MergeIQFullStall',
    'StoreIQFullStall': 'MergeIQFullStall',
    'OtherIQFullStall': 'MergeIQFullStall',

    'ControlRecoveryStall': 'MergeRabWalkStall',
    'MemVioRecoveryStall': 'MergeRabWalkStall',
    'OtherRecoveryStall': 'MergeRabWalkStall',


    'BackendOtherCoreStall': 'MergeCoreOther',

    'IntFlStall': 'MergeFreelistStall',
    'FpFlStall': 'MergeFreelistStall',
    'VecFlStall': 'MergeFreelistStall',
    'V0FlStall': 'MergeFreelistStall',
    'VlFlStall': 'MergeFreelistStall',
    'MultiFlStall': 'MergeFreelistStall',


    'SpecialInsts': 'MergePrivileged',

    'NoStall': 'MergeBase',
    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',
}

xs_mem_rename_map = {
    'MemNotReadyStall': 'MergeMemNotReadyStall',

    'LoadTLBStall': 'MergeLoadTLBStall',
    'LoadL1Stall': 'MergeLoadL1Stall',
    'LoadL2Stall': 'MergeLoadL2Stall',
    'LoadL3Stall': 'MergeLoadL3Stall',
    'LoadMemStall': 'MergeLoadMemStall',

    'StoreStall': 'MergeStore',
    'SqStall': 'MergeSqStall',
    'LqStall': 'MergeLqStall',


    'AtomicStall': 'MergeAtomicStall',

    'LoadMSHRReplayStall': 'MergeMSHRReplayStall',
    'LoadVioReplayStall': 'MergeLoadVioReplay',

    'MemVioRedirectStall': 'MergeMemVioRedirect',
    'MemVioRedirectBubble': 'MergeMemVioRedirect',

    'NoStall': 'MergeBase',
    'commitInstr': 'Insts',
    'total_cycles': 'Cycles',

}

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
    'FrontendOtherCoreStall': "MergeCoreOther",

    'IssueCancelStall': 'MergeCore',
    'DivStall': 'MergeCore',
    'IntNotReadyStall': 'MergeCore',
    'FPNotReadyStall': 'MergeCore',
    'OtherNotReadyStall': 'MergeCore',

    'MemNotReadyStall': 'MergeLoad',

    'RobStall': 'MergeCore',
    'LqStall': 'MergeLoad',
    'SqStall': 'MergeStore',

    'IntFlStall': 'MergeFreelistStall',
    'FpFlStall': 'MergeFreelistStall',
    'VecFlStall': 'MergeFreelistStall',
    'V0FlStall': 'MergeFreelistStall',
    'VlFlStall': 'MergeFreelistStall',
    'MultiFlStall': 'MergeFreelistStall',

    'FusionBubble' : 'MergeCore',

    'LoadDispatchPolicyStall':  'MergeCore',
    'StoreDispatchPolicyStall': 'MergeCore',
    'OtherDispatchPolicyStall': 'MergeCore',

    'BalanceDispatchPolicyStallAlu':  'MergeCore',
    'BalanceDispatchPolicyStallBrh':  'MergeCore',
    'BalanceDispatchPolicyStallInt':  'MergeCore',
    'BalanceDispatchPolicyStallFp':   'MergeCore',
    'BalanceDispatchPolicyStallVec':  'MergeCore',
    'BalanceDispatchPolicyStallLoad': 'MergeCore',
    'BalanceDispatchPolicyStallStore':'MergeCore',
    'OtherBalanceDispatchPolicyStall':'MergeCore',

    'IQEnqPolicyStallIssued': 'MergeCore',
    'IQEnqPolicyStall': 'MergeCore',

    'IntIQFullStallAlu': 'MergeCore',
    'IntIQFullStallBrh': 'MergeCore',
    'IntIQFullStallOther': 'MergeCore',
    'FpIQFullStall': 'MergeCore',
    'VecIQFullStall': 'MergeCore',
    'LoadIQFullStall': 'MergeCore',
    'StoreIQFullStall': 'MergeCore',
    'OtherIQFullStall': 'MergeCore',

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

    'ControlRedirectStall': 'MergeBadSpec',
    'MemVioRedirectStall': 'MergeBadSpec',
    'OtherRedirectStall': 'MergeBadSpec',

    'ControlRecoveryStall': 'MergeCore',
    'MemVioRecoveryStall': 'MergeCore',
    'OtherRecoveryStall': 'MergeCore',

    'SpecialInsts': 'MergePrivileged',

    'BackendOtherCoreStall': 'MergeCoreOther',
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

XS_CORE_PREFIX = r'\[PERF\s*\]\[time=\s*\d+\].*?\.core'

targets = {
    'NoStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: NoStall,\s+(\d+)',
    'OverrideBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OverrideBubble,\s+(\d+)',
    'FtqUpdateBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FtqUpdateBubble,\s+(\d+)',
    'TAGEMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: TAGEMissBubble,\s+(\d+)',
    'SCMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: SCMissBubble,\s+(\d+)',
    'ITTAGEMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: ITTAGEMissBubble,\s+(\d+)',
    'RASMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: RASMissBubble,\s+(\d+)',
    'MemVioRedirectBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: MemVioRedirectBubble,\s+(\d+)',
    'OtherRedirectBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherRedirectBubble,\s+(\d+)',
    'FtqFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FtqFullStall,\s+(\d+)',

    'ICacheMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: ICacheMissBubble,\s+(\d+)',
    'ITLBMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: ITLBMissBubble,\s+(\d+)',
    'BTBMissBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BTBMissBubble,\s+(\d+)',
    'FetchFragBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FetchFragBubble,\s+(\d+)',
    'FrontendOtherCoreStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FrontendOtherCoreStall,\s+(\d+)',

    'IssueCancelStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IssueCancelStall,\s+(\d+)',
    'DivStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: DivStall,\s+(\d+)',
    'IntNotReadyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IntNotReadyStall,\s+(\d+)',
    'FPNotReadyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FPNotReadyStall,\s+(\d+)',
    'MemNotReadyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: MemNotReadyStall,\s+(\d+)',
    'OtherNotReadyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherNotReadyStall,\s+(\d+)',
    'RobStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: RobStall,\s+(\d+)',
    'LqStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LqStall,\s+(\d+)',
    'SqStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: SqStall,\s+(\d+)',

    'IntFlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IntFlStall,\s+(\d+)',
    'FpFlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FpFlStall,\s+(\d+)',
    'VecFlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: VecFlStall,\s+(\d+)',
    'V0FlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: V0FlStall,\s+(\d+)',
    'VlFlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: VlFlStall,\s+(\d+)',
    'MultiFlStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: MultiFlStall,\s+(\d+)',

    'FusionBubble': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FusionBubble,\s+(\d+)',

    'LoadDispatchPolicyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadDispatchPolicyStall,\s+(\d+)',
    'StoreDispatchPolicyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: StoreDispatchPolicyStall,\s+(\d+)',
    'OtherDispatchPolicyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherDispatchPolicyStall,\s+(\d+)',

    'BalanceDispatchPolicyStallAlu': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallAlu,\s+(\d+)',
    'BalanceDispatchPolicyStallBrh': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallBrh,\s+(\d+)',
    'BalanceDispatchPolicyStallInt': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallInt,\s+(\d+)',
    'BalanceDispatchPolicyStallFp':  fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallFp,\s+(\d+)',
    'BalanceDispatchPolicyStallVec': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallVec,\s+(\d+)',
    'BalanceDispatchPolicyStallLoad': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallLoad,\s+(\d+)',
    'BalanceDispatchPolicyStallStore': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BalanceDispatchPolicyStallStore,\s+(\d+)',
    'OtherBalanceDispatchPolicyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherBalanceDispatchPolicyStall,\s+(\d+)',

    'IQEnqPolicyStallIssued': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IQEnqPolicyStallIssued,\s+(\d+)',
    'IQEnqPolicyStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IQEnqPolicyStall,\s+(\d+)',

    'IntIQFullStallAlu': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IntIQFullStallAlu,\s+(\d+)',
    'IntIQFullStallBrh': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IntIQFullStallBrh,\s+(\d+)',
    'IntIQFullStallOther': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: IntIQFullStallOther,\s+(\d+)',
    'FpIQFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FpIQFullStall,\s+(\d+)',
    'VecIQFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: VecIQFullStall,\s+(\d+)',
    'LoadIQFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadIQFullStall,\s+(\d+)',
    'StoreIQFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: StoreIQFullStall,\s+(\d+)',
    'OtherIQFullStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherIQFullStall,\s+(\d+)',

    'LoadTLBStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadTLBStall,\s+(\d+)',
    'LoadL1Stall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadL1Stall,\s+(\d+)',
    'LoadL2Stall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadL2Stall,\s+(\d+)',
    'LoadL3Stall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadL3Stall,\s+(\d+)',
    'LoadMemStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadMemStall,\s+(\d+)',
    'StoreStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: StoreStall,\s+(\d+)',
    'AtomicStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: AtomicStall,\s+(\d+)',

    'LoadVioReplayStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadVioReplayStall,\s+(\d+)',
    'LoadMSHRReplayStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: LoadMSHRReplayStall,\s+(\d+)',


    'ControlRedirectStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: ControlRedirectStall,\s+(\d+)',
    'MemVioRedirectStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: MemVioRedirectStall,\s+(\d+)',
    'OtherRedirectStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherRedirectStall,\s+(\d+)',

    'ControlRecoveryStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: ControlRecoveryStall,\s+(\d+)',
    'MemVioRecoveryStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: MemVioRecoveryStall,\s+(\d+)',
    'OtherRecoveryStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: OtherRecoveryStall,\s+(\d+)',


    'FlushedInsts': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: FlushedInsts,\s+(\d+)',
    'SpecialInsts': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: SpecialInsts,\s+(\d+)',
    'BackendOtherCoreStall': fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock\.dispatch: BackendOtherCoreStall,\s+(\d+)',

    "commitInstr": fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock.rob: commitInstr,\s+(\d+)',
    "total_cycles": fr'{XS_CORE_PREFIX}.backend.*?ctrlBlock.rob: clock_cycle,\s+(\d+)'
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
