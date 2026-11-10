import os.path as osp
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import configs as cf
import argparse
import os.path as osp


ROBHEAD_COLUMN_ORDER = ['mul', 'ldu', 'stu', 'fma', 'fdivsqrt', 'other']


def run_intel_topdown_analyse(df, issue_width):
    analysed_df = df.copy(deep=True)
    original_columns = set(analysed_df.columns)
    icount = 20 * 10 ** 6
    analysed_df['Base'] = icount
    analysed_df['FetchLatencyBound'] = analysed_df['if_fetch_bubble_eq_max'] * issue_width
    analysed_df['FetchBandwidthBound'] = analysed_df['if_fetch_bubble'] - analysed_df['FetchLatencyBound']
    print(f'FetchLatencyBound: {analysed_df["FetchLatencyBound"]}')
    analysed_df['BadSpec'] = analysed_df['inst_spec'] - icount + analysed_df['recovery_bubble']
    print(f'BadSpec Insts: {analysed_df["BadSpec"]}')
    # analysed_df['CoreBound'] = (analysed_df['exec_stall_cycle'] - analysed_df['mem_stall_anyload'] - analysed_df['mem_stall_store']) * issue_width

    analysed_df['L1Bound'] = (analysed_df['mem_stall_anyload'] - analysed_df['mem_stall_l1miss']) * issue_width
    print(f'L1Bound: {analysed_df["L1Bound"]}')
    analysed_df['L2Bound'] = (analysed_df['mem_stall_l1miss'] - analysed_df['mem_stall_l2miss']) * issue_width
    print(f'L2Bound: {analysed_df["L2Bound"]}')
    analysed_df['L3Bound'] = (analysed_df['mem_stall_l2miss'] - analysed_df['mem_stall_l3miss']) * issue_width
    print(f'L3Bound: {analysed_df["L3Bound"]}')
    analysed_df['MemBound'] = analysed_df['mem_stall_l3miss'] * issue_width
    analysed_df['StoreBound'] = analysed_df['mem_stall_store'] * issue_width
    analysed_df['CoreBound'] = analysed_df['total_cycles'] * issue_width - icount - analysed_df['FetchLatencyBound'] - analysed_df['FetchBandwidthBound'] - analysed_df['BadSpec'] - analysed_df['mem_stall_anyload']*issue_width - analysed_df['StoreBound']
    print(f'CoreBound: {analysed_df["CoreBound"]}')
    total_cycles = analysed_df['total_cycles'] * issue_width
    print(f'Total Cycles: {total_cycles}')
    partition_sum = analysed_df['Base'] + analysed_df['FetchLatencyBound'] + analysed_df['FetchBandwidthBound'] + analysed_df['BadSpec'] + analysed_df['CoreBound'] + analysed_df['L1Bound'] + analysed_df['L2Bound'] + analysed_df['L3Bound'] + analysed_df['MemBound'] + analysed_df['StoreBound']
    print(f'Partition Sum: {partition_sum}')
    generated_columns = [col for col in analysed_df.columns if col not in original_columns]
    return analysed_df[['cpi'] + generated_columns]


def run_robhead_analyse(df):
    analysed_df = df.copy(deep=True)
    kept_wait_cycle_map = {
        'mul': 'waitMulCycle',
        'ldu': 'waitLduCycle',
        'stu': 'waitStuCycle',
        'fma': 'waitfmacCycle',
        'fdivsqrt': 'waitfDivSqrtCycle',
    }
    other_wait_cycle_columns = [
        'waitAluCycle',
        'waitDivCycle',
        'waitBrhCycle',
        'waitJmpCycle',
        'waitCsrCycle',
        'waitFenCycle',
        'waitBkuCycle',
        'waitAtmCycle',
        'waitfaluCycle',
        'waitfcvtCycle',
        'waitfcmpCycle',
    ]

    all_wait_cycle_columns = list(kept_wait_cycle_map.values()) + other_wait_cycle_columns
    for column in all_wait_cycle_columns:
        if column not in analysed_df.columns:
            analysed_df[column] = 0.0
    total_wait_cycles = analysed_df[all_wait_cycle_columns].sum(axis=1)

    for output_column, input_column in kept_wait_cycle_map.items():
        analysed_df[output_column] = analysed_df[input_column]
    analysed_df['other'] = analysed_df[other_wait_cycle_columns].sum(axis=1)

    normalizer = total_wait_cycles.replace(0, np.nan)
    analysed_df[ROBHEAD_COLUMN_ORDER] = analysed_df[ROBHEAD_COLUMN_ORDER].div(normalizer, axis=0).fillna(0.0)

    partition_sum = analysed_df[ROBHEAD_COLUMN_ORDER].sum(axis=1)
    print(f'ROB Head Wait Partition Sum: {partition_sum}')
    return analysed_df[['cpi'] + ROBHEAD_COLUMN_ORDER]


def draw(out_csv_paths=None, labels=None, issue_width=8):

    assert len(out_csv_paths) == len(labels)
    assert 1 <= len(out_csv_paths) <= 2
    issue_width = float(issue_width)

    results = {labels[i]: (out_csv_paths[i], labels[i]) for i in range(len(out_csv_paths))}

    configs = list(results.keys())

    color_types = 10
    cmap = plt.get_cmap('tab10')
    color_index = np.arange(0, 1, 1.0 / color_types)
    colors = [cmap(c) for c in color_index] * 3
    hatches = [None] * color_types + ['//'] * color_types + ['|'] * color_types

    n_conf = len(configs)

    x = None
    have_set_label = False

    dfs = [pd.read_csv(result[0], index_col=0)
           for _, result in results.items()]
    common_bmk = list(set.intersection(*[set(df.index) for df in dfs]))
    dfs = [df.loc[common_bmk] for df in dfs]

    # Draw stacked bar chart for each simulator
    # set each width

    total_width = len(dfs[0])
    if hasattr(cf, "benchmark_list") and cf.benchmark_list is not None and len(cf.benchmark_list)>0:
        total_width = len(cf.benchmark_list)
    width = 0.8 / n_conf

    min_width = float(total_width) / 10.0
    if (width > min_width):
        width = min_width

    rename = True



    total_analyse = cf.TOTAL_ANALYSE
    backend_analyse = cf.BACKEND_ANALYSE
    frontend_analyse = cf.FRONTEND_ANALYSE
    mem_analyse = cf.MEM_ANALYSE
    custom_analyse = cf.CUSTOM_ANALYSE
    intel_topdown_analyse = getattr(cf, 'INTEL_TOPDOWN_ANALYSE', False)
    robhead_analyse = getattr(cf, 'ROBHEAD_ANALYSE', False)
    fine_grain_rename = False

    analyse_jobs = []

    if fine_grain_rename:
        analyse_jobs.append(("fine", cf.xs_fine_grain_rename_map))
    else:
        if total_analyse:
            analyse_jobs.append(("total", cf.xs_coarse_rename_map))
        if backend_analyse:
            analyse_jobs.append(("backend", cf.xs_backend_rename_map))
        if frontend_analyse:
            analyse_jobs.append(("frontend", cf.xs_frontend_rename_map))
        if mem_analyse:
            analyse_jobs.append(("mem", cf.xs_mem_rename_map))
        if custom_analyse:
            analyse_jobs.append(("custom", cf.xs_custom_rename_map))
        if intel_topdown_analyse:
            analyse_jobs.append(("intel_topdown", None))
        if robhead_analyse:
            analyse_jobs.append(("robhead", None))
    if not analyse_jobs:
        analyse_jobs.append(("default", cf.xs_coarse_rename_map))

    for tag, current_rename_map in analyse_jobs:
        # set figure size:
        fig, ax = plt.subplots()
        fig.set_size_inches(8.0, 5.0)

        renamed_dfs = []
        x = None
        have_set_label = False

        dfs_this = [d.copy(deep=True) for d in dfs]

        for df in dfs_this:
            to_drops = []
            sorted_cols = []
            origin_columns = df.columns

            def rename_with_map(df, rename_map):
                existing_columns = set(df.columns)
                for k in rename_map:
                    if rename_map[k] is not None:
                        if rename_map[k].startswith('Merge'):
                            merged = rename_map[k][5:]
                            if merged not in df.columns:
                                df[merged] = df[k]
                                sorted_cols.append(merged)
                            else:
                                if merged in origin_columns:
                                    df[merged] = df[k]
                                    sorted_cols.append(merged)
                                else:
                                    df[merged] += df[k]
                        else:
                            df[rename_map[k]] = df[k]
                            sorted_cols.append(rename_map[k])

                        to_drops.append(k)
                    else:
                        sorted_cols.append(k)
                cols_to_keep = set(sorted_cols) | {'cpi'}
                cols_to_drop = existing_columns - cols_to_keep
                df.drop(columns=cols_to_drop, inplace=True)

            if tag == 'intel_topdown':
                df = run_intel_topdown_analyse(df, issue_width)
            elif tag == 'robhead':
                df = run_robhead_analyse(df)
            elif rename and current_rename_map is not None:
                rename_with_map(df, current_rename_map)
                icount = 20 * 10 ** 6
                if 'BadSpecInst' in df.columns:
                    df['BadSpecInst'] += df['Base'] - icount
                if 'Base' in df.columns:
                    df['Base'] = icount

            df = df.astype(float)
            renamed_dfs.append(df)

        common_col = list(set.intersection(
            *[set(df.columns) for df in renamed_dfs]))
        unique_cols = set()
        for df in renamed_dfs:
            unique_col = set(df.columns) - set(common_col)
            for col in unique_col:
                unique_cols.add(col)
        for df in renamed_dfs:
            for col in unique_cols:
                if col not in df.columns:
                    df[col] = 0.0
            df.sort_index(axis=1, inplace=True)

        put_to_front = ['Base', 'BadSpec']

        tmp_df = renamed_dfs[0].sort_values(by='cpi', ascending=False)
        bmk_sort = tmp_df.index.tolist()


        int_bmks = set(cf.spec_bmks['06']['int'])
        fp_bmks = set(cf.spec_bmks['06']['float'])

        allow_bmks = None
        if hasattr(cf, "benchmark_list") and cf.benchmark_list is not None:
            if len(cf.benchmark_list) > 0:
                allow_bmks = set(cf.benchmark_list)


        def keep_bmk(bmk):
            # filter by benchmark type first
            if cf.INT_ONLY and bmk not in int_bmks:
                return False
            if cf.FP_ONLY and bmk not in fp_bmks:
                return False

            # then filter by benchmark_list if provided
            if allow_bmks is not None and bmk not in allow_bmks:
                return False

            return True


        # ===== filter benchmarks by INT_ONLY / FP_ONLY / benchmark_list =====
        bmk_sort = [b for b in bmk_sort if keep_bmk(b)]


        for df in renamed_dfs:
            df = df.loc[bmk_sort]
            existing_columns = [col for col in put_to_front if col in df.columns]
            df = df[existing_columns + [col for col in df.columns if col not in put_to_front]]
            df = df.drop(columns=['cpi'])
            for to_drop in ['ipc', 'cpi', 'Cycles', 'Insts', 'coverage']:
                if to_drop in df.columns:
                    df = df.drop(columns=[to_drop])

            # draw stacked bar chart
            bottom = np.zeros(len(df))
            highest = 0.0
            if x is None:
                x = np.arange(len(df), dtype=float)
            for component, color, hatch in zip(df.columns, colors[:len(df.columns)], hatches[:len(df.columns)]):
                if have_set_label:
                    label = None
                else:
                    label = component
                ax.bar(x, df[component], bottom=bottom,
                       width=width, color=color, label=label, edgecolor='black', hatch=hatch)
                highest = max((bottom + df[component]).max(), highest)
                bottom += df[component]
            x += width
            have_set_label = True

        # replace x tick labels with df.index with rotation
        ax.set_xticks(x - width * (n_conf + 1) / 2)
        ax.set_xticklabels(bmk_sort, rotation=90)
        ax.tick_params(left=False, bottom=False)
        ax.set_ylabel('Ratio')
        ax.set_xlabel('SPECCPU 2006 Benchmarks')
        left_lim = -1.5 * width
        right_lim = (len(bmk_sort) - 1) + (n_conf - 0.5) * width + width
        ax.set_xlim(left_lim, right_lim)

        # ---- legend between title and plot, flatter ----
        handles, labels_ = plt.gca().get_legend_handles_labels()

        if n_conf == 2:
            ax.set_title(f'[{tag}]: {configs[0]} <-- VS. --> {configs[1]}', pad=35)
        else:
            ax.set_title(f'[{tag}]: {configs[0]}', pad=28)

        ax.legend(
            list(reversed(handles)),
            list(reversed(labels_)),
            fancybox=True,
            framealpha=0.3,
            loc='lower center',
            bbox_to_anchor=(0.5, 1.01),
            ncol=6,
            borderaxespad=0.0,
            handlelength=1.2,
            columnspacing=0.8,
            labelspacing=0.2,
            fontsize=8,
        )

        fig.subplots_adjust(top=0.78)

        out_name = "result.png" if len(analyse_jobs) == 1 else f"result_{tag}.png"
        fig.savefig(osp.join('results', out_name),
                    bbox_inches='tight', pad_inches=0.05, dpi=200)
        plt.close(fig)

if __name__ == '__main__':

    parser = argparse.ArgumentParser(usage='draw top-down stacked bar chart (base/ref weighted csv)')
    parser.add_argument('-b', '--base-weighted-csv', default=cf.OUT_BASE,
                        help=f'base weighted csv path (default: {cf.OUT_BASE})')
    parser.add_argument('-r', '--ref-weighted-csv', default=cf.OUT_REF,
                        help=f'ref weighted csv path (default: {cf.OUT_REF})')
    parser.add_argument('--base-label', default='BASE',
                        help=f'base label (default: BASE)')
    parser.add_argument('--ref-label', default='REF',
                        help=f'ref label (default: REF)')
    parser.add_argument('--issue-width', type=float, default=8,
                        help='issue width for intel topdown analyse (default: 8)')
    opt = parser.parse_args()

    base_csv = opt.base_weighted_csv
    ref_csv  = opt.ref_weighted_csv

    base_exists = bool(base_csv) and osp.exists(base_csv)
    ref_exists  = bool(ref_csv) and osp.exists(ref_csv)

    assert (base_exists or ref_exists), \
        f'Neither base nor ref csv exists: base={base_csv}, ref={ref_csv}'

    if base_exists and ref_exists:
        draw([base_csv, ref_csv], [opt.base_label, opt.ref_label], opt.issue_width)
    elif base_exists:
        draw([base_csv], [opt.base_label], opt.issue_width)
    else:
        draw([ref_csv], [opt.ref_label], opt.issue_width)
