import os.path as osp
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import configs as cf


def draw():
    results = {
        'XS': (cf.OUT_CSV, 'XS'),
    }

    configs = list(results.keys())

    color_types = 10
    cmap = plt.get_cmap('tab10')
    color_index = np.arange(0, 1, 1.0 / color_types)
    colors = [cmap(c) for c in color_index] * 3
    hatches = [None] * color_types + ['//'] * color_types + ['|'] * color_types

    n_conf = len(configs)
    # Draw stacked bar chart for each simulator
    width = 0.8 / n_conf
    # set figure size:

    fig, ax = plt.subplots()
    fig.set_size_inches(8.0, 5.0)

    x = None
    have_set_label = False

    dfs = [pd.read_csv(result[0], index_col=0)
           for _, result in results.items()]
    common_bmk = list(set.intersection(*[set(df.index) for df in dfs]))
    dfs = [df.loc[common_bmk] for df in dfs]

    rename = True
    fine_grain_rename = False
    renamed_dfs = []
    for df in dfs:
        to_drops = []
        sorted_cols = []

        def rename_with_map(df, rename_map):
            for k in rename_map:
                if rename_map[k] is not None:
                    if rename_map[k].startswith('Merge'):
                        merged = rename_map[k][5:]
                        if merged not in df.columns:
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
            df.drop(columns=to_drops, inplace=True)

        # Merge df columns according to the rename map if value starting with 'Merge'
        if rename:
            if fine_grain_rename:
                rename_with_map(df, cf.xs_fine_grain_rename_map)
            else:
                rename_with_map(df, cf.xs_coarse_rename_map)

                icount = 20 * 10 ** 6
                if 'BadSpecInst' in df.columns:
                    df['BadSpecInst'] += df['Base'] - icount
                else:
                    df['BadSpecInst'] = df['Base'] - icount
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

    for df in renamed_dfs:
        df = df.loc[bmk_sort]
        df = df[put_to_front +
                [col for col in df.columns if col not in put_to_front]]
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
    ax.set_xticks(x - width * len(results) / n_conf - 0.25)
    ax.set_xticklabels(bmk_sort, rotation=90)
    ax.tick_params(left=False, bottom=False)
    ax.set_ylabel('Slots')
    ax.set_xlabel('SPECCPU 2006 Benchmarks')

    handles, labels = plt.gca().get_legend_handles_labels()
    ax.legend(reversed(handles), reversed(labels), fancybox=True,
              framealpha=0.3,
              loc='best',
              ncol=3,
              )
    if n_conf == 2:
        ax.set_title(f'{configs[0]} <-- VS. --> {configs[1]}')

    fig.savefig(osp.join('results', 'result.png'),
                bbox_inches='tight', pad_inches=0.05, dpi=200)
