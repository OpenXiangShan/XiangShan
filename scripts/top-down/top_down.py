from multiprocessing import Process, Manager
import threading
import os.path as osp
import os
import resource
import json
import argparse
import psutil
import numpy as np
import pandas as pd
import utils as u
import configs as cf
from draw import draw


def batch(out_csv_path: str):
    paths = u.glob_stats(cf.stats_dir, fname='simulator_err.txt')

    manager = Manager()
    all_bmk_dict = manager.dict()

    semaphore = threading.Semaphore(psutil.cpu_count())

    # for workload, path in paths:
    def extract_and_post_process(gloabl_dict, workload, path):
        with semaphore:
            flag_file = osp.join(osp.dirname(path), 'simulator_out.txt')
            with open(flag_file, encoding='utf-8') as f:
                contents = f.read()
                if 'EXCEEDING CYCLE/INSTR LIMIT' not in contents and 'HIT GOOD TRAP' not in contents:
                    print('Skip unfinished job:', workload)
                    return

            print('Process finished job:', workload)

            d = u.xs_get_stats(path, cf.targets)
            if len(d):

                # add bmk and point after topdown processing
                segments = workload.split('_')
                if len(segments):
                    d['point'] = segments[-1]
                    d['workload'] = '_'.join(segments[:-1])
                    d['bmk'] = segments[0]

            gloabl_dict[workload] = d
        return

    jobs = [Process(target=extract_and_post_process, args=(
        all_bmk_dict, workload, path)) for workload, path in paths]
    _ = [p.start() for p in jobs]
    _ = [p.join() for p in jobs]

    df = pd.DataFrame.from_dict(all_bmk_dict, orient='index')
    df = df.sort_index()
    df = df.reindex(sorted(df.columns), axis=1)

    df = df.fillna(0)
    df.to_csv(out_csv_path, index=True)


def proc_input(wl_df: pd.DataFrame, js: dict, workload: str):
    # we implement the weighted metrics computation with the following formula:
    # weight = vec_weight matmul matrix_perf
    # (N, 1) = (1, W) matmul (W, N)
    # To make sure the matrix_perf is in the same order as the vec_weight,
    # we sort the matrix_perf by point
    assert isinstance(wl_df.iloc[0]['point'], np.int64)
    wl_df = wl_df.sort_values(by=['point'])
    # We also sort the vec_weight by point
    wl_js = dict(js[workload])
    wl_df['cpi'] = 1.0 / wl_df['ipc']
    vec_weight = pd.DataFrame.from_dict(wl_js['points'], orient='index')

    # convert string index into int64
    vec_weight.index = vec_weight.index.astype(np.int64)
    # select only existing points
    vec_weight = vec_weight.loc[wl_df['point']]
    # make their sum equals 1.0
    vec_weight.columns = ['weight']

    vec_weight['weight'] = vec_weight['weight'].astype(np.float64)
    coverage = np.sum(vec_weight.values)
    vec_weight = vec_weight / coverage

    # Drop these auxiliary fields
    to_drop = {'bmk', 'point', 'workload', 'ipc'}
    to_drop = to_drop.intersection(set(wl_df.columns.to_list()))
    wl_df = wl_df.drop(to_drop, axis=1)

    weight_metrics = np.matmul(vec_weight.values.reshape(1, -1), wl_df.values)
    weight_metrics_df = pd.DataFrame(weight_metrics, columns=wl_df.columns)
    # We have to process coverage here to avoid apply weight on top of weight
    weight_metrics_df['coverage'] = coverage
    return weight_metrics_df.values, weight_metrics_df.columns


def proc_bmk(bmk_df: pd.DataFrame, js: dict):
    # Similar to per-input proc, we view the instruction count as the weight
    # and compute weighted metrics with matrix multiplication
    workloads = bmk_df['workload'].unique()
    metric_list = []
    for wl in workloads:
        metrics, cols = proc_input(bmk_df[bmk_df['workload'] == wl], js, wl)
        metric_list.append(metrics)
    metrics = np.concatenate(metric_list, axis=0)
    metrics = pd.DataFrame(metrics, columns=cols)

    input_dict = {}
    for workload in workloads:
        if workload.startswith(workload):
            input_dict[workload] = int(js[workload]['insts'])
    input_insts = pd.DataFrame.from_dict(
        input_dict, orient='index', columns=['insts'])
    # make their sum equals 1.0
    vec_weight = input_insts / np.sum(input_insts.values)
    weight_metric = np.matmul(vec_weight.values.reshape(1, -1), metrics.values)
    return weight_metric, metrics.columns

def compute_weighted_metrics(in_csv_path: str, out_csv_path: str, scale: float = 1.0):
    df = pd.read_csv(in_csv_path, index_col=0)
    bmks = df['bmk'].unique()
    with open(cf.JSON_FILE, 'r', encoding='utf-8') as f:
        js = json.load(f)

    weighted = {}
    cols = None
    for bmk in bmks:
        if bmk not in cf.spec_bmks['06']['int'] and cf.INT_ONLY:
            continue
        if bmk not in cf.spec_bmks['06']['float'] and cf.FP_ONLY:
            continue
        df_bmk = df[df['bmk'] == bmk]
        workloads = df_bmk['workload'].unique()
        n_wl = len(workloads)
        if n_wl == 1:
            metrics, cols = proc_input(df_bmk, js, workloads[0])
        else:
            metrics, cols = proc_bmk(df_bmk, js)
        weighted[bmk] = metrics[0]

    weighted_df = pd.DataFrame.from_dict(weighted, orient='index', columns=cols)

    # ===== NEW: scale all numeric columns by `scale` (except commitInstr) =====
    scale = float(scale)

    # only scale numeric columns; exclude commitInstr
    scale_cols = [c for c in weighted_df.columns if c != 'commitInstr']
    weighted_df[scale_cols] = weighted_df[scale_cols].astype(float) * scale
    if 'commitInstr' in weighted_df.columns:
        weighted_df['commitInstr'] = weighted_df['commitInstr'].astype(float)
    # =========================================================================

    if 'cpi' in weighted_df.columns:
        weighted_df = weighted_df.sort_values(by='cpi', ascending=False)
    else:
        weighted_df = weighted_df.sort_index()
    weighted_df.to_csv(out_csv_path)


def run_one(tag: str, stat_dir: str, csv_path: str, out_path: str, rename_width: int = 1) -> str:
    print(f"\n========== RUN {tag} ==========")
    cf.stats_dir = stat_dir
    batch(csv_path)
    compute_weighted_metrics(csv_path, out_path, rename_width)
    return out_path


if __name__ == '__main__':
    parser = argparse.ArgumentParser(usage='generate top-down results for base/ref (optional)')
    parser.add_argument('-b', '--base-stat-dir', action='store', required=True, default=None,
                        help='base stat output directory (required)')
    parser.add_argument('-r', '--ref-stat-dir', action='store', default=None,
                        help='ref stat output directory (optional)')
    parser.add_argument('-j', '--json', action='store', required=True,
                        help='specify json file', default='resources/spec06_rv64gcb_o2_20m.json')
    parser.add_argument('--base-issue', type=float, default=None,
                        help='base issue width (required when both --base-stat-dir and --ref-stat-dir are provided)')
    parser.add_argument('--ref-issue', type=float, default=None,
                        help='ref issue width (required when both --base-stat-dir and --ref-stat-dir are provided)')
    parser.add_argument('--base-label', type=str, default='BASE',
                        help='label prefix for base (default: BASE)')
    parser.add_argument('--ref-label', type=str, default='REF',
                        help='label prefix for ref (default: REF)')

    opt = parser.parse_args()

    cf.JSON_FILE = opt.json

    if not osp.exists('results'):
        os.makedirs('results')

    if resource.getrlimit(resource.RLIMIT_NOFILE)[0] <= 8192:
        resource.setrlimit(resource.RLIMIT_NOFILE, (8192, 8192))

    out_csv_paths = []
    labels = []

    base_scale = 1.0
    ref_scale  = 1.0
    base_issue = 8.0 if opt.base_issue is None else float(opt.base_issue)
    ref_issue = 8.0 if opt.ref_issue is None else float(opt.ref_issue)
    draw_issue_width = 8.0

    if opt.base_stat_dir and opt.ref_stat_dir:
        if opt.base_issue is None or opt.ref_issue is None:
            raise SystemExit(
                "Error: when both --base-stat-dir and --ref-stat-dir are provided, "
                "you must also provide --base-issue and --ref-issue"
            )
        b = float(opt.base_issue)
        r = float(opt.ref_issue)
        if b <= 0 or r <= 0:
            raise SystemExit("Error: --base-issue and --ref-issue must be > 0")

        if b < r:
            base_scale = r / b
        elif r < b:
            ref_scale = b / r

        print(f"[INFO] issue widths: base={b}, ref={r}")
        print(f"[INFO] computed scales: BASE scale={base_scale:.6f}, REF scale={ref_scale:.6f}")
        draw_issue_width = max(b, r)
    else:
        print("[INFO] only one stat dir provided -> scale=1.0")
        if opt.base_stat_dir:
            draw_issue_width = base_issue
        elif opt.ref_stat_dir:
            draw_issue_width = ref_issue

    if opt.base_stat_dir:
        print(f"[INFO] Run BASE with scale={base_scale:.6f}")
        out_base = run_one("BASE", opt.base_stat_dir, cf.CSV_BASE, cf.OUT_BASE, base_scale)
        out_csv_paths.append(out_base)
        labels.append(opt.base_label)

    if opt.ref_stat_dir:
        print(f"[INFO] Run REF with scale={ref_scale:.6f}")
        out_ref = run_one("REF", opt.ref_stat_dir, cf.CSV_REF, cf.OUT_REF, ref_scale)
        out_csv_paths.append(out_ref)
        labels.append(opt.ref_label)

    if not out_csv_paths:
        raise SystemExit("Error: please provide at least one of --base-stat-dir or --ref-stat-dir")
    draw(out_csv_paths, labels, draw_issue_width)
