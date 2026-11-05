"""Summarize only the complete predefined ten-point query screen; never estimate a partial suite."""
from collections import defaultdict
from pathlib import Path
import argparse
import hashlib
import json
import math


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve()
    path=root/'wb-query/analysis/mechanism-evidence.json';data=json.loads(path.read_text());spec=json.loads((root/'evidence/fdiv-wb-query/stage/experiment.json').read_text());cases={c['point']:c for c in spec['cases']};rows={r['point']:r for r in data['rows']}
    assert data['stage']=='wb-query' and data['source']=='b0a22b93a867c5dff56b1b3fb32708c9e6363b01' and data['complete'] and data['completed']==data['expected']==len(cases)==len(rows)==10 and cases.keys()==rows.keys(),'Complete original ten-point screen required'
    workloads=defaultdict(list)
    for point,row in rows.items():
        ratio=row['comparison']['control_cpi']/row['comparison']['candidate_cpi'];assert abs(ratio-1-row['comparison']['speedup'])<1e-14
        if ratio==1:assert row['zero_cpi_equality']['neutral'] and all(row['zero_cpi_equality']['checks'].values())
        workloads[cases[point]['workload']].append(math.log(ratio))
    mean_by_workload={name:math.exp(sum(values)/len(values))-1 for name,values in workloads.items()}
    result=dict(source=data['source'],closed_points=10,covered_workloads=len(workloads),positive=sum(r['comparison']['speedup']>0 for r in rows.values()),negative=sum(r['comparison']['speedup']<0 for r in rows.values()),full_neutral=sum(r['comparison']['speedup']==0 for r in rows.values()),equal_point_geometric_gain=math.exp(sum(math.log(1+r['comparison']['speedup']) for r in rows.values())/10)-1,equal_covered_workload_geometric_gain=math.exp(sum(sum(v)/len(v) for v in workloads.values())/len(workloads))-1,within_workload_equal_point_geometric_gain=mean_by_workload,mechanism_evidence_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),scope='Complete predefined ten1M+1M short windows only. Equalpoint and equalcoveredworkload screen statistics match the previous selected-screen convention; neither uses the145point profile nor estimates full-suite score. Existinglbm regression remains, newformal controls and originalCI are separate.',decision='Positive complete selected screen supports continuing the preregistered three20Mformal checks. No default adoption or newCI from this summary alone.')
    (root/'wb-query/analysis/complete-ten-screen.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))


if __name__=='__main__':main()
