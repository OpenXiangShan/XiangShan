"""Gate the existing query candidate's endpoint validation on one original closed CoreMark."""
from pathlib import Path
import argparse,hashlib,json,sqlite3,sys

TABLES=['Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0']


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream,'sha256').hexdigest()


def compare(before,after):
    from native_perf import read_blocks,counter_multisets
    blocks=[read_blocks(folder/'simulator_err.txt') for folder in [before,after]]
    assert all(len(block)==1 for block in blocks)
    checks=dict(all_native_multivalues_equal={stamp:counter_multisets(block) for stamp,block in blocks[0].items()}=={stamp:counter_multisets(block) for stamp,block in blocks[1].items()})
    tables={}
    for table in TABLES:
        values=[]
        for folder in [before,after]:
            with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
                values.append(db.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall())
        equal=values[0]==values[1];checks[table+'_equal']=equal
        tables[table]=dict(rows=[len(rows) for rows in values],all_rows_equal=equal)
    return dict(passed=all(checks.values()),checks=checks,tables=tables)


def analyze(root):
    stage=root/'wb-query-endpoint-validation';receipt_path=stage/'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True,passed=False,database_read=False,reason='Original query endpoint build/CoreMark closure not archived')
    control=root/'tea-exact-endpoint'
    receipts=[json.loads((path/'closure-receipt.json').read_text()) for path in [control,stage]]
    expected=['addd8cc4d35a5a3f5f3fa3cfe8371280ff787807','2915a6b022bd957570bd961a01611026c2220fb7']
    for folder,receipt,source in zip([control,stage],receipts,expected):
        assert receipt['ready'] and not any(receipt[key] for key in ['original_build_controller_live','original_smoke_controller_live','original_guest_live'])
        assert receipt['source_head']==receipt['build']['source']==source
        assert receipt['build']['phase']==receipt['smoke']['phase']=='complete'
        for name,entry in receipt['files'].items():
            path=folder/name
            assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
        result=json.loads((folder/'coremark/result.json').read_text());process=json.loads((folder/'coremark/process.json').read_text());launch=json.loads((folder/'coremark/launch-observation.json').read_text())
        assert result['source']==source and result['binary_sha256']==receipt['build']['binary_sha256'] and result['exitcode']==0 and result['difftest_passed'] and result['original_process_closed']
        assert process==receipt['guest']==result['process'] and launch['live'] and all(launch[key]==value for key,value in process.items())
        assert (folder/'coremark/exitcode').read_text().strip()=='0'
    assert receipts[1]['build']['pid']==431191 and receipts[1]['build']['start_ticks']=='61520314' and receipts[1]['smoke']['pid']==444711 and receipts[1]['smoke']['start_ticks']=='61524832'
    commands=[json.loads((folder/'coremark/command.json').read_text()) for folder in [control,stage]]
    for flag in ['-I','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
        assert commands[0][commands[0].index(flag)+1]==commands[1][commands[1].index(flag)+1]
    assert commands[1][commands[1].index('--dump-select-db')+1].split()==TABLES
    assert receipts[0]['smoke']['cst_sha256']==receipts[1]['smoke']['cst_sha256'] and receipts[0]['smoke']['image_sha256']==receipts[1]['smoke']['image_sha256']
    sys.path.insert(0,str(root))
    comparison=compare(control/'coremark',stage/'coremark')
    return dict(pending=False,database_read=True,passed=comparison['passed'],comparison=comparison,source=expected[1],control_source=expected[0],binary_sha256=receipts[1]['build']['binary_sha256'],control_binary_sha256=receipts[0]['build']['binary_sha256'],consumer_sha256=digest(Path(__file__)),native_parser_sha256=digest(root/'native_perf.py'),receipt_sha256=[digest(folder/'closure-receipt.json') for folder in [control,stage]],closed_input_sets=[dict(root=receipt['root'],files=receipt['files']) for receipt in receipts],performance_speedup=None,scope='Original query validation CoreMark vs original addd endpoint CoreMark: both controllers/guest closed, source/binary/config/raw hashes matched before DB. Every native multivalue, including all existing diagnostic/endpoints, and six complete tables compared. This gate permits focused validation only, not SPEC improvement, full-suite score or default adoption.')


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path)
    args=parser.parse_args();root=args.report_dir.resolve();result=analyze(root)
    if not result['pending']:
        output=root/'wb-query-endpoint-validation/analysis';output.mkdir(exist_ok=True)
        (output/'coremark-gate.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__=='__main__':
    main()
