
import sys
import argparse
import yaml
import os
from datetime import date
import time

github_yaml = ""
workspace = ""
nemu_home = ""
am_home = ""
head_sha = ""
wave_home = ""
perf_home = ""
set_env = ""
sh_path = ""

def remove_empty(list_str):
  list_new = []
  for s in list_str:
    if not (s == ""):
      list_new.append(s)
  return list_new

def split_cmd(cmd):
  c = cmd.replace("\\\n", " ")
  c = remove_empty(c.split("\n"))
  cs = []
  for ci in c:
    ci = ci.replace("\\\n", " ")
    ci = ci.replace("\n\n", "\n")
    ci = ci.replace("  ", " ")
    if ci[-1] == "\n":
      ci = ci[:-1]
    cs.append(ci)
  return cs

def parse_yaml(yaml_file):
  file_data = open(yaml_file, "r", encoding="utf-8").read()
  yaml_data = yaml.load(file_data, Loader=yaml.CLoader)
  return yaml_data

def show(test_info):
  name = test_info["name"]
  print(name)

def run_test(test_info, numa, run_mode):
  name = test_info["name"]
  coe_key = "continue-on-error"
  to_key = "timeout-minutes"
  s_key = "steps"
  continue_on_error = True if (coe_key not in test_info.keys()) else (False if (test_info[coe_key] == "false") else True)
  timeout_minutes = 9999 if (to_key not in test_info.keys()) else int(test_info[to_key])
  if s_key not in test_info.keys():
    print(name, " ", s_key, " not found in yaml, skip")
    sys.exit()
  steps_raw = test_info[s_key]
  # print("Steps")
  # print(steps)
  steps = {}
  for s in steps_raw:
    if "name" in s.keys():
      steps[s["name"]] = s["run"]
  # print(steps)

  replace_list = [
    ["--numa", "--numa" if numa else ""],
    ["$GITHUB_WORKSPACE", f"{workspace}"],
    ["$HEAD_SHA", f"{head_sha}"],
    ["$PERF_HOME", f"{perf_home}"],
    ["$WAVE_HOME", f"{wave_home}"],
    ["$AM_HOME", f"{am_home}"]
  ]

  steps.pop("set env")

  for s in steps.keys():
    for r in replace_list:
      steps[s] = steps[s].replace(r[0], r[1])

  cmd = []
  for s in steps.keys():
    cs = split_cmd(steps[s])
    cmd = cmd + cs

  if run_mode:
    for c in cmd:
      f_c = set_env + " " + c
      print(f"[CMD] {f_c}", flush=True)
      os.system(f_c)
  else:
    if (sh_path is None):
      print("sh_path is None")
      sys.exit()
    elif (not os.path.exists(sh_path)):
      os.mkdir(sh_path)


    sh_file_name = os.path.join(sh_path, "_".join(name.strip().replace("-", "").split())+".sh")
    with open(sh_file_name, "w") as tmp_sh:
      tmp_sh.write(f"mkdir -p {wave_home}\n")
      tmp_sh.write(f"mkdir -p {perf_home}\n")
      for c in cmd:
        print(c)
        tmp_sh.write(c+"\n")

if __name__ == "__main__":
  # Usage:
  # 1. run ci test
  # python3 scripts/local_ci.py --xs-path $(pwd) --run
  # More Params:
  # --pick-test MC: only run 'EMU - MC'

  # 2. print ci test name
  # python3 scripts/local_ci.py --xs-path $(pwd) --show-test
  # This can also use --pick-test

  # 3. print ci test command into splited sh files. Run the sh manualy.
  # python3 scripts/local_ci.py --xs-path $(pwd) --sh-path /nfs/home/zhangzifei/work/xs-master/ci-sh
  # just remove --run

  # Other Params:
  # --numa: use numa ctrl, require eypc
  # --head-sha: magic word, default is today's date
  # --nemu-home/--am-home: don't know if it is used

  parser = argparse.ArgumentParser(description="run ci locally")
  parser.add_argument("--xs-path", type=str, help="XiangShan, NOOP_HOME")
  parser.add_argument("--nemu-home", type=str, help="NEMU_HOME")
  parser.add_argument("--am-home", type=str, help="AM_HOME")
  parser.add_argument("--sh-path", type=str, help="ci's sh file path")
  parser.add_argument("--head-sha", type=str, help="magic word")
  parser.add_argument("--run", action='store_true', help="run test, not gen sh")
  parser.add_argument("--numa", action='store_true', help="epyc numactl")
  parser.add_argument("--show-test", action="store_true", help="print test case")
  parser.add_argument("--pick-test", type=str, help="pick only one test")

  args = parser.parse_args()

  print(args)

  workspace = os.getenv("NOOP_HOME") if (args.xs_path is None) else args.xs_path
  head_sha = date.today().strftime(r"%Y%m%d") if (args.head_sha is None) else args.head_sha
  wave_home = os.path.join(workspace, "wave", head_sha)
  perf_home = os.path.join(workspace, "perf", head_sha)
  github_yaml = os.path.join(workspace, ".github/workflows/emu.yml")
  nemu_home = os.getenv("NEMU_HOME") if (args.nemu_home is None) else args.nemu_home
  am_home = os.getenv("AM_HOME") if (args.am_home is None) else args.am_home
  set_env = f"NEMU_HOME={nemu_home}  NOOP_HOME={workspace}  WAVE_HOME={wave_home}  PERF_HOME={perf_home}  AM_HOME={am_home}"
  sh_path = f"{workspace}/ci-sh" if (args.sh_path is None) else args.sh_path

  print("workspace(NOOP_HOME): ", workspace)
  print("head_sha: ", head_sha)
  print("wave_home: ", wave_home)
  print("perf_home: ", perf_home)
  print("github_yaml: ", github_yaml)
  print("nemu_home: ", nemu_home)
  print("am_home: ", am_home)
  print("set_env: ", set_env)
  print("sh_path", sh_path)

  input("Press Enter to continue")

  ci_tests = parse_yaml(github_yaml)["jobs"]

  if (args.show_test):
    for test in ci_tests.keys():
      show(ci_tests[test])
  else:
    for test in ci_tests.keys():
      if args.pick_test is not None:
        if (args.pick_test in ci_tests[test]["name"]):
          run_test(ci_tests[test], args.numa, args.run)
      else:
        run_test(ci_tests[test], args.numa, args.run)