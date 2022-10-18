import json

json_path = '/nfs-nvme/home/share/checkpoints_profiles/spec06_rv64gcb_o2_20m/utils/simpoint_coverage0.3_test.json'

with open(json_path, 'r') as f:
	json_content = json.loads(f.read())
	for name, cycle_weight in json_content.items():
		for cycle, weight in cycle_weight.items():
			print(f'{name}_{cycle}_{weight}')
