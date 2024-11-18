import matplotlib.pyplot as plt

data = '''
robsize = 1024      ipc: 0.548546
robsize = 512       ipc: 0.548546
robsize = 256       ipc: 0.548546
robsize = 128       ipc: 0.548546
robsize = 64        ipc: 0.550934
robsize = 32        ipc: 0.526288
robsize = 16        ipc: 0.435863
robsize = 8         ipc: 0.306063
robsize = 4         ipc: 0.190299
robsize = 2         ipc: 0.548546
'''

robsize = []
ipc = []

for line in data.strip().split('\n'):
    parts = line.strip().split()
    r = int(parts[2])
    i = float(parts[4])
    robsize.append(r)
    ipc.append(i)

plt.plot(robsize, ipc, marker='o')
plt.xlabel('ROB 大小')
plt.ylabel('IPC')
plt.title('IPC 随 ROB 大小变化的曲线图')
plt.grid(True)
plt.show()