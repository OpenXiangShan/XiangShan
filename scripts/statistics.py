mymap = {}
last = ""

with open("../build/XSSimTop.v", "r") as f:
    line = f.readline()
    cnt = 0
    while(line):
        if "module " in line:
            if last!="" :
                mymap[last] = cnt
            last = line[6:-2]
            cnt = 1
        else:
            cnt = cnt + 1
        line = f.readline()
    for k,v in mymap.items():
        print(k, v)