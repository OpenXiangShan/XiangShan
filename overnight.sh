make emu REMOTE=100
# ./build/emu -b 4340927 -e 4353927 -i /home/ci-runner/xsenv/workloads/linux-hello/bbl.bin -s 0 --dump-wave 2>linux.log
./build/emu -b 23820203 -e 23826203 -i /bigdata/xs-workloads/2021.2.5-linux-4.18-nosmp-redis/bbl.bin -s 0 --dump-wave 2>linux.log