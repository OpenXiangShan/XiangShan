make sim-verilog CONFIG=NanHuGFPGAConfig RELEASE=1 MFC=1 WITH_CHISELDB=0 WITH_CONSTANTIN=0 PLDM=1
cp -r build/*.v ../env-scripts/simulation/src/build
cp build/*.cpp ../env-scripts/simulation/src/csrc
cd ../env-scripts/simulation/src
cp FlashHelper.v RAMHelper.v SDHelper.v SimJTAG.v build
