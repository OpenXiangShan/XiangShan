

#general
pl := UVM_HIGH
COVER_DEFINE = +define+MEMBLOCK_UT_FCOV
CMP_OPTIONS += +define+MEMBLOCK_UT
VRD_OPTIONS += +define+MEMBLOCK_UT
#IF_ADD_DLY_OPTIONS += +define+INTERFACE_ADD_DELAY

#vcs extern declare
#指定覆盖率 exlude文件，在extern_declare_cfg.mk中修改此变量指定
##e.g.
##vcs  >>assign the el file >>>COV_EX_OPTION = -elfile ../cfg/pred_exclude.el
##xrun >>assign the el dir  >>>COV_EX_OPTION = ../cfg/el/
COV_EX_OPTION =
COV_ADD_MERGE =

INITREG_CFG_FILE :=
#xrun extern declare
INSTANCE_NAME := 'top_tb.U_MEMBLOCK'

#SYSC_COMP_OPTS += syscan -cpp g++ ../sysc/sc_add.cpp:sc_add -Mdir=${CSRC_FILE}
#CMP_OPTIONS += -cpp g++ -cc gcc -sysc
