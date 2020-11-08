#compliance_tests/兼容性测试


一、目的：
	主要将官网(https://github.com/riscv/riscv-compliance)给出的测试用例放到自己设计的mode上运行，将生成的实际测试结果同预期结果进行比对；
从而根据比对结果判断设计的mode是否具有兼容性。(64位机器要满足基础整数rv64i指令集，和基础整数乘除rv64im指令集)


二、运行步骤：

	1.确保环境变量中有:(1)NEMU_HOME="/home/usr/work/NEMU"

		(2)AM_HOME="/home/usr/work/nexus-am"

		(3)NOOP_HOME="/home/usr/work/XiangShan"

	  确保有NEMU和nexus-am。

	2.compliance_tests/目录下有4个makefile脚本文件。

		2.1首先根据自己要测试的ISA指令集，vim分别进入4个脚本中，对变量RISCV_ISA进行配置：

			如：
				RISCV_ISA  ?= rv64i （有rv64i、rv64im、（rv32i）可选）

		2.2配置完毕后：

			First step 输入执行：make -f Makefile_complie；  (生成中间文件以及待执行的.bin文件)

			Second step 输入执行：make -f Makefile_log ; (生成log文件，期间需要多次ctrl + z，并重复执行)

			Third step 输入执行：make -f Makefile_signature ； (生成需要的.signature_output文件)

			Last step 输入执行： make  ; (将生成的.signature_output文件与.reference_output文件进行对比，并显示出对比结果)(也可以将结果输出到result.log文件里)
		
三、预期结果示例：
	
Compare to reference files ...

Check                     DIVW ... OK
Check                     MULW ... OK
Check                    REMUW ... OK
Check                     REMW ... OK
--------------------------------
OK: 4/4

