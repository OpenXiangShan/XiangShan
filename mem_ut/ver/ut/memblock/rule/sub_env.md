› ut验证环境中可指定内部module做一个sub_env,该sub_env主要目的为了指定的module接口monitor采样然后送往该module rm check对比，需要建立一套规则：
（首先基于这个目的，基于目前mem_ut环境构建一套demo模块到sub_env的demo文件夹中：范围为下面的流程中提到demo模块均需要建立）
  1 在memblock/subenv/文件夹新键以module命名
  的：<modulenam>_env文件夹，之后所有sub_env相关文件均放入这个文件夹内部；
  2 基于该module在XiangShan/
  build_memblock的verilog文件中找到对应的module文件,提取该模块顶层IO按照agent概念分类汇总一个agent拆分方案
  文档（文档中需要包含agent分类以及各个agent对应内部input和output信号）放入<modulenam>_env文件夹，并提示用
  户review;
  3 用户reviewc成功之后基于agent拆封方案文档，参考已有agent目录中demo_agent，在<modulenam>_env文件夹中
  新增对应的agent目录，每个agent目录中包含该agent的所有文件（monitor.sv, interface.sv, transaction.sv）;
  4 <modulenam>_env中参考demo_rm创建<modulenam>_rm.sv文件；
  5 <modulenam>_env中创建env.sv文件参考demo_env完成所有该模块agent和rm的例化连接；
  6 <modulenam>_env中创建connect文件夹，参考agent_connect_demo文件，新增对应模块的agent connect文件,创建一个<modulenam>_connect.sv文件将所有agent connect文件include进来，之后memblock/tb/top_tb.sv中include这个文件，用于将dut_module连接到sub_env的验证环境中。
  7 <modulenam>_env中创建 <modulenam>_env.f并添加所有该模块的文件路径
  8、将<modulenam>_env.f添加到sub_env/sub_env.f
  9、将<modulenam>_env例化到memblock_env.sv中并通过memblock_env_cfg.sv中对应的配置控制例化
  10、最后需要进行编译解决所有编译问题
