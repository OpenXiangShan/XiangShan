这里RVCDecoder的功能是：把RVC指令展开成RVI指令。该模块在/nfs/home/dengzibin/xiangshan/XiangShan/src/main/scala/xiangshan/frontend/ifu/RvcExpander.scala中调用。在原来的RVCDecoder实现中，使用的是rocket-chip的实现，在/nfs/home/dengzibin/xiangshan/XiangShan/rocket-chip/src/main/scala/rocket/RVC.scala。但是原实现不好维护，现在需要你进行重写。
重写希望参照/nfs/home/dengzibin/xiangshan/XiangShan/src/main/scala/xiangshan/backend/decode中的实现方法，通过BitPat以及手册定义的指令类型进行区分。如RVC分为：CR，CI，CSS，CIW，CL，CS，CA，CB，CJ九种类型，每种类型需要不同函数实现。另外还要实现ZC的指令。压缩指令的手册的网站在https://docs.riscv.org/reference/isa/v20260120/unpriv/c-st-ext.html（RVC指令）和https://docs.riscv.org/reference/isa/v20260120/unpriv/zc.html（ZC）中。
实现要求：
1. 原来实现/nfs/home/dengzibin/xiangshan/XiangShan/rocket-chip/src/main/scala/rocket/RVC.scala中实现的所有指令都要实现。
2. RVCDecoder的顶层信号需要与原实现完全一致，这意味着需要额外实现指令拼接。
3. 代码在/nfs/home/dengzibin/xiangshan/XiangShan/src/main/scala/xiangshan/frontend/ifu/RVCDecoder下实现，并修改/nfs/home/dengzibin/xiangshan/XiangShan/src/main/scala/xiangshan/frontend/ifu/RvcExpander.scala，使其调用重新实现后的代码。

只需要把chisel代码写出来，先不用管编译是否通过。从头开始写，不要参考/nfs/home/dengzibin/xiangshan/xs-rvcdecoder/src中的文件
