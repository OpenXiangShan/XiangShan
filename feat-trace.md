请生成一份计划：修复高密度 ROB 压缩下的 trace 扩展功能特性。

1. trace 扩展是用于增加处理器硅后调试的一个扩展特性。其对于一条/几条指令，会记录这么几个内容：
    1. itype: 指令的类型，会有normal，taken的分支指令、不taken的分支指令、call、return、中断、异常等
    2. iretire：指令段的退休长度，指的是这一段指令的指令字长（以半字数，即2B为单位）
    3. ilastsize：指令段中最后一条指令的大小，如果是RVC，2B指令，则该值为0;否则，如果是4B指令，该值为1
2. 对于我们实现的 ROB 压缩来说，因为我们将分支、跳转指令视为 complex 指令，一个 ROB entry 可以在前后两个槽中存储两条分支、跳转指令。因此对于一个 ROB entry 来说，其至少需要 2 个 itype 信息和 2 个 ilastsize 信息。但是由于我们的压缩类型有 CC、SC、CS这三种，以及连续的simple指令独占一个entry的 NORMAL。C即complex是可以通过其对应槽的RVC来得知其指令字长，从而推断其iretire的；因此iretire这里只需要存一份，记录SC类型中S的指令字长、或者CS中S的指令字长，以及 NORMAL 中的指令字长。
3. 需要完善 Rename 从 compressUnit 获取压缩 mask，从而计算 itype，iretire, ilastsize 等信息的逻辑、以及其入队 ROB entry，更新，提交给trace接口的逻辑。、
4. 为了简单验证功能的正确性，将formerLen替换为从trace推断。如果其是CC、CS类型的，其formerLen根据RVC(0)推断；如果其是SC或者NORMAL的，则formerLen根据iretire获得。

最终实现后，需要编译并使用 ~/nexus-am/apps/coremark/build/coremark-riscv64-xs.bin 作为workload 作为验证。并编写一个英文的commit message和英文的commit message body。
