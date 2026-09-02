# User Prompts

This file records the user requests issued during the MemBlock verification
work. The original `/goal` is reproduced first, followed by the later requests
in conversation order. Command-only follow-ups are listed separately from
natural-language requirements.

## Initial `/goal` (verbatim)

The earliest MemBlock goal was recovered from the local Codex history record
for this thread. It is reproduced below rather than paraphrased:

```text
/goal XiangShan是一个高性能CPU。我现在想针对这个CPU的MemBlock（主要包含LSU和Dcache）做block level testing。请你详细阅读代码，基于make verilog出来的verilog代码，（1）分析memblock的端口信息，分解出可行的随机测试激励维度，尝试在IO层找到验证variant（assert，比如未被刷新的指令一定会执行完成且数据正确），并找到端口的约束（assume）；（2）测试是否可以用github XS-MLVP项目里面的UT框架，快速搭建其一套测试环境，最好是基于C++或者python的，这样维护起来比较方便；（3）基于UVM的验证方法学，合理确定软件架构，实现memblock BT测试环境，放在tests/memblock目录下；（4）确认BT框架是否能够正确运行，并修正所有false positive/negative，持续进行仿真；（5）对memblock进行BT测试，尽可能多地进行仿真测试，可以用本CPU的8线程进行仿真（如果8线程更快的话）。（6）整理tests/memblock测试框架代码，并将运行的随机BT测试结果报告给我，仿真测试时长需要超过4个小时。在测试过程中，如果你发现有CPU BUG，需要记录并修复，每个修复独立一个commit，同时汇总形成BUG修复报告给我
```

Source: `/home/xuyinan/.codex/history.jsonl`, session
`01a051f8-7787-7003-b87a-79852aa669a7`.

## Consolidated Goal (reconstructed)

The later requests refine the initial goal into a defensive MemBlock/LSU
functional-verification environment that mixes scalar and vector memory traffic
under constrained-random control, exercises cache/TLB/translation/virtualiza-
tion/misalignment/backpressure and queue-wrap conditions, uses explicit
design-independent scoreboards and memory oracles, audits and reproduces
relevant LSU bug fixes since January, and can sustain a long campaign of at
least six hours without weakening failure detection.

The goal is processor functional verification and defensive validation, not an
attack or security exploit. This paragraph is a summary, not a recovered quote.

## Later Requirements

1. `you should ensure things like vectored load/store is tested`

2. `You should check all IO pins and extensively explore their possible values to maximize input space and avoid missing tests`

3. `既然你跑了这么久没有BUG，请你同时回头看一下kunminghu-v2自今年1月开始的bug fix历史，挑选一些memblock的问题，尝试revert并看你的UT是否能测试出来吧。请不要停止工作直到memblock测试环境能够复现出他们。`

4. `我们不是在尝试攻击，而是在尝试完成处理器的功能验证，做防御。请继续工作。`

5. `我看了一下你的测试，random-loads, random-vector-loads等等，这些可以继续混合发吗？就是各种loads/store/vector-load/vector-store甚至现在未覆盖的指令类型，以及dcache/tlb等等各种场景，都混合在一起发，可以吗。如果可以的话，你需要先增强一下现有的UT框架，尽可能完善覆盖各种LSU测试场景。在此基础上，一方面保证完善的测试能持续运行4小时以上，另一方面要尽可能发现CPU的功能BUG，用于CPU防御性验证。`

6. `我看了一下，觉得1. 验证覆盖还是不够多，包括各种指令的类型、虚拟化的情况、cache miss的情况，可能需要有更多的激励进来；2. 验证计划里面的contract/oracle似乎不够明确，最好是一些清晰、长期没问题的oracle，而尽可能避免特定design detail里面的约束。3. 你前面说的非对齐等场景，需要添加验证用例。4. 最后的UT测试，请加大到6小时没问题，尽可能多得跑更长的用例，覆盖多样的压力场景。`

7. `我看了一下，你还是在跑单独的用例，但是这样很难撞出问题。你需要同时把所有可能的激励都混合进去，然后基于constrained random的思路来持续跑UT测试，而且长度也要放到更长，而不是跑一些短用例自娱自乐。`

8. `continue`

9. `不要停止现有工作。把我历史prompt输出到文档给我，这样我好知道我都发出了哪些命令。`

## Command-Only Follow-Ups

These commands continued or resumed the same MemBlock goal and are included
because they affect the requested work history:

1. `/goal resume`
2. `/goal resume`
3. `/goal resume`
4. `/goal resume`
5. `/goal resume`

The first five entries were issued after the initial goal and before the later
requirements above. The exact original `/goal` text is the authoritative record;
the consolidated paragraph is intentionally labeled as reconstructed.
