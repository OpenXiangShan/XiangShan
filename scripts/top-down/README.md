# top-down 分析工具

最新的 top-down 分析工具已经与 env-scripts 集成。在使用 `xs_autorun.py` 完成 checkpoint 的运行后，使用 `--report-top-down` 参数即可！
本仓库集成了 top-down 分析所需要的工具。

## 运行仿真

1. 将仿真文件拷贝至 `emus` 目录下，如 `emus/emu_20220316_0`
2. 将要运行的测试名称写在 `file.f` 中，具体格式可以参考已有文件（目前最大并行度设置为 16 个 emus，以 fifo 顺序运行 `file.f` 中的程序，因此可按需调整该文件的内容）
3. 在 tmux/screen 中运行 `./run_emu.sh <emu>`，或是 `nohup ./run_emu.sh <emu>`，以忽略退出终端时的 hup 信号
4. 运行结束后，将自动进行下列操作

### 提取性能计数器

1. 性能计数器位于 `${spec_name}/${emu}.dir` 中，如 `spec06_rv64gcb_o2_20m/emu_20220316_0.dir`
2. 性能计数器包含 warmup 过程的结果，因此需要先删去每个文件的前半部分，脚本会自动在 `${spec_name}/${emu}.dir/csv` 生成中间文件
3. 提取 csv 格式的 top-down 性能计数器
4. 删除中间文件

```bash
sed "1,$(($(cat ${dir}/${spec_name}/${emu}.dir/${name}.log | wc -l) / 2))d" ${dir}/${spec_name}/${emu}.dir/${name}.log >${dir}/${spec_name}/${emu}.dir/csv/${name}.log
${dir}/top-down.sh ${dir}/${spec_name}/${emu}.dir/csv/${name}.log
rm ${dir}/${spec_name}/${emu}.dir/csv/${name}.log
```

### 生成图表

生成图表使用的是 `top_down.py`，其会被 `run_emu.sh` 自动调用：

```bash
$python ${dir}/top_down.py ${name} ${dir}/${spec_name}/${emu}.dir ${emu} # python ./top_down.py title dir suffix
```

`top_down.py` 中需要关注的代码如下：

```python
# top_down.py
(
    Page(page_title=title, layout=Page.SimplePageLayout)
    .add(process_one(directory + "/csv/" + title + ".log.csv", title + "_" + suffix))
    .render(directory + "/html/" + title + ".html"))
```

每一个以 `.add` 开头的行代表了一个子图，可以按需增删这些行。
