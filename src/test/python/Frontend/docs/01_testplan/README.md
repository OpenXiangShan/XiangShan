# Frontend BT 验证方案与验证设计

验证方案按版本保存，正文、导出文件及附件放在同一版本目录；验证设计和 BPU 专题分别维护各自版本。组内修订的验证方案为 **V1.3**，新的验证设计 **V0.1** 为独立评审稿，承接 V1.3 的范围和环境说明。

| 目录 | 文档入口 | 定位与维护方式 |
| --- | --- | --- |
| `v1.0/` | [验证方案 V1.0](v1.0/Frontend_BT_验证方案_v1.0.md) | 原始方案，历史对照 |
| `v1.1/` | [验证方案 V1.1](v1.1/Frontend_BT_验证方案.md) | 既有执行方案，保留原文件名和章节引用 |
| `v1.2/` | [Markdown](v1.2/Frontend_BT_验证方案_v1.2.md) · [Word](v1.2/Frontend_BT_验证方案_v1.2.docx) · [PDF](v1.2/Frontend_BT_验证方案_v1.2.pdf) | Markdown 为内容源，附件及导出脚本随版保存 |
| `v1.3/` | [Markdown](v1.3/Frontend_BT_验证方案_v1.3.md) · [Word](v1.3/Frontend_BT_验证方案_v1.3.docx) · [PDF](v1.3/Frontend_BT_验证方案_v1.3.pdf) | 组内修订版；Word 为内容源，Markdown 为内容镜像 |
| `verification_design/v0.1/` | [验证设计：场景推导与测试点分解](verification_design/v0.1/Frontend_BT_验证设计_场景推导与测试点分解_v0.1.md) | 新评审稿；以 Markdown 维护场景来源、分解方法和检查闭环 |
| `bpu/v0.1/` | [Markdown](bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.md) · [Word](bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.docx) · [PDF](bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.pdf) | BPU 专项待评审方案，Markdown 为内容源 |
| `references/` | [MemBlock 验证方案](references/memblock验证方案.docx) | 共享格式参考，供 V1.2 和 BPU 导出使用 |

测试点和状态仍维护在[唯一测试点主表](../02_testpoint/Frontend_testpoint_v1.0.csv)，不随方案版本复制。

## 同步与导出

以下命令均在 `01_testplan/` 目录执行。

修改 V1.3 Word 后，同步同版 Markdown 与 PDF：

```bash
bash v1.3/frontend_plan_v1.3_assets/sync_from_word.sh
```

修改 V1.2 Markdown 后，重建同版 Word 与 PDF：

```bash
bash v1.2/frontend_plan_v1.2_assets/sync_frontend_plan.sh
```

修改 BPU 专项 Markdown 后，复用 V1.2 格式和导出工具：

```bash
bash v1.2/frontend_plan_v1.2_assets/sync_frontend_plan.sh bpu/v0.1/BPU_验证策略与参考模型建设方案_v0.1.md
```

V1.2 和 BPU 的 Word/PDF 会被 Markdown 导出覆盖；V1.3 的 Markdown/PDF 会被 Word 同步覆盖。新验证设计稿直接维护 Markdown。

脚本使用 LibreOffice 兼容保存并生成 PDF；本节点没有 LibreOffice 时，通过 `open19` 完成。字体嵌入依赖 `fonttools`，共享节点的依赖位于 `~/.local/share/frontend-plan-python`，完整 Noto CJK 字体位于 `~/.local/share/frontend-plan-fonts`；也可通过 `FRONTEND_PLAN_FONT_DIR` 指定字体目录。字体检查覆盖正文、表格、页眉页脚和可打印 ASCII，缺少字形或字体源文件时会报错。
