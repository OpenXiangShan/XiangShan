---
name: mem-ut-session-regress
description: Use only in the XiangShan repository at /nfs/home/lixiangrui/work/memblock_ut/XiangShan when the user asks to compile, simulate, regress, run smoke tests, run eda01 jobs, or continue mem_ut/memblock verification. This skill keeps the current Codex conversation alive in tmux, launches or resumes Codex with full permissions, watches long eda01 compile or simulation tasks, and injects result logs back into the same tmux-hosted Codex session automatically.
---

# mem_ut Session Regress

This skill is only for this repository and only for the memblock UVM environment under:

- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan/mem_ut/ver/ut/memblock`

Use it when a task needs:

- remote `eda01` compile or run
- long-running job monitoring
- automatic result collection
- returning the result back into the current Codex session

## Required session model

The active Codex conversation must live inside a fixed `tmux` session.

Default session name:

- `codex_memblock`

The tmux session is started and prepared by:

- `AI_DOC/skills/mem-ut-session-regress/scripts/start_codex_tmux.sh`

That script opens:

- `/nfs/home/lixiangrui/work/memblock_ut/XiangShan`

and runs:

- `codex resume --last --dangerously-bypass-approvals-and-sandbox -C /nfs/home/lixiangrui/work/memblock_ut/XiangShan`

## Main scripts

- `AI_DOC/skills/mem-ut-session-regress/scripts/auto_regress.sh`
  - compile -> run -> result state machine
- `AI_DOC/skills/mem-ut-session-regress/scripts/triage_result.sh`
  - build a short result summary
- `AI_DOC/skills/mem-ut-session-regress/scripts/notify_current_session.sh`
  - inject result prompt back into the tmux Codex session
- `AI_DOC/skills/mem-ut-session-regress/scripts/resume_from_result.sh`
  - choose tmux injection first, then CLI fallback
- `AI_DOC/skills/mem-ut-session-regress/scripts/auto_resume_regress.sh`
  - end-to-end entrypoint

## Preferred usage

Run from any directory under this repository. The skill scripts use the fixed
`mem_ut` sim path internally.

Recommended command:

```bash
bash /nfs/home/lixiangrui/work/memblock_ut/XiangShan/AI_DOC/skills/mem-ut-session-regress/scripts/auto_resume_regress.sh tc_smoke smoke
```

This should:

1. ensure tmux Codex session exists
2. compile on `eda01`
3. run on `eda01`
4. write `result/<tc>.json`
5. write `result/<tc>.triage.txt`
6. send a follow-up prompt into the tmux Codex session

## Debugging session resume only

To debug only the handoff back to the current Codex session, use:

```bash
bash /nfs/home/lixiangrui/work/memblock_ut/XiangShan/AI_DOC/skills/mem-ut-session-regress/scripts/resume_from_result.sh tc_smoke smoke
```

If tmux session exists, it should use:

```bash
tmux set-buffer -- "..."
tmux paste-buffer -t codex_memblock
tmux send-keys -t codex_memblock Enter
```

If no tmux session exists, it falls back to `codex resume`.

## Important constraints

- This skill is project-specific. Do not use it outside `/nfs/home/lixiangrui/work/memblock_ut/XiangShan`.
- Do not rely on background `codex resume` alone for non-TTY recovery
- Prefer tmux injection for “same current session” behavior
- Avoid compile/run concurrency in the same `mode`
- Treat `result.json` as the source of truth for terminal state
