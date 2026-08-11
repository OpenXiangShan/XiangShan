# Agent Docs

This directory is the durable knowledge base for agents working in this
repository. Treat the project root as `$NOOP_HOME`.

The root `AGENTS.md` is intentionally short and should point here instead of
carrying detailed instructions inline.

For frontend verification, start with
`docs/agents/frontend-verification.md`. It defines the harness contract and
links to the source-tree entrypoint.

Read an additional document only when the task needs it:

- DUT, monitor, or environment mismatch:
  `docs/agents/frontend-debugging.md`.
- Backend-agent semantics or its implementation:
  `docs/agents/frontend-backend-agent.md`, then
  `docs/agents/frontend-backend-model-review.md`.
- `resolve`, `redirect`, `commit`, or `callRetCommit` RTL background:
  `docs/agents/frontend-backend-controlflow/README.md`.
- Instruction-uncache or MMIO/non-MMIO boundaries:
  `docs/agents/frontend-uncache-boundaries.md` and, when applicable,
  `docs/agents/frontend-mmio-nonmmio-switch-risks.md`.
- General testbench APIs and fixture reference: `docs/testbench/Guide_Doc/`.

Before changing backend-agent semantics, run
`frontend-backend-agent.md` section `实现一致性最小检查项` in order: `必须项`,
then `建议项`.

Before compiling Frontend or starting a real DUT regression, establish this
baseline in the same session: read this map, `frontend-verification.md`, the
applicable `docs/testbench/` guidance, and the Frontend README plus its
testplan, testpoint, and functional-coverage documents. Inspect the selected
test script and current DUT manifest before choosing a command. Reuse a
manifest-compatible Verilator DUT; rebuild only when the artifact is missing
or stale, or when the user explicitly requests a rebuild.

Repository-wide background lives outside this directory:

- `README.md` for top-level XiangShan orientation.
- `docs/testbench/testbench_stages.yaml` for staged testbench guidance and
  generated workflow templates.
- Never use `git push -f` under any circumstances.
- Never use `git merge` to incorporate remote code; use `git rebase` only.

Keep this file as a doc map rather than a second copy of detailed workflow or
semantic rules.

This doc set is intentionally small. Add durable facts here when they are
repo-specific, stable enough to version, and important enough that an agent
should be able to discover them without relying on chat history.
