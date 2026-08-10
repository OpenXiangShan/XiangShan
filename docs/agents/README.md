# Agent Docs

This directory is the durable knowledge base for agents working in this
repository. Treat the project root as `$NOOP_HOME`.

The root `AGENTS.md` is intentionally short and should point here instead of
carrying detailed instructions inline.

Use these documents in order for frontend verification work:

1. `docs/agents/frontend-verification.md` for the default workflow, file map,
   commands, and change hygiene under `src/test/python/Frontend/`.
2. `docs/agents/frontend-debugging.md` for DUT, monitor, or env mismatch
   analysis.
3. `docs/agents/frontend-uncache-boundaries.md` for instruction uncache
   boundary cases around MMIO/non-cacheable frontend fetch.
4. `docs/agents/frontend-icache-rtl-update-b7e4c56fa-0053df46b.md` for the
   ICache RTL delta and verification impact between the two named frontend
   branch synchronization points.
5. `docs/agents/frontend-mmio-nonmmio-switch-risks.md` for Chisel-level risk
   points when frontend fetch switches between MMIO and non-MMIO regions.
6. `docs/agents/frontend-backend-agent.md` for normative backend-agent
   semantics.
   Before changing backend-agent semantics or related code, run section
   `实现一致性最小检查项` in order: `必须项` first, then `建议项`.
7. `docs/agents/frontend-backend-model-review.md` for code-level reading
   guidance and current hotspots in
   `src/test/python/Frontend/env/backend_model.py`.
8. `docs/agents/frontend-backend-controlflow/README.md` for RTL/control-flow
   background around `resolve`, `redirect`, `commit`, and `callRetCommit`.
9. `docs/testbench/Guide_Doc/` for broader testbench reference on fixtures,
   APIs, coverage, and test authoring.

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
