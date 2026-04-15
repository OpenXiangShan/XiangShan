# Agent Docs

This directory is the durable knowledge base for agents working in this repository. Treat the project root as `$NOOP_HOME`. The root `AGENTS.md` is intentionally short and should point here instead of carrying detailed instructions inline.

Use these documents in order:

1. `docs/agents/frontend-verification.md` for the default workflow in `src/test/python/Frontend/`: file map, test commands, generated-artifact locations, and change hygiene.
2. `docs/agents/frontend-backend-agent.md` for the normative Backend Agent semantics: logical queue meaning, correct-path / wrong-path rules, and the required behavior of `resolve`, `redirect`, `commit`, and `callRetCommit`.
3. `docs/agents/frontend-debugging.md` when debugging DUT or monitor mismatches, especially around `microbench.bin`, FTQ mapping, resolve payloads, or redirect semantics.
4. `docs/agents/frontend-backend-controlflow/README.md` for consolidated notes on backend `resolve`, backend `redirect`, `commit` / `callRetCommit`, and frontend redirect-consumption risks.
5. `docs/testbench/Guide_Doc/` for deeper reference material on fixtures, APIs, coverage, and test authoring templates.

Repository-wide background lives outside this directory:

- `README.md` for top-level XiangShan orientation.
- `docs/testbench/testbench_stages.yaml` for staged testbench guidance and generated workflow templates.
- `docs/superpowers/plans/` and `docs/superpowers/specs/` for recent local design and execution history.
- `docs/superpowers/specs/2026-04-13-frontend-backend-agent-semantic-alignment-plan.md`
  for the concrete implementation plan to align the current env with the
  normative Backend Agent semantics.
- `docs/superpowers/specs/2026-04-10-frontend-env-backend-reconstruction-design.md`
  as historical design background only; if it conflicts with
  `docs/agents/frontend-backend-agent.md`, the latter is the normative
  semantic definition.

This doc set is intentionally small. Add durable facts here when they are repo-specific, stable enough to version, and important enough that an agent should be able to discover them without relying on chat history.
