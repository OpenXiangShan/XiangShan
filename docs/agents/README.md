# Agent Docs

This directory is the durable knowledge base for agents working in this repository. Treat the project root as `$NOOP_HOME`. The root `AGENTS.md` is intentionally short and should point here instead of carrying detailed instructions inline.

Use these documents in order:

1. `docs/agents/frontend-verification.md` for the default workflow in `src/test/python/Frontend/`: file map, test commands, generated-artifact locations, and change hygiene.
2. `docs/agents/frontend-backend-agent.md` for the normative Backend Agent semantics: two-queue model (`cfVec_queue` / `commit_queue`), correct-path / wrong-path rules, and the required behavior of `resolve`, `redirect`, `commit`, and `callRetCommit`.
   Before changing backend-agent semantics or related code, run section `实现一致性最小检查项` in order: `必须项` first, then `建议项`.
3. `docs/agents/frontend-backend-model-review.md` for code-level reading guidance and current risk hotspots in `src/test/python/Frontend/env/backend_model.py`.
4. `docs/agents/frontend-debugging.md` when debugging DUT or monitor mismatches, especially around bin-trace cases, FTQ mapping, resolve payloads, or redirect semantics. Treat this as the stable debugging guide.
5. `docs/agents/frontend-backend-controlflow/README.md` for consolidated notes on backend `resolve`, backend `redirect`, `commit` / `callRetCommit`, and frontend redirect-consumption risks.
6. `docs/testbench/Guide_Doc/` for deeper reference material on fixtures, APIs, coverage, and test authoring templates.

Repository-wide background lives outside this directory:

- `README.md` for top-level XiangShan orientation.
- `docs/testbench/testbench_stages.yaml` for staged testbench guidance and generated workflow templates.

This doc set is intentionally small. Add durable facts here when they are repo-specific, stable enough to version, and important enough that an agent should be able to discover them without relying on chat history.
