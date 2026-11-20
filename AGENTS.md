# Local coordination instructions

## Standing user authorization: rebuild after design updates

The user confirmed on 2026-10-23 that coord must carry out the complete DUT
refresh workflow whenever remote `origin/frontend-bt` design/build inputs change.
When continuing coordination or updating this repository, check the remote and
perform the rebuild and qualification without waiting for another user reminder.
This is standing authorization, not a background polling service.

Follow [the DUT build workflow](src/test/python/Frontend/scripts/DUT_BUILD_WORKFLOW.md).
Use an isolated clean checkout and a frozen SHA; protect dirty owner worktrees.
Run `git submodule sync --recursive` and `make init`, checking recorded gitlinks.
Export `NOOP_HOME` to that checkout. Use `make -j32` and
`FRONTEND_BUILD_JOBS=32`, adjusting only for actual host resource constraints.
Use the mcpgateway virtualenv; do not source the legacy unitychip activation.
Require loader-valid provenance, interface/unit/import gates, a real DUT smoke,
and a full ready-to-run binary trace replay before declaring the refresh complete.
Coord owns the serialized build/DUT slot and versioned artifacts. Preserve old
outputs; never relabel their source SHA. Do not promote smoke/replay to canonical
coverage HIT. Canonical coverage belongs to ctrl-goal; random tooling to audit.
Verification branch synchronization uses rebase; never force-push or introduce a
verification merge. Preserve design merges already present in remote history.

## Repository publication and local synchronization

User confirmed on 2026-10-29: `frontend-bt-jiabowen` is the sole external
remote fetch/publication repository. `frontend-bt-random` follows the local main
repository; its origin points to that local path and its push URL is disabled.
Do not fetch or push the external GitHub remote from random. After accepted main
updates and before a new verification batch, coord synchronizes random against
main using rebase, preserving all dirty/untracked files and owner local commits.
Keep source trees frozen during active DUT/build runs. Audit delivers local
patches/commits for coord integration; validated changes are published from main
only under the existing push authorization. This is event-driven coordination,
not a background synchronization daemon.

## Task scratch and preservation placement

User explicitly prohibited new task material under system `/tmp`, `/root`,
`/var/tmp`, or other system-root locations on 2026-10-29. Place every agent-created
scratch script, report, log, test temporary root and preservation archive inside
the two permanent repositories, using ignored `.cache/` or `build-frontend/`
directories. Set TMPDIR and test temporary directories explicitly for commands
we control. Do not create another checkout or a task-specific directory under
system paths. Existing legacy paths there are read/cleanup targets only.
Runtime sockets/mounts managed internally by the execution platform are not
task output destinations and must not be removed while in use.
