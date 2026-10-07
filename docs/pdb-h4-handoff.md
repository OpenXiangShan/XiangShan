# H4 used capacity victim move

H4 branches from `feat-pdb-h3-stream-bop` (`5649c1416`). It adds used Stream capacity-victim transfer to DCache; H3, H1 and user SPEC versions are retained.

## Behavior

`DefaultConfig`: PDB64/LRU, dynamic initial16, seven existing depth levels, 500 successful Stream PDB refills per window, used move permission ON. Permission does not force moves: the existing used-reuse shadow opens after >=20 hits in three consecutive windows and closes after <=5 hits in two consecutive windows. Used reuse never contributes depth pressure.

Only after a completed refill is waiting for full PDB capacity does the existing replacement choose a victim. A clean, used Stream victim with active permission goes through the existing MainPipe move path. Other victims release through WBQueue. The selected address and direction are held across backpressure and policy/recency updates. Withdrawing capacity pressure or an intervening Probe/Store claim cancels the unaccepted selection. Store correctness transfers remain independent. The old Load-use background move flag remains false.

One capacity move may be outstanding. The PDB retains its data/coherence ownership while MainPipe stalls; successful S3 installation frees it. S2 abort restores resident/poison state without publishing a victim. Clean abort may retry; poison follows the original release path. MainPipe uses the original data, coherence, source and final-used metadata; no new TileLink path is introduced.

A clean capacity release and a completed capacity move both publish one capacity-victim event. Failed moves, reservation cancellation, Probe and Store/background transfers are excluded. All three demand pipes query the existing FIFO regardless of DCache hit/miss or move permission, so the same reuse observations can maintain an enabled policy. Release preserves the existing last-S2-use correction. A completed move captures metadata before slot reuse.

## Experiment switches

Runtime record `enablePDBUsedVictimMove0` gates the used shadow for hart0. `enablePDBAutoDepth0=false` and `pdbFixedDepth0=64` select fixed64 while retaining observations. `enablePDBMoveToDCache0` is a different legacy background policy and must remain false. H4 has no unused physical policy. Trace/depth parameters and other prefetch configuration follow H3.

The user's runner uses `DefaultConfig`. A Constantin override only applies when actually forwarded via `--cst-file`; for a runner without this forwarding, rebuild after changing the explicit `PDBDepthParameters` default. Do not infer applied settings from a file merely copied to the results directory.

## Validation and delivery

Directed regressions pass: 27 distinct cases (21 initial; the affected capacity7 rerun plus real MainPipe4 and monitor2). Real MainPipe validation found and fixed a valid/accepted-claim combinational loop; corrected 11-case regression passes. Fresh `make verilog CONFIG=DefaultConfig JVM_XMX=40G` passes; generated PDB/monitor evidence and source hashes are retained in `tmp/h4-delivery/validation.json`. Commit/push is the final delivery step. No SPEC run is authorized. Passing functional tests does not establish performance benefit or timing closure. H3 versus H1 user results and H4 versus H3 user results remain the performance decision boundary.
