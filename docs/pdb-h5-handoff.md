# H5 unused capacity victim move

H5 branches from H4 `a4d4b6515` and retains the H3 `5649c1416` ancestor. H4 remains an independently runnable used-only comparison.

## Policy and pipeline

`DefaultConfig`: PDB64/LRU, dynamic initial16, seven depth levels and 500 successful Stream PDB refills per window. Both used/unused permissions default ON; each gates its existing shadow state independently. The legacy Load background promotion stays OFF.

Used shadow: >=20 reuse hits in three consecutive windows opens; <=5 in two consecutive windows closes. Unused shadow opens only following an entire depth4 window with nonzero raw unused pressure >= late pressure; eight consecutive raw-zero unused-pressure windows close it. Shadow and FIFO observation continue with physical permissions disabled. No epoch, observation-age timer or window clearing is introduced.

H5 chooses the victim's move class at capacity selection: clean Stream used/unused victims follow their respective active permission; all others release. Address and direction remain stable while waiting. The H4 one-inflight move/abort/ownership path is reused. Successful capacity moves publish one victim with final used state, even if a Load authorized on the move edge turns an unused candidate into used. Aborted moves do not publish. Observer demand queries include DCache hits, so moved victims remain visible to maintenance feedback.

While physical unused move is active, raw unused pressure belongs to move and depth uses zero unused pressure. Raw pressure still controls unused shadow activation/closure and is recorded separately. Late pressure continues competing for depth. Only `4 -> 8` has an elevated credit threshold1.5 (three integer half units); other depths retain1.0 and the original settling window. Shadow-only operation retains raw depth competition and the original credit. These are the existing planned/Gem5 mode2 couplings, not newly tuned parameters. Used move does not change depth pressure.

## User experiment configuration

| Comparison | enablePDBUsedVictimMove0 | enablePDBUnusedVictimMove0 |
|---|---|---|
| R3 same-tree only release | false | false |
| R4 used only | true | false |
| R5 unused only | false | true |
| R6 both (H5 default) | true | true |

These are permissions, not forced policy states. Keep `enablePDBMoveToDCache0=false`. Keep `enablePDBAutoDepth0=true` for the four dynamic comparisons; optional same-tree fixed64 diagnosis also sets itfalse and pdbFixedDepth0=64 while disabling both move permissions.

The performance runner uses DefaultConfig. Overrides require actual `--cst-file` forwarding; merely copying a Constantin file does not apply it. When the wrapper does not forward overrides, rebuild after changing only usedMoveEnabled/unusedMoveEnabled in explicit DefaultConfig PDBDepthParameters. Record the resulting commit/dirty diff and actual initialized records. Do not reuse H4's build directory as an H5 emu.

Trace PDBDepthWindow adds raw-versus-depth unused pressure, actual unused-active state and the applied up-threshold. Existing shadow fields show next-window state. The actual-active field describes the closing window. Counters capacity_used_move/capacity_unused_move count final completed class, capacity_move_request/abort track protocol actions; evict remains WBQueue releases. Store correctness moves are separate from capacity policy counts.

## Validation / next boundary

Thirty directed regression cases pass, including all four permission combinations, last-S2-use classification, actual used/unused monitor hysteresis, pressure ownership/min-depth credit, real MainPipe abort/dirty writeback/data/coherence/source checks, and H3 depth/Stream compatibility. Fresh `make verilog CONFIG=DefaultConfig JVM_XMX=40G` passes. Source hashes, test/generation logs and generated PB/monitor evidence are recorded in `tmp/h5-delivery/validation.json`. No SPEC/performance task is started. After successful make verilog and commit/push, review H4/H5 and select comparisons on M/P or A before adopting move. The user's H3/H1 results and differential/difftest/PPA/performance checks remain required evidence; functional generation alone is not a benefit claim.
