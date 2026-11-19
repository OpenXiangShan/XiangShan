# Translation exploration model

`translation_exploration_plan.py` constructs Python model scenarios for stage 1,
stage 2, and combined translation. Plans cover PTW delay, context IDs, PBMT/PMA,
and fault inputs. Each plan reports model-only results and leaves runtime
reachability unconfirmed. The model does not start a DUT or accept a replay as
coverage evidence.

Model tests keep request identity when a pending PTW response is explicitly
preserved across a fence. They do not show that the RTL PTW filter or TLB
consumes the response. Fault/recovery behavior needs a separate reviewed
continuation and checker before use with a golden trace.
