"""The surgery's rounds as a chain of genrules (xs_surgery.py)."""

_PLAN_JSON = "//xstile:plan/plan.json"
_PLAN_BZL = "//xstile:plan/plan.bzl"
_YOSYS = "@yosys//:yosys"
_SLANG = "@sv-elab//src/yosys_plugin:slang.so"

def _proven(name, src):
    """constant_outputs.py on src: the outputs yosys proves constant."""
    native.genrule(
        name = name,
        srcs = [src, _PLAN_JSON, _PLAN_BZL],
        outs = [name + ".json"],
        cmd = " && ".join([
            "PLAN='--plan $(location %s) --plan-bzl $(location %s)'" % (_PLAN_JSON, _PLAN_BZL),
            "$(execpath :constant_outputs) select $$PLAN --out $(@D)/%s.ys" % name,
            "$(execpath %s) -q -m $(execpath %s) " % (_YOSYS, _SLANG) +
            "-p 'read_slang --keep-hierarchy --top XSTile $(location %s); hierarchy -top XSTile; " % src +
            "script $(@D)/%s.ys; opt -fast -purge; write_json -selected $(@D)/%s.kept.json' > /dev/null" % (name, name),
            "$(execpath :constant_outputs) scan $(@D)/%s.kept.json $$PLAN --out $@" % name,
            "rm $(@D)/%s.kept.json $(@D)/%s.ys" % (name, name),
        ]),
        tools = [":constant_outputs", _SLANG, _YOSYS],
    )

def _boundary(name, src):
    """boundary_constants.py on src: the inputs tied at kept boundaries."""
    native.genrule(
        name = name,
        srcs = [src, _PLAN_JSON, _PLAN_BZL],
        outs = [name + ".json"],
        cmd = "$(execpath @bazel-orfs//tools/boundary_constants) $(location %s) --top XSTile " % src +
              "--plan $(location %s) --plan-bzl $(location %s) --out $@" % (_PLAN_JSON, _PLAN_BZL),
        tags = ["manual"],
        tools = ["@bazel-orfs//tools/boundary_constants"],
    )

def xs_surgery_chain(name, src, manifest, rounds, protect):
    """Rounds 1..rounds of manifest applied to src, one genrule each.

    An odd round ties inputs whose constant is in the text; an even round
    ties outputs yosys proves constant on the previous round's result,
    proven again on every build. <name>.sv is the last round's result;
    <name>_boundary_<r> and <name>_proven_<r> are the discovery that
    writes round r+1 into the manifest.

    Args:
      name: the last round's genrule; its output is <name>.sv.
      src: the flat Verilog the first round reads.
      manifest: xs_surgery.py's manifest, every round's entries.
      rounds: how many rounds the manifest has.
      protect: modules whose ports no entry may touch.
    """
    prev = src
    for r in range(1, rounds + 1):
        out = "%s_round%d.sv" % (name, r) if r < rounds else name + ".sv"
        proven = []
        if r % 2 == 0:
            _proven("%s_proven_%d" % (name, r), prev)
            proven = [":%s_proven_%d" % (name, r)]
        native.genrule(
            name = "%s_round%d" % (name, r) if r < rounds else name,
            srcs = [manifest, prev, _PLAN_JSON, _PLAN_BZL] + proven,
            outs = [out],
            cmd = "$(execpath :xs_surgery) apply --manifest $(location %s) --rounds %d-%d " % (manifest, r, r) +
                  "".join(["--protect %s " % p for p in protect]) +
                  ("--proven $(location %s) " % proven[0] if proven else "") +
                  # the entries for modules this plan keeps: a carve that
                  # flattens a module drops its boundary's constants
                  "--plan $(location %s) --plan-bzl $(location %s) " % (_PLAN_JSON, _PLAN_BZL) +
                  "$(location %s) $@" % prev,
            tools = [":xs_surgery"],
            visibility = ["//:__subpackages__"],
        )
        prev = ":" + out

        # Discovery for the next round, on this round's result.
        _boundary("%s_boundary_%d" % (name, r), prev)

def kept_cells(name, src):
    """Cells and flop bits per kept module of src.

    kept_cells.py after the same proc and opt -fast the proof runs.
    Manual: a measurement.

    Args:
      name: the genrule; its output is <name>.json.
      src: the flat Verilog to measure.
    """
    native.genrule(
        name = name,
        srcs = [src, _PLAN_JSON, _PLAN_BZL],
        outs = [name + ".json"],
        cmd = " && ".join([
            "$(execpath :constant_outputs) select --plan $(location %s) --plan-bzl $(location %s) --out $(@D)/%s.ys" % (_PLAN_JSON, _PLAN_BZL, name),
            "$(execpath %s) -q -m $(execpath %s) " % (_YOSYS, _SLANG) +
            "-p 'read_slang --keep-hierarchy --top XSTile $(location %s); hierarchy -top XSTile; " % src +
            "script $(@D)/%s.ys; opt -fast -purge; write_json -selected $(@D)/%s.kept.json' > /dev/null" % (name, name),
            "$(execpath :kept_cells) $(@D)/%s.kept.json --out $@" % name,
            "rm $(@D)/%s.kept.json $(@D)/%s.ys" % (name, name),
        ]),
        tags = ["manual"],
        tools = [":constant_outputs", ":kept_cells", _SLANG, _YOSYS],
    )
