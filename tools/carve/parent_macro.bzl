"""The parent as a macro: the parent and its blocks built side by side from one plan, checked flat by a top level that dissolves them."""

load("@bazel-orfs//:openroad.bzl", "orfs_flow", "orfs_run")
load("@bazel-orfs//tools/carve:memories.bzl", "carve_memories")
load("@rules_shell//shell:sh_test.bzl", "sh_test")

# the settings of the Top's place step, unfirm_user_arguments (top_level.tcl)
_UNFIRM_SETTINGS = ["TOP_UNFIRM_DEEP", "TOP_UNFIRM_SIDE", "TOP_CLOCK_PERIOD"]

def parent_macro_flow(
        name,
        top,
        plan,
        plan_pins,
        place_macros,
        blocks,
        verilog_files,
        arguments,
        sources,
        user_arguments = {},
        user_sources = {},
        user_stages = {},
        stage_arguments = {},
        top_arguments = {},
        top_sources = {},
        macros = [],
        clock_port = "clock",
        block_clocks = ["clock"],
        budget_ps = "2000",
        max_skew_ps = "20",
        report_budget_ps = None,
        plan_budget = "1500000",
        top_level_arguments = {},
        top_level_user_arguments = {},
        whole_check = False,
        headroom_sets = "",
        headroom_skip = "",
        unfirm_user_arguments = {},
        crossing_data_min_ps = "1500",
        variant = None,
        pdk = "//flow:asap7",
        tags = ["manual"]):
    """The parent as a macro, its blocks, and the flat top level that checks the carve.

    The plan in git is all the parent and the blocks share: the blocks'
    outlines and places and every block pin (plan_floorplan.py's
    <block>_pins.tcl, clock pins included). The blocks build from their
    pin files; the parent, <name>_<stage>, builds from the same files and
    nothing of a block's, so all of them build side by side to global
    route, each its own fast signal, and only the top level,
    <name>_pm_<stage>, waits for all of them.

    The parent is the design's own RTL with the blocks blackboxes,
    cut out in synthesis (evert.tcl: each block pin a port
    u_<block>__<pin>, the instance gone), on the plan's die with a hole
    per block (parent_holes.tcl). <name>_abstract is its LEF with
    exact shapes, <name>_timing_model its timing model.

    The top level is the parent and the blocks, instances only
    (top_netlist.tcl), each placed by the plan and dissolved into the
    cells, nets and route guides of its own global route, its clock tree
    left behind (top_dissolve.tcl): the cells stay fixed where their
    macro put them. top_level.tcl builds it in two passes. The first,
    <name>_pm_firm_top, builds one clock tree over every register and
    routes the stitches and the clock, every cell where its macro put it.
    The second starts again from the floorplan: <name>_pm_top_place
    unfirms the cells on the first pass's crossing paths over the
    report's budget (unfirm.tcl: made movable, their nets' routes
    dropped), places them by global placement with timing and
    routability driving among the fixed rest and repairs them;
    <name>_pm_top builds the clock tree and routes, as the first pass.

    The crossing report: every reg2reg path not contained in one macro
    that needs more than budget_ps, each cell on it by name, timed in
    sets of macros that fit memory. <name>_pm_crossing_plan finds the
    sets (crossing_plan.tcl), <name>_pm_crossing_sets cuts each to its
    crossing cones and writes it with its parasitics as SPEF,
    <name>_pm_crossing_time times each in a process of its own, and
    <name>_pm_crossings merges them: crossing.txt, tables.md and
    histogram.svg. These are the second pass's; the first pass's are
    <name>_pm_firm_crossing_plan, _sets, _time and <name>_pm_firm_crossings,
    under <name>_pm/firm. <name>_pm_crossing_ends writes the end pins of
    the first pass's crossings over the budget from its crossing data
    (crossing_ends.py), the second pass's input, and
    <name>_pm_crossing_verdict compares the two reports
    (crossing_verdict.py). The second pass needs a crossing over the
    budget in the first: with none there is nothing to unfirm, and
    <name>_pm_crossing_ends stops, and every target of the second pass
    with it.

    Run by hand: <name>_pm_unfirm, the second pass's selection on the
    floorplan, its counts and its stops' controls (unfirm_check.tcl), and
    <name>_pm_crossing_floor, how much of each crossing of the second
    pass is wire (crossing_floor.tcl), with its control,
    <name>_pm_crossing_floor_control.

    Args:
      name: the parent macro's flow; the top level is <name>_pm.
      top: the design's top module, the parent macro's module.
      plan: the PLAN dict plan_floorplan.py wrote.
      plan_pins: {block: its plan pins file}.
      place_macros: the plan's place_macros.tcl: the parent's own
        macros and placed netlists where the plan put them.
      blocks: {block: {"abstract": its abstract target, "grt": its
        global route target}}.
      verilog_files: the design's RTL, the blocks' included.
      arguments: the parent macro's arguments.
      sources: the parent macro's sources.
      user_arguments: the parent macro's user arguments; a
        SYNTH_POST_HIERARCHY_SCRIPTS among them runs before evert.ys.
      user_sources: the parent macro's user sources.
      user_stages: the parent macro's user stages.
      stage_arguments: the parent macro's per-stage arguments, over
        arguments (orfs_flow's stage_arguments).
      top_arguments: the top level's arguments, over the defaults.
      top_sources: the top level's sources, over the defaults
        (SDC_FILE at least).
      macros: the parent's own macros.
      clock_port: the design's clock port.
      block_clocks: the blocks' clock pins, by name.
      budget_ps: the longest reg2reg path, ps, that may cross a macro
        boundary.
      max_skew_ps: the top level's clock skew it stops over, ps.
      report_budget_ps: the crossing report's budget, ps; budget_ps by
        default.
      plan_budget: instances a crossing set may hold, the measure of
        its memory.
      top_level_arguments: ORFS variables for top_level.tcl's runs
        (SKIP_PIN_ACCESS).
      top_level_user_arguments: top_level.tcl's own settings
        (TOP_DPL_ARGS, TOP_GRT_ITERATIONS, TOP_PARASITICS).
      crossing_data_min_ps: the crossing timing also writes every crossing
        path needing more than this, under the budget too, with its wire
        delay and both ends' places (crossing_data.<set>.tsv beside the
        report), for the carve's offline model and for the second pass,
        which takes its ends from the first pass's; below the report's
        budget, or the flow is refused.
      whole_check: the checks for a design small enough to time whole:
        the top level timed whole, <name>_pm_crossing_whole, reports the
        paths the sets do (<name>_pm_crossing_test); the launch and
        headroom checks; the second pass's selection and stops
        (<name>_pm_unfirm_test), its verdict against the first
        (<name>_pm_crossing_verdict_test), the constraints both hand on
        (<name>_pm_top_sdc_test) and the crossing floor
        (<name>_pm_crossing_floor_test).
      headroom_sets: the crossing sets <name>_pm_crossing_headroom
        measures, their numbers separated by spaces; empty: every set.
      headroom_skip: the measurements it skips, of: wire vt skew cycles.
      unfirm_user_arguments: top_level.tcl's settings for the second
        pass's place step, <name>_pm_top_place, and <name>_pm_unfirm:
        TOP_UNFIRM_DEEP, the registers deep past the paths' own the
        selection takes (0, the paths only); TOP_UNFIRM_SIDE, the side
        that depth is taken on, both (default), back or fwd;
        TOP_CLOCK_PERIOD, the period, ps, the place step is timed at, the
        SDC's by default. Any other key is refused.
      variant: the parent macro's variant, when another flow of the
        package has the same top module; its targets are then
        <name>_<variant>_<stage>, and <name>_<variant> stands for <name>
        above.
      pdk: the PDK.
      tags: forwarded.
    """

    # the second pass's ends are the first pass's crossing data over the
    # budget, which crossing_paths.tcl writes only when its threshold is
    # below the budget
    report_budget = report_budget_ps or budget_ps
    if not crossing_data_min_ps or float(crossing_data_min_ps) >= float(report_budget):
        fail(("parent_macro_flow %s: crossing_data_min_ps is '%s', not below the " +
              "crossing report's budget, %s ps: the Top's second pass takes its " +
              "ends from the first pass's crossing data, which crossing_paths.tcl " +
              "writes only when this threshold is below the budget") %
             (name, crossing_data_min_ps, report_budget))

    # unfirm_user_arguments takes only TOP_UNFIRM_DEEP, TOP_UNFIRM_SIDE and
    # TOP_CLOCK_PERIOD
    unread = [k for k in unfirm_user_arguments if k not in _UNFIRM_SETTINGS]
    if unread:
        fail("parent_macro_flow %s: unfirm_user_arguments takes only %s and %s, not %s" %
             (name, ", ".join(_UNFIRM_SETTINGS[:-1]), _UNFIRM_SETTINGS[-1], ", ".join(sorted(unread))))
    base = name + ("_" + variant if variant else "")
    names = sorted(blocks.keys())
    clocks = " ".join(block_clocks)
    holes = []
    for b in names:
        p = plan["macros"][b]
        _, _, x1, y1 = p["DIE_AREA"].split(" ")
        holes.append("%s %s %s %s %s" % (b, p["x_um"], p["y_um"], x1, y1))
    pins = [plan_pins[b] for b in names]

    parent_sources = dict(sources)
    parent_sources["IO_CONSTRAINTS"] = ["@bazel-orfs//tools/carve:parent_holes.tcl"]
    parent_sources["MACRO_PLACEMENT_TCL"] = [place_macros]

    # evert.ys runs evert.tcl by its path in the workspace, after the
    # design's own hook scripts
    hooks = user_arguments.get("SYNTH_POST_HIERARCHY_SCRIPTS", "")
    parent_user_arguments = user_arguments | {
        "CARVE_BLOCKS": " ".join(names),
        "CARVE_CLOCKS": clocks,
        "CARVE_TOP": top,
        "HOLES": " ".join(holes),
        "PLAN_BLOCKS_ELSEWHERE": "1",
        "SYNTH_POST_HIERARCHY_SCRIPTS": (hooks + " tools/carve/evert.ys").strip(),
    }
    parent_user_sources = user_sources | {
        "CARVE_EVERT": [
            "@bazel-orfs//tools/carve:evert.tcl",
            "@bazel-orfs//tools/carve:evert.ys",
        ],
        "PLAN_PINS": pins,
    }

    # the plan reaches the floorplan only: a new plan does not
    # synthesize the parent again
    floorplan = ["floorplan"]
    parent_user_stages = user_stages | {
        "CARVE_BLOCKS": ["synth", "floorplan"],
        "CARVE_CLOCKS": ["synth"],
        "CARVE_EVERT": ["synth"],
        "CARVE_TOP": ["synth"],
        "HOLES": floorplan,
        "PLAN_BLOCKS_ELSEWHERE": floorplan,
        "PLAN_PINS": floorplan,
        "SYNTH_POST_HIERARCHY_SCRIPTS": ["synth"],
    }
    orfs_flow(
        name = name,
        top = top,
        arguments = {
            # by default the first flush through grt, for turnaround:
            # XSTile_parent's CTS setup repair ground for over an hour at
            # WNS -437 ps without moving it, on the 4771 x 2064 um die. A
            # design's arguments or stage_arguments turn repair back on.
            "SKIP_CTS_REPAIR_TIMING": "1",
            "SKIP_INCREMENTAL_REPAIR": "1",
        } | arguments | {
            "CORE_AREA": plan["parent"]["CORE_AREA"],
            "DIE_AREA": plan["parent"]["DIE_AREA"],
            "OPENROAD_HIERARCHICAL": "0",
            "SYNTH_BLACKBOXES": " ".join(names),
        },
        last_stage = "grt",
        macros = macros,
        pdk = pdk,
        sources = parent_sources,
        stage_arguments = stage_arguments,
        variant = variant,
        tags = tags,
        user_arguments = parent_user_arguments,
        user_sources = parent_user_sources,
        user_stages = parent_user_stages,
        verilog_files = verilog_files,
    )

    # the LEF and the timing model apart: the top level reads only the
    # LEF, the parent being dissolved at its floorplan, and XSTile's
    # parent's timing model was an hour on one core
    orfs_run(
        name = "%s_abstract" % base,
        src = ":%s_grt" % base,
        outs = ["%s/%s.lef" % (base, top)],
        cmd = "run",
        script = "@bazel-orfs//test/abutment:abstract_exact.tcl",
        tags = tags,
        user_arguments = {"LEF_OUT": "$(location %s/%s.lef)" % (base, top)},
    )
    orfs_run(
        name = "%s_timing_model" % base,
        src = ":%s_grt" % base,
        # named as orfs_headroom drops a block's own model
        outs = ["%s/%s_typ.lib" % (base, top)],
        cmd = "run",
        script = "@bazel-orfs//test/abutment:abstract_exact.tcl",
        tags = tags,
        user_arguments = {"LIB_OUT": "$(location %s/%s_typ.lib)" % (base, top)},
    )

    # the plan's pin partner dump, from the parent's synthesis alone
    orfs_run(
        name = "%s_pin_partners" % base,
        src = ":%s_synth" % base,
        outs = ["%s/pin_partners.txt" % base],
        script = "@bazel-orfs//tools/carve:parent_pin_partners.tcl",
        tags = tags,
        user_arguments = {
            "CARVE_BLOCKS": " ".join(names),
            "PROBE_OUT": "$(location %s/pin_partners.txt)" % base,
        },
    )

    # the memories inside every macro the top level dissolves: the
    # masters its dissolve copies cells of (ODB-1221), and their timing
    carve_memories(
        name = "%s_memories" % base,
        srcs = [":%s_grt" % base] + [blocks[b]["grt"] for b in names],
        lef = "%s/memories.lef" % base,
        lib = "%s/memories.lib" % base,
        tags = tags,
    )

    pm = "%s_pm" % base
    orfs_run(
        name = "%s_top_netlist" % base,
        src = ":%s_synth" % base,
        outs = ["%s/%s.v" % (base, pm)],
        script = "@bazel-orfs//tools/carve:top_netlist.tcl",
        tags = tags,
        user_arguments = {
            "CARVE_BLOCKS": " ".join(names),
            "CARVE_CLOCKS": clocks,
            "CLOCK_PORT": clock_port,
            "TOP_NAME": pm,
            "TOP_V": "$(location %s/%s.v)" % (base, pm),
        },
        user_sources = {"PLAN_PINS": pins},
    )

    # the top level, flat: every macro dissolved at its own global route
    odbs = []
    for b in [top] + names:
        src = ":%s_grt" % base if b == top else blocks[b]["grt"]
        native.filegroup(
            name = "%s_%s_grt_odb" % (pm, b),
            srcs = [src],
            output_group = "5_1_grt.odb",
            tags = tags,
        )
        odbs.append(":%s_%s_grt_odb" % (pm, b))
    orfs_flow(
        name = pm,
        arguments = {
            # the parent is a macro the size of the die until it is
            # dissolved, and the floorplan refuses a macro larger than the
            # core (IFP-0002): the core is the die, and top_dissolve.tcl
            # makes the rows on the parent's core once it is gone
            "CORE_AREA": plan["parent"]["DIE_AREA"],
            "DIE_AREA": plan["parent"]["DIE_AREA"],
            "DONT_BUFFER_PORTS": "1",
            "MACRO_ROWS_HALO_X": "0",
            "MACRO_ROWS_HALO_Y": "0",
            "SYNTH_HIERARCHICAL": "0",
        } | top_arguments,
        # the floorplan dissolves the macros; the clock tree and global
        # route are top_level.tcl's (<pm>_top below): ORFS's place, CTS
        # and global route optimise cells, and every cell here is fixed
        last_stage = "floorplan",
        macros = [blocks[b]["abstract"] for b in names] + macros,
        pdk = pdk,
        sources = {
            "ADDITIONAL_LEFS": [":%s/%s.lef" % (base, top), ":%s/memories.lef" % base],
            "ADDITIONAL_LIBS": [":%s/memories.lib" % base],
            "IO_CONSTRAINTS": ["@bazel-orfs//test/abutment:io_top.tcl"],
            "MACRO_PLACEMENT_TCL": ["@bazel-orfs//tools/carve:top_dissolve.tcl"],
            "SYNTH_NETLIST_FILES": [":%s/%s.v" % (base, pm)],
            # the macros brought their own tap cells
            "TAPCELL_TCL": ["@bazel-orfs//tools/carve:no_tapcells.tcl"],
        } | top_sources,
        tags = tags,
        user_arguments = {
            "DISSOLVE_MASTERS": " ".join([top] + names),
            "HOLES": " ".join(holes),
            "PARENT_CORE_AREA": plan["parent"]["CORE_AREA"],
            "PARENT_INST": "u_parent",
        },
        user_sources = {
            "DISSOLVE_ODBS": odbs,
            "PLAN_PINS": pins,
        },
        verilog_files = [],
    )

    _top_level(
        pm,
        budget_ps = report_budget,
        max_skew_ps = max_skew_ps,
        plan_budget = plan_budget,
        top_level_arguments = top_level_arguments,
        top_level_user_arguments = top_level_user_arguments,
        whole_check = whole_check,
        headroom_sets = headroom_sets,
        headroom_skip = headroom_skip,
        unfirm_user_arguments = unfirm_user_arguments,
        crossing_data_min_ps = crossing_data_min_ps,
        tags = tags,
    )

def _top_level(pm, budget_ps, max_skew_ps, plan_budget, top_level_arguments, top_level_user_arguments, whole_check, headroom_sets, headroom_skip, unfirm_user_arguments, crossing_data_min_ps, tags):
    """The top level's two passes, each to global route with its crossing report.

    Args:
      pm: the top level's flow, at its floorplan.
      budget_ps: the crossing report's budget, ps.
      max_skew_ps: the clock check's limit, ps.
      plan_budget: instances a crossing set may hold.
      top_level_arguments: ORFS variables for top_level.tcl's runs.
      top_level_user_arguments: top_level.tcl's own settings.
      whole_check: time the top level whole too and test the sets
        against it, and test the second pass.
      headroom_sets: the crossing sets the headroom measures.
      headroom_skip: the measurements the headroom skips.
      unfirm_user_arguments: top_level.tcl's settings for the second
        pass's place step and the unfirm check.
      crossing_data_min_ps: the crossing timing's dataset threshold, ps.
      tags: forwarded.
    """
    scripts = {
        "CLOCK_CHECK": ["@bazel-orfs//tools/carve:top_clock_check.tcl"],
        "CROSSING_CONES": ["@bazel-orfs//tools/carve:crossing_cones.tcl"],
        "CROSSING_PATHS": ["@bazel-orfs//tools/carve:crossing_paths.tcl"],
        "CROSSING_TIME": ["@bazel-orfs//tools/carve:crossing_time.tcl"],
    }
    top_arguments = {
        # one tree over every register of every macro: on the toy
        # (test/parent_macro) clusters of 10 sinks took the setup skew
        # from 37.4 to 17.9 ps, the default's 30 left it at 37.4
        "CTS_CLUSTER_SIZE": "10",
        # the macros' routes kept, the clock tree and the stitches routed
        # (patch 0173)
        "GLOBAL_ROUTE_ARGS": "-allow_congestion -verbose -unguided_only",
    } | top_level_arguments
    unfirm_tcl = {"UNFIRM_TCL": ["//tools/carve:unfirm.tcl"]}
    ends = ":%s/ends.tsv" % pm

    # The Top's runs are on this machine, never on a remote executor: on
    # XSTile the first pass's Top peaks at 77 GB, and the place step at
    # 74 GB for two hours.
    local = tags + ["no-remote-exec"]

    # the first pass, floorplan to global route, every cell where its
    # macro put it: the routed top level, 5_top_grt.odb
    firm_top = ":%s_firm_top" % pm
    orfs_run(
        name = "%s_firm_top" % pm,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/firm/top" % pm,
        cmd = "run",
        script = "@bazel-orfs//tools/carve:top_level.tcl",
        arguments = top_arguments,
        tags = local,
        user_arguments = top_level_user_arguments | {"MAX_SKEW_PS": max_skew_ps},
        user_sources = scripts,
    )
    firm = _crossing_report(
        "%s_firm" % pm,
        "%s/firm" % pm,
        pm = pm,
        top = firm_top,
        budget_ps = budget_ps,
        plan_budget = plan_budget,
        crossing_data_min_ps = crossing_data_min_ps,
        top_arguments = top_arguments,
        top_level_user_arguments = top_level_user_arguments,
        scripts = scripts,
        tags = tags,
    )

    # the end pins of the first pass's crossing paths over the budget,
    # each in the group of its worst path, and their start pins: what the
    # second pass unfirms; crossing_ends.py stops when there is none
    native.genrule(
        name = "%s_crossing_ends" % pm,
        srcs = [firm.times],
        outs = ["%s/ends.tsv" % pm, "%s/starts.txt" % pm],
        cmd = "$(location @bazel-orfs//tools/carve:crossing_ends) $(location %s) %s $$(dirname $(location %s))" % (firm.times, budget_ps, ends),
        tools = ["@bazel-orfs//tools/carve:crossing_ends"],
        tags = tags,
    )

    # the second pass's place step, from the floorplan: the cells on the
    # paths into those ends unfirmed, placed among the fixed rest and
    # repaired (unfirm.tcl), 3_top_place.odb. An action of its own, given
    # only the settings that step reads, so that a change to the tree's or
    # the route's settings leaves the placement cached
    place = ":%s_top_place" % pm
    orfs_run(
        name = "%s_top_place" % pm,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/place" % pm,
        cmd = "run",
        data = [ends],
        script = "@bazel-orfs//tools/carve:top_level.tcl",
        arguments = {
            # timing-driven global placement keeps the buffers of its
            # repair only below this overflow; at 0 they are made on the
            # final placement. XiangShan's parent sets the same, for the
            # same reason (xiangshan.bzl, XS_PARENT)
            "GPL_KEEP_OVERFLOW": "0",
        },
        tags = local,
        user_arguments = {
            "TOP_TO": "place",
            "TOP_UNFIRM": "$(location %s)" % ends,
        } | unfirm_user_arguments,
        user_sources = unfirm_tcl,
    )

    # the second pass, from the place step to global route: the tree over
    # every register, the legaliser seating the placed cells with the
    # clock buffers, and global route, which stops on a net of theirs it
    # left without a route (unfirm.tcl): the routed top level,
    # 5_top_grt.odb
    top = ":%s_top" % pm
    orfs_run(
        name = "%s_top" % pm,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/top" % pm,
        cmd = "run",
        data = [place, ends],
        script = "//tools/carve:top_level_fast_dpl.tcl",
        arguments = top_arguments,
        tags = local,
        user_arguments = top_level_user_arguments | {
            "MAX_SKEW_PS": max_skew_ps,
            "TOP_DIR": "$(location %s)" % place,
            "TOP_DPL_ARGS": "-max_displacement {150 150}",
            "TOP_FROM": "place",
            "TOP_UNFIRM": "$(location %s)" % ends,
        },
        user_sources = scripts | unfirm_tcl,
    )
    final = _crossing_report(
        pm,
        pm,
        pm = pm,
        top = top,
        budget_ps = budget_ps,
        plan_budget = plan_budget,
        crossing_data_min_ps = crossing_data_min_ps,
        top_arguments = top_arguments,
        top_level_user_arguments = top_level_user_arguments,
        scripts = scripts,
        tags = tags,
    )
    sets = final.sets
    times = final.times

    # how much earlier each flop launching a crossing over the budget
    # could be clocked, and the hold padding that would take
    # (crossing_launch.tcl): plain timing, run by hand
    orfs_run(
        name = "%s_crossing_launch" % pm,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/launch" % pm,
        cmd = "run",
        data = [sets, ":%s/sets.txt" % pm],
        script = "@bazel-orfs//tools/carve:crossing_time_sets.tcl",
        tags = ["manual"],
        user_arguments = {
            "BUDGET_PS": budget_ps,
            "CROSSING_SETS": "$(location :%s/sets.txt)" % pm,
            "HOLD_TO": "600",
            "SETS_DIR": "$(location %s)" % sets,
        },
        user_sources = scripts | {
            "CROSSING_TIME": ["@bazel-orfs//tools/carve:crossing_launch.tcl"],
        },
    )

    # how much of each set's crossing period the Top could still win, and
    # in which move (crossing_headroom.tcl), each set in an openroad of its
    # own with every thread: run by hand, not on the report's path
    orfs_run(
        name = "%s_crossing_headroom" % pm,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/headroom" % pm,
        cmd = "run",
        data = [sets, ":%s/sets.txt" % pm],
        script = "@bazel-orfs//tools/carve:crossing_headroom_sets.tcl",
        tags = ["manual"],
        user_arguments = {
            "BUDGET_PS": budget_ps,
            "CROSSING_SETS": "$(location :%s/sets.txt)" % pm,
            # skew and VT move the flops of the 200 worst crossing
            # endpoints: skew's hold pass times again after every flop it
            # moves, and every flop of an XSTile set did not end in hours
            "HR_ENDS": "200",
            # skew first, the move a build would act on; the floors after
            "HR_ORDER": "skew wire vt cycles",
            "HR_SETS": headroom_sets,
            # a skew round is a full timing update of the set: an XSTile
            # set's rounds stop after an hour with the best so far
            "HR_SKEW_SECONDS": "3600",
            "HR_SKIP": headroom_skip,
            "SETS_DIR": "$(location %s)" % sets,
        },
        user_sources = scripts | {
            "CROSSING_TIME": ["@bazel-orfs//tools/carve:crossing_headroom.tcl"],
            "HR_TCL": ["@bazel-orfs//tools/odb_debug:headroom.tcl"],
        },
    )

    # how much of each crossing is wire: each set timed as placed and with
    # its signal wires' resistance and capacitance near zero, the clock's
    # as placed in both (crossing_floor.tcl), an estimate on placement
    # parasitics, not the report's numbers; the control leaves the signal
    # wires as placed in both readings: run by hand, on this machine as the
    # sets' timing is (_crossing_report)
    floor = ":%s_crossing_floor" % pm
    floor_control = ":%s_crossing_floor_control" % pm
    for out, control in [("floor", {}), ("floor_control", {"FLOOR_CONTROL": "1"})]:
        orfs_run(
            name = "%s_crossing_%s" % (pm, out),
            src = ":%s_floorplan" % pm,
            out_dir = "%s/%s" % (pm, out),
            cmd = "run",
            data = [sets, ":%s/sets.txt" % pm],
            script = "@bazel-orfs//tools/carve:crossing_time_sets.tcl",
            tags = ["manual", "no-remote-exec"],
            user_arguments = {
                "BUDGET_PS": budget_ps,
                "CROSSING_DATA_MIN_PS": crossing_data_min_ps,
                "CROSSING_SETS": "$(location :%s/sets.txt)" % pm,
                "SETS_DIR": "$(location %s)" % sets,
            } | control,
            user_sources = scripts | {
                "CROSSING_TIME": ["@bazel-orfs//tools/carve:crossing_floor.tcl"],
            },
        )

    # the second pass's selection on the floorplan, nothing placed: its
    # counts, the unfirm rules, and each stop's control raising its own
    # message (unfirm_check.tcl): run by hand, on this machine as the
    # Top's runs are
    orfs_run(
        name = "%s_unfirm" % pm,
        src = ":%s_floorplan" % pm,
        outs = ["%s/unfirm.txt" % pm],
        cmd = "run",
        data = [ends],
        script = "@bazel-orfs//tools/carve:unfirm_check.tcl",
        tags = ["manual", "no-remote-exec"],
        user_arguments = {
            "TOP_UNFIRM": "$(location %s)" % ends,
            "UNFIRM_TXT": "$(location %s/unfirm.txt)" % pm,
        } | unfirm_user_arguments,
        user_sources = unfirm_tcl,
    )

    # the second pass against the first (crossing_verdict.py): pairs over
    # the budget and the worst crossing of each, by group, whether both
    # were timed in the same sets, and the paths inside one macro
    verdict = "@bazel-orfs//tools/carve:crossing_verdict"
    native.genrule(
        name = "%s_crossing_verdict" % pm,
        srcs = [firm.sets_txt, final.sets_txt, firm.times, final.times, firm.crossing_txt, final.crossing_txt],
        outs = ["%s/verdict.txt" % pm],
        cmd = "$(location %s) --sets $(location %s) $(location %s) --time $(location %s) $(location %s) %s $(location %s) $(location %s) > $@ && cat $@" % (
            verdict,
            firm.sets_txt,
            final.sets_txt,
            firm.times,
            final.times,
            budget_ps,
            firm.crossing_txt,
            final.crossing_txt,
        ),
        tools = [verdict],
        tags = ["manual"],
    )

    if not whole_check:
        return

    # the method's check, on a design small enough to time whole: the
    # top level timed in one process reports the paths the sets do
    orfs_run(
        name = "%s_crossing_whole" % pm,
        src = ":%s_floorplan" % pm,
        outs = ["%s/whole.txt" % pm],
        cmd = "run",
        data = [top],
        script = "@bazel-orfs//tools/carve:top_level.tcl",
        tags = tags,
        user_arguments = top_level_user_arguments | {
            "BUDGET_PS": budget_ps,
            "CROSSING_TXT": "$(location %s/whole.txt)" % pm,
            "MAX_SKEW_PS": max_skew_ps,
            "TOP_DIR": "$(location %s)" % top,
            "TOP_FROM": "grt",
        },
        user_sources = scripts,
    )
    sh_test(
        name = "%s_crossing_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:crossing_check.sh"],
        args = [
            "$(rootpath @bazel-orfs//tools/carve:crossing_report)",
            budget_ps,
            "$(rootpath :%s/whole.txt)" % pm,
            "$(rootpath :%s/sets.txt)" % pm,
            "$(rootpath %s)" % times,
        ],
        data = [
            ":%s/sets.txt" % pm,
            ":%s/whole.txt" % pm,
            times,
            "@bazel-orfs//tools/carve:crossing_report",
        ],
        tags = tags,
    )

    # the launch-side bound's check: every set's worst launched crossing
    # is the worst crossing the sets' timing reports for it, each room is
    # the smaller of setup and hold, each bound its crossing less its room
    launch = ":%s_crossing_launch" % pm
    sh_test(
        name = "%s_crossing_launch_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:crossing_launch_check.sh"],
        args = [
            "$(rootpath @bazel-orfs//tools/carve:crossing_launch_check)",
            "$(rootpath %s)" % launch,
            "$(rootpath %s)" % times,
        ],
        data = [
            launch,
            times,
            "@bazel-orfs//tools/carve:crossing_launch_check",
        ],
        tags = tags,
    )

    # the headroom's check: every set's period is the worst crossing the
    # sets' timing reports for it, every what-if is undone, and no move
    # reads worse than the design as built
    headroom = ":%s_crossing_headroom" % pm
    sh_test(
        name = "%s_crossing_headroom_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:crossing_headroom_check.sh"],
        args = [
            "$(rootpath @bazel-orfs//tools/carve:crossing_headroom_check)",
            "$(rootpath %s)" % headroom,
            "$(rootpath %s)" % times,
        ],
        data = [
            headroom,
            times,
            "@bazel-orfs//tools/carve:crossing_headroom_check",
        ],
        tags = tags,
    )

    # the second pass's selection: some cells unfirmed and the rest left
    # firm, and every control raised its stop (unfirm_check.sh)
    sh_test(
        name = "%s_unfirm_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:unfirm_check.sh"],
        args = ["$(rootpath :%s/unfirm.txt)" % pm],
        data = [":%s/unfirm.txt" % pm],
        tags = tags,
    )

    # the second pass against the first: it moved the crossings, by the
    # rule's verdict; the first pass's report against itself must first
    # read as having done nothing (crossing_verdict.sh)
    sh_test(
        name = "%s_crossing_verdict_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:crossing_verdict.sh"],
        args = [
            "$(rootpath %s)" % verdict,
            budget_ps,
            "$(rootpath %s)" % firm.crossing_txt,
            "$(rootpath %s)" % final.crossing_txt,
            "--expect",
            "moved",
            "--sets",
            "$(rootpath %s)" % firm.sets_txt,
            "$(rootpath %s)" % final.sets_txt,
            "--time",
            "$(rootpath %s)" % firm.times,
            "$(rootpath %s)" % final.times,
        ],
        data = [
            firm.sets_txt,
            final.sets_txt,
            firm.times,
            final.times,
            firm.crossing_txt,
            final.crossing_txt,
            verdict,
        ],
        tags = tags,
    )

    # the constraints each pass hands on are the floorplan's, the ones
    # read: the place step's clock period and false paths reach neither
    # the tree, the route nor the report (top_level.tcl's top_checkpoint)
    sh_test(
        name = "%s_top_sdc_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:same_files_check.sh"],
        args = [
            "$(rootpath %s)/3_top_place.sdc" % place,
            "$(rootpath %s)/5_top_grt.sdc" % firm_top,
            "$(rootpath %s)/5_top_grt.sdc" % top,
            "$(rootpath %s)/5_top_grt.sdc" % firm_top,
        ],
        data = [place, top, firm_top],
        tags = tags,
    )

    # how much of each crossing is wire: with no signal wire the worst
    # crossing is lower; the control reads the same twice and fails that
    # check; every set has its report in both (crossing_floor_check.py)
    sh_test(
        name = "%s_crossing_floor_test" % pm,
        srcs = ["@bazel-orfs//tools/carve:crossing_floor_check.sh"],
        args = [
            "$(rootpath @bazel-orfs//tools/carve:crossing_floor_check)",
            budget_ps,
            "$(rootpath %s)" % floor,
            "--control",
            "$(rootpath %s)" % floor_control,
            "--sets",
            "$(rootpath :%s/sets.txt)" % pm,
        ],
        data = [
            floor,
            floor_control,
            ":%s/sets.txt" % pm,
            "@bazel-orfs//tools/carve:crossing_floor_check",
        ],
        tags = tags,
    )

def _crossing_report(stem, out, pm, top, budget_ps, plan_budget, crossing_data_min_ps, top_arguments, top_level_user_arguments, scripts, tags):
    """One pass's crossing report: its sets planned, cut, timed and merged.

    Args:
      stem: the targets' stem, <stem>_crossing_plan and the rest.
      out: the directory of their outputs, in the package.
      pm: the top level's flow, at its floorplan.
      top: the pass's routed top level.
      budget_ps: the crossing report's budget, ps.
      plan_budget: instances a crossing set may hold.
      crossing_data_min_ps: the crossing timing's dataset threshold, ps.
      top_arguments: ORFS variables for top_level.tcl's runs.
      top_level_user_arguments: top_level.tcl's own settings.
      scripts: the scripts top_level.tcl and the timing source.
      tags: forwarded.

    Returns:
      The sets file, the cut sets, the timed sets and the report's
      crossing.txt, by label.
    """

    # the sets: every path's signature held by one
    orfs_run(
        name = "%s_crossing_plan" % stem,
        src = ":%s_floorplan" % pm,
        outs = ["%s/sets.txt" % out, "%s/signatures.txt" % out],
        cmd = "run",
        data = [top],
        script = "@bazel-orfs//tools/carve:crossing_plan.tcl",
        tags = tags,
        user_arguments = {
            "CROSSING_SETS": "$(location %s/sets.txt)" % out,
            "CROSSING_SIGNATURES": "$(location %s/signatures.txt)" % out,
            "PLAN_BUDGET": plan_budget,
            "TOP_DIR": "$(location %s)" % top,
        },
    )

    # The sets are cut and timed on this machine, never on a remote
    # executor: a set takes 60-70 GB to cut or time, and several started at
    # once on remote executors were killed for memory (exit 137); in one
    # action they run one at a time, as memory here allows. Bazel has no
    # per-action memory reservation for local actions to run two at once.
    local = tags + ["no-remote-exec"]

    # each set cut to its crossing cones, written with its parasitics
    orfs_run(
        name = "%s_crossing_sets" % stem,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/sets" % out,
        cmd = "run",
        data = [top, ":%s/sets.txt" % out],
        script = "@bazel-orfs//tools/carve:top_level.tcl",
        arguments = top_arguments,
        tags = local,
        user_arguments = top_level_user_arguments | {
            "CROSSING_SETS": "$(location :%s/sets.txt)" % out,
            "TOP_DIR": "$(location %s)" % top,
            "TOP_FROM": "grt",
        },
        user_sources = scripts,
    )

    # each set timed in a process of its own
    sets = ":%s_crossing_sets" % stem
    orfs_run(
        name = "%s_crossing_time" % stem,
        src = ":%s_floorplan" % pm,
        out_dir = "%s/time" % out,
        cmd = "run",
        data = [sets, ":%s/sets.txt" % out],
        script = "@bazel-orfs//tools/carve:crossing_time_sets.tcl",
        tags = local,
        user_arguments = {
            "BUDGET_PS": budget_ps,
            "CROSSING_DATA_MIN_PS": crossing_data_min_ps,
            "CROSSING_SETS": "$(location :%s/sets.txt)" % out,
            "SETS_DIR": "$(location %s)" % sets,
        },
        user_sources = scripts,
    )

    times = ":%s_crossing_time" % stem
    report = "$(location @bazel-orfs//tools/carve:crossing_report)"
    native.genrule(
        name = "%s_crossings" % stem,
        srcs = [times, ":%s/sets.txt" % out],
        outs = ["%s/crossings/%s" % (out, f) for f in ["crossing.txt", "tables.md", "histogram.svg"]],
        cmd = "%s $$(dirname $(location %s/crossings/crossing.txt)) %s $(location :%s/sets.txt) $(location %s)" % (report, out, budget_ps, out, times),
        tools = ["@bazel-orfs//tools/carve:crossing_report"],
        tags = tags,
    )
    return struct(
        sets_txt = ":%s/sets.txt" % out,
        sets = sets,
        times = times,
        crossing_txt = ":%s/crossings/crossing.txt" % out,
    )
