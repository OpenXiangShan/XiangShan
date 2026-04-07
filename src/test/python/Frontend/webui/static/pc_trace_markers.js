(function initPcTraceMarkers(root, factory) {
  const api = factory();
  if (typeof module === "object" && module.exports) {
    module.exports = api;
  }
  root.PcTraceMarkers = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function factory() {
  const FRONTEND_FILL = "#57c8ff";
  const GOLDEN_FILL = "#ffd166";
  const FRONTEND_SLOT_SPREAD = 8;

  function isOverlap(point) {
    return Boolean(
      point
      && point.frontend_valid
      && point.frontend_pc != null
      && point.golden_pc != null
      && Number(point.frontend_pc) === Number(point.golden_pc)
    );
  }

  function canRenderGoldenTrack(point) {
    return isOverlap(point);
  }

  function classifyTracePointMarker(point) {
    const golden = canRenderGoldenTrack(point)
      ? {
          radius: 1.7,
          fill: GOLDEN_FILL,
        }
      : null;
    const frontend = point && point.frontend_valid && point.frontend_pc != null
      ? {
          radius: 1.9,
          fill: FRONTEND_FILL,
        }
      : null;

    if (golden && frontend && isOverlap(point)) {
      return {
        kind: "overlap",
        golden: {
          radius: 3.1,
          fill: golden.fill,
        },
        frontend: {
          radius: 1.55,
          fill: frontend.fill,
        },
      };
    }

    if (frontend) {
      return {
        kind: "frontend_only",
        golden: null,
        frontend,
      };
    }

    return {
      kind: "none",
      golden: null,
      frontend: null,
    };
  }

  function expandTraceEventPoints(evt) {
    const p = (evt && evt.payload) || {};
    const cycle = Number((evt && evt.cycle) || 0);
    const ts = Number((evt && evt.ts) || 0);
    const primaryFrontendPc = p.frontend_pc == null ? null : Number(p.frontend_pc);
    const primaryPoint = {
      cycle,
      frontend_pc: primaryFrontendPc,
      frontend_instr: p.frontend_instr == null ? null : Number(p.frontend_instr),
      golden_pc: p.golden_pc == null ? null : Number(p.golden_pc),
      frontend_valid: Boolean(p.frontend_valid === true && primaryFrontendPc != null),
      is_rvc: Boolean(p.is_rvc === true),
      pred_taken: Boolean(p.pred_taken === true),
      slot: p.slot == null ? -1 : Number(p.slot),
      redirect_update: Boolean(p.redirect_update === true),
      frontend_line: true,
      ts,
    };

    const out = [primaryPoint];
    const slots = Array.isArray(p.frontend_slots) ? p.frontend_slots : [];
    for (const item of slots) {
      const slot = Number(item.slot);
      const pc = item.pc == null ? null : Number(item.pc);
      if (slot === primaryPoint.slot) continue;
      if (pc == null) continue;
      out.push({
        cycle,
        frontend_pc: pc,
        frontend_instr: item.instr == null ? null : Number(item.instr),
        golden_pc: null,
        frontend_valid: true,
        is_rvc: Boolean(item.is_rvc === true),
        pred_taken: Boolean(item.pred_taken === true),
        slot,
        redirect_update: false,
        frontend_line: false,
        ts,
      });
    }
    return out;
  }

  function expandRawTracePoints(evt) {
    const p = (evt && evt.payload) || {};
    const cycle = Number((evt && evt.cycle) || 0);
    const ts = Number((evt && evt.ts) || 0);
    const goldenSlots = Array.isArray(p.golden_slots) ? p.golden_slots.map((value) => Number(value)) : [];
    const slots = Array.isArray(p.slots) ? p.slots : [];
    if (slots.length === 0) {
      return [];
    }
    const item = slots[0];
    const pc = item.pc == null ? null : Number(item.pc);
    if (pc == null) {
      return [];
    }
    return [
      {
        cycle,
        frontend_pc: pc,
        frontend_instr: item.instr == null ? null : Number(item.instr),
        golden_pc: goldenSlots.length > 0 ? Number(goldenSlots[0]) : null,
        frontend_valid: true,
        is_rvc: Boolean(item.is_rvc === true),
        pred_taken: Boolean(item.pred_taken === true),
        slot: Number(item.slot),
        redirect_update: false,
        frontend_line: true,
        ts,
      },
    ];
  }

  function frontendTrackPc(point) {
    if (!point || point.frontend_valid !== true || point.frontend_pc == null) {
      return null;
    }
    return Number(point.frontend_pc);
  }

  function auxiliaryFrontendPoints(points) {
    return Array.isArray(points)
      ? points.filter((point) => point && point.frontend_line === false && point.frontend_valid === true && point.frontend_pc != null)
      : [];
  }

  function tracePointCoord(point) {
    const cycle = Number((point && point.cycle) || 0);
    const slot = Number((point && point.slot) || -1);
    if (!Number.isFinite(slot) || slot < 0) {
      return cycle;
    }
    return cycle + (slot / FRONTEND_SLOT_SPREAD);
  }

  function goldenTrackPoints(points) {
    return Array.isArray(points) ? points.filter((point) => canRenderGoldenTrack(point)) : [];
  }

  function buildTriggerMark(evt) {
    const et = String((evt && evt.type) || "");
    const p = (evt && evt.payload) || {};
    const cycle = Number((evt && evt.cycle) || 0);

    if (et === "monitor.dut_redirect") {
      return {
        phase: "trigger",
        cycle,
        target_pc: Number(p.target_pc || p.target || 0),
        reason: "",
        kind: "redirect",
      };
    }
    if (et === "backend.exception") {
      return {
        phase: "trigger",
        cycle,
        target_pc: Number(p.target_pc || p.target || 0),
        reason: String(p.reason || ""),
        kind: "exception",
      };
    }
    return null;
  }

  return {
    buildTriggerMark,
    canRenderGoldenTrack,
    classifyTracePointMarker,
    auxiliaryFrontendPoints,
    expandRawTracePoints,
    expandTraceEventPoints,
    frontendTrackPc,
    goldenTrackPoints,
    tracePointCoord,
  };
});
