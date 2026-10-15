import csv
import glob
import sqlite3
from decimal import Decimal
from fractions import Fraction
from pathlib import Path


STATE_NAMES = {
    0: "computing",
    1: "stalled",
    2: "flushed",
    3: "drained",
}

COMMIT_TYPE_NAMES = {
    0: "NORMAL",
    1: "BRANCH",
    2: "LOAD",
    3: "STORE",
}


def latest_db_path(pattern: str = "build/*.db") -> Path:
    matches = [Path(p) for p in glob.glob(pattern)]
    if not matches:
        raise FileNotFoundError(f"no database matches pattern: {pattern}")
    return max(matches, key=lambda p: p.stat().st_mtime)


def tip_table_name(hart: int) -> str:
    return f"Tip_{hart}"


def open_db(db_path: Path | str) -> sqlite3.Connection:
    db_path = Path(db_path)
    if not db_path.exists():
        raise FileNotFoundError(f"database not found: {db_path}")
    conn = sqlite3.connect(str(db_path))
    conn.row_factory = sqlite3.Row
    return conn


def ensure_tip_table(conn: sqlite3.Connection, hart: int) -> str:
    table = tip_table_name(hart)
    row = conn.execute(
        "select name from sqlite_master where type='table' and name=?",
        (table,),
    ).fetchone()
    if row is None:
        raise ValueError(f"table {table} not found in database")
    return table


def _tip_commit_columns(conn: sqlite3.Connection, table: str) -> list[tuple[str, str]]:
    columns = {row[1] for row in conn.execute(f"pragma table_info({table})")}
    indices = sorted(
        int(name.removeprefix("COMMITVALID_"))
        for name in columns
        if name.removeprefix("COMMITVALID_").isdigit()
    )
    pairs = [(f"COMMITVALID_{index}", f"COMMITPCS_{index}") for index in indices]
    required = {
        "ENTRYID",
        "ENTRYGENERATION",
        "ENTRYVALID",
        "FIRSTARCHVALID",
        "FIRSTARCHPC",
        "STALLVALID",
        "STALLPC",
        "OFFENDINGVALID",
        "OFFENDINGPC",
    }
    missing = required - columns
    missing.update(pc for _, pc in pairs if pc not in columns)
    if not pairs or missing:
        details = ", ".join(sorted(missing)) if missing else "COMMITVALID_*/COMMITPCS_*"
        raise ValueError(f"{table} does not contain the TIP attribution schema: {details}")
    return pairs


def _execute_dicts(conn: sqlite3.Connection, sql: str, params=()) -> list[dict]:
    cursor = conn.execute(sql, params)
    rows = cursor.fetchall()
    columns = [desc[0] for desc in cursor.description]
    return [dict(zip(columns, row)) for row in rows]


def fetch_state_summary(conn: sqlite3.Connection, hart: int) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    result = _execute_dicts(
        conn,
        f"""
        select STATE as state,
               count(*) as count
        from {table}
        group by STATE
        order by STATE
        """
    )
    for row in result:
        row["state_name"] = STATE_NAMES.get(row["state"], f"unknown_{row['state']}")
    return result


def fetch_commit_width_summary(conn: sqlite3.Connection, hart: int) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    width_expr = " + ".join(valid for valid, _ in _tip_commit_columns(conn, table))
    return _execute_dicts(
        conn,
        f"""
        select ({width_expr}) as commit_width,
               count(*) as count
        from {table}
        where COMMITS_ISCOMMIT=1
        group by commit_width
        order by commit_width
        """
    )


def _round_cycle_column(values: list[Fraction], places: int = 9) -> list[Decimal]:
    scale = 10 ** places
    scaled = [value * scale for value in values]
    units = [value.numerator // value.denominator for value in scaled]
    target = sum(values) * scale
    if target.denominator != 1:
        raise ValueError("TIP cycle column does not sum to a whole output unit")
    remainder = target.numerator - sum(units)
    order = sorted(
        range(len(values)),
        key=lambda index: (-(scaled[index] - units[index]), index),
    )
    for index in order[:remainder]:
        units[index] += 1
    decimal_scale = Decimal(scale)
    return [Decimal(value) / decimal_scale for value in units]


def fetch_time_profile(conn: sqlite3.Connection, hart: int) -> tuple[list[dict], dict]:
    table = ensure_tip_table(conn, hart)
    commit_columns = _tip_commit_columns(conn, table)
    select_columns = [
        "STATE",
        "FIRSTARCHVALID",
        "FIRSTARCHPC",
        "STALLVALID",
        "STALLPC",
        "OFFENDINGVALID",
        "OFFENDINGPC",
    ]
    select_columns.extend(column for pair in commit_columns for column in pair)
    cursor = conn.execute(
        f"select {', '.join(select_columns)} from {table} order by ID"
    )
    column_index = {description[0]: index for index, description in enumerate(cursor.description)}

    def value(row, column: str):
        return row[column_index[column]]

    totals: dict[int, dict[str, Fraction]] = {}
    total_cycles = 0
    attributed_cycles = Fraction(0)
    pending_drained = 0

    def attribute(pc: int, state_name: str, cycles: Fraction) -> None:
        nonlocal attributed_cycles
        entry = totals.setdefault(
            pc,
            {
                "cycles": Fraction(0),
                "computing_cycles": Fraction(0),
                "stalled_cycles": Fraction(0),
                "flushed_cycles": Fraction(0),
                "drained_cycles": Fraction(0),
            },
        )
        entry["cycles"] += cycles
        entry[f"{state_name}_cycles"] += cycles
        attributed_cycles += cycles

    for row in cursor:
        total_cycles += 1
        state = value(row, "STATE")
        if pending_drained and value(row, "FIRSTARCHVALID"):
            attribute(value(row, "FIRSTARCHPC"), "drained", Fraction(pending_drained))
            pending_drained = 0

        if state == 0:
            pcs = [value(row, pc) for valid, pc in commit_columns if value(row, valid)]
            if not pcs:
                raise ValueError("computing TIP row has no committed architectural instruction")
            weight = Fraction(1, len(pcs))
            for pc in pcs:
                attribute(pc, "computing", weight)
        elif state == 1:
            if not value(row, "STALLVALID"):
                raise ValueError("stalled TIP row has no valid stall PC")
            attribute(value(row, "STALLPC"), "stalled", Fraction(1))
        elif state == 2:
            if not value(row, "OFFENDINGVALID"):
                raise ValueError("flushed TIP row has no valid offending PC")
            attribute(value(row, "OFFENDINGPC"), "flushed", Fraction(1))
        elif state == 3:
            pending_drained += 1
        else:
            raise ValueError(f"unknown TIP state: {state}")

    unattributed_cycles = pending_drained
    pcs = sorted(totals, key=lambda pc: (-totals[pc]["cycles"], pc))
    state_columns = {}
    for state_name in STATE_NAMES.values():
        column = f"{state_name}_cycles"
        state_columns[column] = _round_cycle_column([totals[pc][column] for pc in pcs])

    rows = []
    for index, pc in enumerate(pcs):
        columns = {name: values[index] for name, values in state_columns.items()}
        cycles = sum(columns.values(), Decimal(0))
        rows.append(
            {
                "pc_raw": pc,
                "pc": f"0x{pc:x}",
                "cycles": cycles,
                "percent": round(100.0 * float(totals[pc]["cycles"]) / total_cycles, 6) if total_cycles else 0.0,
                **columns,
            }
        )
    summary = {
        "total_cycles": total_cycles,
        "attributed_cycles": float(attributed_cycles),
        "unattributed_cycles": unattributed_cycles,
        "coverage_percent": round(100.0 * attributed_cycles / total_cycles, 6) if total_cycles else 0.0,
    }
    return rows, summary


def _commit_union(table: str, columns: str) -> str:
    parts = []
    for idx in range(8):
        parts.append(
            f"""
            select {columns.format(i=idx)}
            from {table}
            where COMMITS_ISCOMMIT=1 and COMMITS_COMMITVALID_{idx}=1
            """
        )
    return "\nunion all\n".join(parts)


def fetch_commit_type_summary(conn: sqlite3.Connection, hart: int) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    union = _commit_union(table, "COMMITS_INFO_{i}_COMMITTYPE as commit_type")
    result = _execute_dicts(
        conn,
        f"""
        with commits as (
          {union}
        )
        select commit_type,
               count(*) as count
        from commits
        group by commit_type
        order by commit_type
        """
    )
    for row in result:
        row["commit_type_name"] = COMMIT_TYPE_NAMES[row["commit_type"]]
    return result


def fetch_commit_event_rows(conn: sqlite3.Connection, hart: int, top: int | None = 100) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    union = _commit_union(
        table,
        "COMMITS_INFO_{i}_DEBUG_PC as pc_raw, COMMITS_INFO_{i}_COMMITTYPE as commit_type",
    )
    limit_clause = "limit ?" if top is not None else ""
    params = (top,) if top is not None else ()
    rows = _execute_dicts(
        conn,
        f"""
        with commits as (
          {union}
        )
        select pc_raw,
               printf('0x%x', pc_raw) as pc,
               commit_type,
               count(*) as count
        from commits
        group by pc_raw, commit_type
        order by count desc, pc_raw asc, commit_type asc
        {limit_clause}
        """,
        params,
    )
    for row in rows:
        row["commit_type_name"] = COMMIT_TYPE_NAMES[row["commit_type"]]
    return rows


def fetch_pc_hotspots(conn: sqlite3.Connection, hart: int, top: int = 20) -> list[dict]:
    rows = fetch_commit_event_rows(conn, hart, top=None)
    if not rows:
        return []
    totals: dict[int, int] = {}
    names: dict[int, str] = {}
    for row in rows:
        pc_raw = row["pc_raw"]
        totals[pc_raw] = totals.get(pc_raw, 0) + row["count"]
        names.setdefault(pc_raw, row["pc"])
    sorted_items = sorted(totals.items(), key=lambda item: (-item[1], item[0]))
    result = [
        {"pc": names[pc_raw], "count": count}
        for pc_raw, count in sorted_items[:top]
    ]
    return result


def fetch_redirect_summary(conn: sqlite3.Connection, hart: int) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    rows = _execute_dicts(
        conn,
        f"""
        select count(*) as redirects,
               coalesce(sum(REDIRECT_BITS_DEBUGISCTRL), 0) as ctrl_redirects,
               coalesce(sum(REDIRECT_BITS_DEBUGISMEMVIO), 0) as memvio_redirects
        from {table}
        where REDIRECT_VALID=1
        """
    )
    return rows


def fetch_redirect_targets(conn: sqlite3.Connection, hart: int, top: int = 20) -> list[dict]:
    rows = fetch_redirect_event_rows(conn, hart, top=None)
    if not rows:
        return []
    totals: dict[int, int] = {}
    labels: dict[int, str] = {}
    for row in rows:
        target_raw = row["target_pc_raw"]
        totals[target_raw] = totals.get(target_raw, 0) + row["count"]
        labels.setdefault(target_raw, row["target_pc"])
    sorted_items = sorted(totals.items(), key=lambda item: (-item[1], item[0]))
    result = [
        {"target": labels[target_raw], "count": count}
        for target_raw, count in sorted_items[:top]
    ]
    return result


def fetch_redirect_event_rows(conn: sqlite3.Connection, hart: int, top: int | None = 100) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    limit_clause = "limit ?" if top is not None else ""
    params = (top,) if top is not None else ()
    return _execute_dicts(
        conn,
        f"""
        select REDIRECT_PC as source_pc_raw,
               printf('0x%x', REDIRECT_PC) as source_pc,
               REDIRECT_BITS_TARGET as target_pc_raw,
               printf('0x%x', REDIRECT_BITS_TARGET) as target_pc,
               coalesce(sum(REDIRECT_BITS_DEBUGISCTRL), 0) as ctrl_redirects,
               coalesce(sum(REDIRECT_BITS_DEBUGISMEMVIO), 0) as memvio_redirects,
               count(*) as count
        from {table}
        where REDIRECT_VALID=1
        group by REDIRECT_PC, REDIRECT_BITS_TARGET
        order by count desc, source_pc_raw asc, target_pc_raw asc
        {limit_clause}
        """,
        params,
    )


def _replay_entries_cte(table: str) -> str:
    order = "STAMP, rowid"
    return f"""
        with ordered as (
          select STAMP,
                 ENTRYID as rob_entry_id,
                 ENTRYGENERATION as rob_entry_generation,
                 FIRSTARCHPC as pc_raw,
                 DEBUGLSINFO_S3_ISREPLAY as is_replay,
                 DEBUGLSINFO_S3_ISREPLAYFAST as fast_replay,
                 DEBUGLSINFO_S3_ISREPLAYSLOW as slow_replay,
                 DEBUGLSINFO_S3_ISREPLAYRS as rs_replay,
                 DEBUGLSINFO_S2_ISBANKCONFLICT as bank_conflict,
                 DEBUGLSINFO_S2_ISDCACHEFIRSTMISS as dcache_first_miss,
                 DEBUGLSINFO_S2_ISFORWARDFAIL as forward_fail,
                 DEBUGLSINFO_REPLAYCNT as replay_cnt,
                 lag(STAMP) over (order by {order}) as prev_stamp,
                 lag(ENTRYID) over (order by {order}) as prev_entry_id,
                 lag(ENTRYGENERATION) over (order by {order}) as prev_entry_generation,
                 lag(DEBUGLSINFO_REPLAYCNT) over (order by {order}) as prev_replay_cnt
          from {table}
          where STATE != 3 and ENTRYVALID=1 and FIRSTARCHVALID=1
        ), boundaries as (
          select *,
                 case when prev_stamp is null
                            or STAMP != prev_stamp + 1
                            or rob_entry_id != prev_entry_id
                            or rob_entry_generation != prev_entry_generation
                            or replay_cnt < prev_replay_cnt
                      then 1 else 0 end as new_entry
          from ordered
        ), grouped as (
          select *,
                 sum(new_entry) over (order by STAMP rows unbounded preceding) as entry_id
          from boundaries
        ), replay_entries as (
          select entry_id,
                 max(pc_raw) as pc_raw,
                 max(is_replay) as is_replay,
                 max(fast_replay) as fast_replay,
                 max(slow_replay) as slow_replay,
                 max(rs_replay) as rs_replay,
                 max(bank_conflict) as bank_conflict,
                 max(dcache_first_miss) as dcache_first_miss,
                 max(forward_fail) as forward_fail,
                 max(replay_cnt) as replay_cnt
          from grouped
          group by entry_id
          having max(is_replay)=1
              or max(bank_conflict)=1
              or max(dcache_first_miss)=1
              or max(forward_fail)=1
        )
    """


def fetch_replay_summary(conn: sqlite3.Connection, hart: int) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    return _execute_dicts(
        conn,
        f"""
        {_replay_entries_cte(table)}
        select count(*) as entries,
               coalesce(sum(is_replay), 0) as replay_entries,
               coalesce(sum(fast_replay), 0) as fast_replay,
               coalesce(sum(slow_replay), 0) as slow_replay,
               coalesce(sum(rs_replay), 0) as rs_replay,
               coalesce(sum(bank_conflict), 0) as bank_conflict,
               coalesce(sum(dcache_first_miss), 0) as dcache_first_miss,
               coalesce(sum(forward_fail), 0) as forward_fail,
               coalesce(sum(replay_cnt), 0) as replay_cnt_sum
        from replay_entries
        """,
    )


def fetch_replay_event_rows(conn: sqlite3.Connection, hart: int, top: int | None = 100) -> list[dict]:
    table = ensure_tip_table(conn, hart)
    limit_clause = "limit ?" if top is not None else ""
    params = (top,) if top is not None else ()
    return _execute_dicts(
        conn,
        f"""
        {_replay_entries_cte(table)}
        select pc_raw,
               printf('0x%x', pc_raw) as pc,
               count(*) as count,
               coalesce(sum(fast_replay), 0) as fast_replay,
               coalesce(sum(slow_replay), 0) as slow_replay,
               coalesce(sum(rs_replay), 0) as rs_replay,
               coalesce(sum(bank_conflict), 0) as bank_conflict,
               coalesce(sum(dcache_first_miss), 0) as dcache_first_miss,
               coalesce(sum(forward_fail), 0) as forward_fail,
               coalesce(sum(replay_cnt), 0) as replay_cnt_sum
        from replay_entries
        group by pc_raw
        order by replay_cnt_sum desc, count desc, pc_raw asc
        {limit_clause}
        """,
        params,
    )


def write_csv(path: Path | str, rows: list[dict]) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        with path.open("w", newline="", encoding="utf-8") as fp:
            fp.write("")
        return
    with path.open("w", newline="", encoding="utf-8") as fp:
        writer = csv.DictWriter(fp, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
