import csv
import glob
import sqlite3
from pathlib import Path


STATE_NAMES = {
    0: "computing",
    1: "stalled",
    2: "walk",
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
    width_expr = (
        "COMMITS_COMMITVALID_0 + COMMITS_COMMITVALID_1 + COMMITS_COMMITVALID_2 + "
        "COMMITS_COMMITVALID_3 + COMMITS_COMMITVALID_4 + COMMITS_COMMITVALID_5 + "
        "COMMITS_COMMITVALID_6 + COMMITS_COMMITVALID_7"
    )
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
        row["commit_type_name"] = COMMIT_TYPE_NAMES.get(row["commit_type"], "FUSED_OR_OTHER")
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
        row["commit_type_name"] = COMMIT_TYPE_NAMES.get(row["commit_type"], "FUSED_OR_OTHER")
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


def fetch_pc_commit_type_hotspots(conn: sqlite3.Connection, hart: int, top: int = 20) -> list[dict]:
    return fetch_commit_event_rows(conn, hart, top=top)


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
               REDIRECT_BITS_CFIUPDATE_TARGET as target_pc_raw,
               printf('0x%x', REDIRECT_BITS_CFIUPDATE_TARGET) as target_pc,
               coalesce(sum(REDIRECT_BITS_DEBUGISCTRL), 0) as ctrl_redirects,
               coalesce(sum(REDIRECT_BITS_DEBUGISMEMVIO), 0) as memvio_redirects,
               count(*) as count
        from {table}
        where REDIRECT_VALID=1
        group by REDIRECT_PC, REDIRECT_BITS_CFIUPDATE_TARGET
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
                 COMMITS_INFO_0_DEBUG_PC as pc_raw,
                 COMMITS_ISCOMMIT,
                 COMMITS_COMMITVALID_0,
                 DEBUGLSINFO_S3_ISREPLAY as is_replay,
                 DEBUGLSINFO_S3_ISREPLAYFAST as fast_replay,
                 DEBUGLSINFO_S3_ISREPLAYSLOW as slow_replay,
                 DEBUGLSINFO_S3_ISREPLAYRS as rs_replay,
                 DEBUGLSINFO_S2_ISBANKCONFLICT as bank_conflict,
                 DEBUGLSINFO_S2_ISDCACHEFIRSTMISS as dcache_first_miss,
                 DEBUGLSINFO_S2_ISFORWARDFAIL as forward_fail,
                 DEBUGLSINFO_REPLAYCNT as replay_cnt,
                 lag(STAMP) over (order by {order}) as prev_stamp,
                 lag(COMMITS_INFO_0_DEBUG_PC) over (order by {order}) as prev_pc,
                 lag(DEBUGLSINFO_REPLAYCNT) over (order by {order}) as prev_replay_cnt,
                 lag(COMMITS_ISCOMMIT and COMMITS_COMMITVALID_0) over (order by {order}) as prev_committed
          from {table}
        ), boundaries as (
          select *,
                 case when prev_stamp is null
                            or STAMP != prev_stamp + 1
                            or pc_raw != prev_pc
                            or replay_cnt < prev_replay_cnt
                            or prev_committed
                      then 1 else 0 end as new_entry
          from ordered
        ), grouped as (
          select *,
                 sum(new_entry) over (order by STAMP rows unbounded preceding) as entry_id
          from boundaries
        ), replay_entries as (
          select entry_id,
                 pc_raw,
                 max(is_replay) as is_replay,
                 max(fast_replay) as fast_replay,
                 max(slow_replay) as slow_replay,
                 max(rs_replay) as rs_replay,
                 max(bank_conflict) as bank_conflict,
                 max(dcache_first_miss) as dcache_first_miss,
                 max(forward_fail) as forward_fail,
                 max(replay_cnt) as replay_cnt
          from grouped
          group by entry_id, pc_raw
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


def fetch_replay_hotspots(conn: sqlite3.Connection, hart: int, top: int = 20) -> list[dict]:
    return fetch_replay_event_rows(conn, hart, top=top)


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
