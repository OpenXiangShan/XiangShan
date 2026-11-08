/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// Simulation-only architectural member history. Input/output words follow TeaMemberTracker.scala.
#ifndef XIANGSHAN_TEA_MEMBER_TRACKER_H
#define XIANGSHAN_TEA_MEMBER_TRACKER_H
#include <array>
#include <cassert>
#include <cstdint>
#include <cstring>

template <int RobSize, int RenameWidth, int CommitWidth, int Writebacks, int Events>
class TeaNativeMemberTracker {
  struct Ptr {
    uint64_t index, flag, former;
    bool same(const Ptr &b) const {
      return index == b.index && flag == b.flag && former == b.former;
    }
    bool after(const Ptr &b) const {
      return ((flag != b.flag) != (index > b.index)) ||
             (index == b.index && flag == b.flag && !former && b.former);
    }
  };
  struct Alloc {
    uint64_t valid;
    Ptr ptr;
    uint64_t rank, owner, pc, psv, remaining;
  };
  struct Update {
    uint64_t valid;
    Ptr ptr;
    uint64_t rank, mask;
  };
  struct Completion {
    uint64_t valid;
    Ptr ptr;
    uint64_t rank;
  };
  struct Event {
    uint64_t valid;
    Ptr ptr;
    uint64_t mask;
  };
  struct Commit {
    uint64_t valid;
    Ptr ptr;
  };
  struct Frame {
    uint64_t cycle, mark;
    Ptr head;
    uint64_t ready[2];
    Ptr read[2];
    uint64_t redirect;
    Ptr flush;
    uint64_t self;
    Alloc alloc[RenameWidth];
    Update enq[RenameWidth];
    Completion wb[Writebacks];
    Event events[Events];
    Commit commits[2 * CommitWidth];
  };
  struct Member {
    uint64_t former = 0, owner = 0, pc = 0, psv = 0, remaining = 0;
    bool live = false, sampled = false;
  };
  struct Entry {
    uint64_t flag = 0, cycle = 0;
    Member members[RenameWidth];
  };
  Entry entries[RobSize];
  Ptr sampledEntry{};
  Frame previous{};
  bool previousValid = false, previousPhase = false;

  Ptr ptr(int entry, int rank) const {
    return {uint64_t(entry), entries[entry].flag, entries[entry].members[rank].former};
  }
  bool killed(const Frame &in, const Ptr &p) const {
    return in.redirect && (p.after(in.flush) || (in.self && p.same(in.flush)));
  }
  bool retired(const Frame &in, const Ptr &p) const {
    for (const auto &c : in.commits) {
      if (c.valid && c.ptr.same(p)) return true;
    }
    return false;
  }
  uint64_t merged(const Frame &in, const Ptr &p, int rank, uint64_t psv) const {
    for (const auto &e : in.enq) {
      if (e.valid && e.rank == uint64_t(rank) && e.ptr.same(p)) psv |= e.mask;
    }
    for (const auto &e : in.events) {
      if (e.valid && e.ptr.same(p)) psv |= e.mask;
    }
    return psv;
  }
  int unfinished(const Frame &in) const {
    const auto &e = entries[in.head.index];
    for (int rank = 0; rank < RenameWidth; ++rank) {
      const auto &m = e.members[rank];
      if (!m.live || e.flag != in.head.flag) continue;
      int count = 0;
      for (const auto &other : e.members) count += other.live && other.former == m.former;
      if (!(in.ready[!m.former] || (count > 1 && e.members[m.owner].remaining == 0))) return rank;
    }
    return -1;
  }
  void advance(const Frame &in) {
    const int stalled = unfinished(in);
    const bool mark = in.mark && stalled >= 0 &&
                      !retired(in, ptr(in.head.index, stalled)) && !killed(in, ptr(in.head.index, stalled));
    std::array<bool, RobSize> allocating{};
    for (const auto &a : in.alloc) {
      if (a.valid) allocating[a.ptr.index] = true;
    }

    // Independent WB ports each remove one remaining uop, with saturation.
    for (const auto &w : in.wb) {
      if (w.valid && !allocating[w.ptr.index]) {
        auto &e = entries[w.ptr.index];
        auto &m = e.members[w.rank];
        if (m.live && e.flag == w.ptr.flag && m.former == w.ptr.former && m.remaining) --m.remaining;
      }
    }
    for (const auto &u : in.enq) {
      if (u.valid && !allocating[u.ptr.index]) {
        auto &e = entries[u.ptr.index];
        auto &m = e.members[u.rank];
        if (m.live && e.flag == u.ptr.flag && m.former == u.ptr.former) m.psv |= u.mask;
      }
    }
    for (const auto &u : in.events) {
      if (u.valid && !allocating[u.ptr.index]) {
        auto &e = entries[u.ptr.index];
        if (e.flag == u.ptr.flag) {
          for (auto &m : e.members) {
            if (m.live && m.former == u.ptr.former) m.psv |= u.mask;
          }
        }
      }
    }
    for (const auto &c : in.commits) {
      if (c.valid && !allocating[c.ptr.index]) {
        auto &e = entries[c.ptr.index];
        if (e.flag == c.ptr.flag) {
          for (auto &m : e.members) {
            if (m.former == c.ptr.former) m.live = m.sampled = false;
          }
        }
      }
    }
    if (in.redirect) {
      for (int entry = 0; entry < RobSize; ++entry) {
        if (!allocating[entry]) {
          for (int rank = 0; rank < RenameWidth; ++rank) {
            auto &m = entries[entry].members[rank];
            if (m.live && killed(in, ptr(entry, rank))) m.live = m.sampled = false;
          }
        }
      }
    }
    // Allocation replaces the entry's membership, but retains unused member payloads.
    for (const auto &a : in.alloc) {
      if (a.valid && allocating[a.ptr.index]) {
        auto &e = entries[a.ptr.index];
        for (auto &m : e.members) m.live = m.sampled = false;
        e.flag = a.ptr.flag;
        e.cycle = in.cycle;
        allocating[a.ptr.index] = false;
      }
    }
    for (const auto &a : in.alloc) {
      if (a.valid) {
        auto &m = entries[a.ptr.index].members[a.rank];
        assert(!m.live && "duplicate TEA architectural member");
        m.live = true;
        m.former = a.ptr.former;
        m.owner = a.owner;
        m.pc = a.pc;
        m.psv = merged(in, a.ptr, a.rank, a.psv);
        m.remaining = a.remaining;
      }
    }
    if (in.mark && stalled >= 0) sampledEntry = in.head;
    if (mark) entries[in.head.index].members[stalled].sampled = true;
  }
  void result(const Frame &in, int entry, int rank, uint64_t *&out) const {
    const auto &e = entries[entry];
    const auto &m = e.members[rank];
    *out++ = e.cycle;
    *out++ = entry;
    *out++ = rank;
    *out++ = m.pc;
    *out++ = merged(in, ptr(entry, rank), rank, m.psv);
  }

public:
  void evaluate(bool reset, bool phase, const uint64_t *words, uint64_t *out) {
    Frame in;
    std::memcpy(&in, words, sizeof(in));
    if (reset) {
      for (auto &e : entries) {
        for (auto &m : e.members) m.live = m.sampled = false;
      }
      sampledEntry = {};
      previousValid = false;
    } else {
      // Combinational reevaluations replace the pending frame, never apply it twice.
      if (previousValid && phase != previousPhase) advance(previous);
      previous = in;
      previousPhase = phase;
      previousValid = true;
    }
    for (const auto &p : in.read) {
      int rank = RenameWidth - 1;
      const auto &e = entries[p.index];
      for (int r = 0; r < RenameWidth; ++r) {
        if (e.members[r].live && e.flag == p.flag && e.members[r].former == p.former) {
          rank = r;
          break;
        }
      }
      result(in, p.index, rank, out);
    }
    const int stalled = unfinished(in);
    const int rank = stalled < 0 ? RenameWidth - 1 : stalled;
    *out++ = stalled >= 0;
    *out++ = entries[in.head.index].cycle;
    *out++ = in.head.index;
    *out++ = rank;
    for (int lane = 0; lane < CommitWidth; ++lane) {
      const auto &e = entries[in.commits[2 * lane].ptr.index];
      for (int r = 0; r < RenameWidth; ++r) {
        bool matching = false;
        for (int slot = 0; slot < 2; ++slot) {
          const auto &c = in.commits[2 * lane + slot];
          matching |= c.valid && c.ptr.flag == e.flag && c.ptr.former == e.members[r].former;
        }
        *out++ = e.members[r].live && matching;
        result(in, in.commits[2 * lane].ptr.index, r, out);
      }
    }
    const auto &e = entries[sampledEntry.index];
    for (int r = 0; r < RenameWidth; ++r) {
      const auto p = ptr(sampledEntry.index, r);
      const bool retire = e.members[r].live && retired(in, p);
      const bool kill = e.members[r].live && killed(in, p);
      *out++ = e.members[r].live && e.members[r].sampled && e.flag == sampledEntry.flag && (retire || kill);
      *out++ = in.cycle;
      *out++ = 0;
      *out++ = kill && !retire;
      result(in, sampledEntry.index, r, out);
    }
  }
};
#endif
