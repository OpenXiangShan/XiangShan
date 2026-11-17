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
#include <algorithm>
#include <unordered_map>
#include <vector>
#include <utility>

// The L1 epoch and allocationCycle protect both reusable transaction IDs and ROB slots.
// A late demand merging into a prefetch inherits the outstanding transaction's miss.
class TeaNativeLlcTracker {
public:
  struct Event { uint64_t kind, id, a, b, c, d; };
  struct Token {
    uint64_t index, flag, former, rank, cycle;
    bool operator==(const Token &b) const {
      return index == b.index && flag == b.flag && former == b.former && rank == b.rank && cycle == b.cycle;
    }
  };
private:
  struct Ref { uint64_t id, epoch; };
  struct L1 {
    uint64_t epoch;
    bool miss = false;
    std::vector<Token> waiters;
  };
  struct L2 {
    bool miss = false;
    std::vector<Ref> requests;
  };
  uint64_t epoch = 0;
  std::unordered_map<uint64_t, L1> l1;
  std::unordered_map<uint64_t, L2> l2;

public:
  template <typename Alive> void prune(Alive alive) {
    for (auto &request : l1) {
      auto &waiters = request.second.waiters;
      waiters.erase(std::remove_if(waiters.begin(), waiters.end(),
        [&](const Token &t) { return !alive(t); }), waiters.end());
    }
  }
  template <typename Resolve, typename Notify>
  void observe(const Event *events, int count, Resolve resolve, Notify notify) {
    // Each pass follows the cache handoff order, independent of probe port order.
    for (int i = 0; i < count; ++i) {
      const auto &e = events[i];
      if (e.kind == 1) l1[e.id] = L1{++epoch, false, {}};
    }
    for (int i = 0; i < count; ++i) {
      const auto &e = events[i];
      if (e.kind != 2) continue;
      auto request = l1.find(e.id);
      Token token{};
      if (request == l1.end() || !resolve(e, token)) continue;
      auto &r = request->second;
      if (std::find(r.waiters.begin(), r.waiters.end(), token) == r.waiters.end()) r.waiters.push_back(token);
      if (r.miss) notify(token);
    }
    for (int i = 0; i < count; ++i) {
      const auto &e = events[i];
      if (e.kind != 4 && e.kind != 5) continue;
      if (e.kind == 4) l2[e.id] = L2{};
      auto transaction = l2.find(e.id);
      auto request = l1.find(e.a);
      if (transaction == l2.end() || request == l1.end()) continue;
      auto &t = transaction->second;
      auto &r = request->second;
      t.requests.push_back({e.a, r.epoch});
      if (t.miss) {
        r.miss = true;
        for (const auto &token : r.waiters) notify(token);
      }
    }
    for (int i = 0; i < count; ++i) {
      const auto &e = events[i];
      if (e.kind != 6 || !e.a) continue;
      auto transaction = l2.find(e.id);
      if (transaction == l2.end()) continue;
      transaction->second.miss = true;
      for (const auto &ref : transaction->second.requests) {
        auto request = l1.find(ref.id);
        if (request == l1.end() || request->second.epoch != ref.epoch) continue;
        auto &r = request->second;
        r.miss = true;
        for (const auto &token : r.waiters) notify(token);
      }
    }
    // A final result in the release cycle still belongs to the old lifetime.
    for (int i = 0; i < count; ++i) {
      const auto &e = events[i];
      if (e.kind != 3 && e.kind != 7) continue;
      for (uint64_t mask = e.a; mask; mask &= mask - 1) {
        const auto id = e.id + __builtin_ctzll(mask);
        if (e.kind == 3) l1.erase(id);
        else l2.erase(id);
      }
    }
  }
};

template <int RobSize, int RenameWidth, int CommitWidth, int Writebacks, int Events, int LlcEvents>
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
  struct Read {
    Ptr ptr;
    uint64_t pc;
  };
  struct Frame {
    uint64_t cycle, mark;
    Ptr head;
    uint64_t ready[2];
    Read read[2];
    uint64_t redirect;
    Ptr flush;
    uint64_t self;
    Alloc alloc[RenameWidth];
    Update enq[RenameWidth];
    Completion wb[Writebacks];
    Event events[Events];
    Commit commits[2 * CommitWidth];
    TeaNativeLlcTracker::Event llc[LlcEvents];
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
  TeaNativeLlcTracker llc, pendingLlc;
  std::vector<TeaNativeLlcTracker::Token> llcUpdates;
  bool live(const TeaNativeLlcTracker::Token &t) const {
    const auto &e = entries[t.index];
    const auto &m = e.members[t.rank];
    return m.live && e.flag == t.flag && m.former == t.former && e.cycle == t.cycle;
  }
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
    const TeaNativeLlcTracker::Token token{p.index, p.flag, p.former, uint64_t(rank), entries[p.index].cycle};
    if (std::find(llcUpdates.begin(), llcUpdates.end(), token) != llcUpdates.end()) psv |= uint64_t(1) << 8;
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
    llc = std::move(pendingLlc);
    for (const auto &t : llcUpdates) {
      if (live(t)) entries[t.index].members[t.rank].psv |= uint64_t(1) << 8;
    }
    llcUpdates.clear();
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
    llc.prune([&](const TeaNativeLlcTracker::Token &t) { return live(t); });
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
    if (reset) {
      if (previousValid) {
        for (auto &e : entries) {
          for (auto &m : e.members) m.live = m.sampled = false;
        }
        sampledEntry = {};
        previousValid = false;
        llc = TeaNativeLlcTracker{};
        pendingLlc = TeaNativeLlcTracker{};
        llcUpdates.clear();
      }
      std::memset(out, 0, (8 + CommitWidth * RenameWidth * 6 + RenameWidth * 9) * sizeof(uint64_t));
      return;
    }
    Frame in;
    std::memcpy(&in, words, sizeof(in));
    // Combinational reevaluations replace the pending frame, never apply it twice.
    if (previousValid && phase != previousPhase) advance(previous);
    previous = in;
    previousPhase = phase;
    previousValid = true;
    pendingLlc = llc;
    llcUpdates.clear();
    pendingLlc.observe(in.llc, LlcEvents,
      [&](const TeaNativeLlcTracker::Event &e, TeaNativeLlcTracker::Token &t) {
        if (e.a >= RobSize || e.d >= RenameWidth) return false;
        t = {e.a, e.b, e.c, e.d, entries[e.a].cycle};
        return live(t);
      },
      [&](const TeaNativeLlcTracker::Token &t) { if (live(t)) llcUpdates.push_back(t); });
    for (const auto &query : in.read) {
      const auto &p = query.ptr;
      int rank = RenameWidth - 1;
      const auto &e = entries[p.index];
      for (int r = 0; r < RenameWidth; ++r) {
        if (e.members[r].live && e.flag == p.flag && e.members[r].former == p.former &&
            e.members[r].pc == query.pc) {
          rank = r;
          break;
        }
      }
      *out++ = e.members[rank].live && e.flag == p.flag && e.members[rank].former == p.former &&
               e.members[rank].pc == query.pc;
      *out++ = merged(in, ptr(p.index, rank), rank, e.members[rank].psv);
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
