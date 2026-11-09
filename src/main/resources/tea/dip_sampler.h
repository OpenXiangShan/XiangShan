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

// Simulation-only sampled-target history. Word layout follows Dip.scala.
#ifndef XIANGSHAN_DIP_SAMPLER_H
#define XIANGSHAN_DIP_SAMPLER_H
#include <array>
#include <cstdint>
#include <cstring>

template <int RobSize, int RenameWidth, int CommitWidth>
class DipNativeSampler {
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
  struct Uop {
    Ptr ptr;
    uint64_t pc, psv;
  };
  struct Commit {
    uint64_t valid;
    Ptr ptr;
  };
  struct Flush {
    Ptr ptr;
    uint64_t self, pc, cause;
  };
  struct Frame {
    uint64_t enable, period, valid, fire, stall;
    Uop uops[RenameWidth];
    uint64_t cycle, retired;
    Commit commits[2 * CommitWidth];
    uint64_t flushValid;
    Flush flush;
  };
  struct Target {
    uint64_t generation;
    Ptr ptr;
  };
  struct Slot {
    uint64_t state, cause, pc, kind;
    Target target;
  };
  struct Final {
    uint64_t valid, cycle, kind;
    Target target, next;
    uint64_t pc, cause, self;
  };
  struct Output {
    uint64_t valid, cycle, total, retired;
    Slot slots[RenameWidth];
    Final finals[2 * CommitWidth + 3];
  };
  struct Tracked {
    bool live = false, flag = false, cause = false;
    uint64_t generation = 0;
  };
  Tracked tracked[2 * RobSize];
  uint64_t countdown = 0, totalRetired = 0, waitingGeneration = 0;
  bool recovering = false, flushSettled = false, waiting = false;
  Flush lastFlush{};
  Frame previous{};
  bool previousValid = false, previousPhase = false;

  static int index(const Ptr &p) { return 2 * p.index + !p.former; }
  Ptr pointer(int i) const { return {uint64_t(i / 2), tracked[i].flag, uint64_t(!(i % 2))}; }
  bool atCommit(const Frame &in, const Ptr &p) const {
    for (const auto &c : in.commits) {
      if (c.valid && c.ptr.same(p)) return true;
    }
    return false;
  }
  bool settled(const Frame &in, const Ptr &p) const {
    for (const auto &c : in.commits) {
      if (c.valid && (c.ptr.same(p) || c.ptr.after(p))) return true;
    }
    return false;
  }
  bool killed(const Frame &in, const Ptr &p) const {
    return p.after(in.flush.ptr) || (in.flush.self && p.same(in.flush.ptr));
  }
  Target reference(const Frame &in, const Ptr &p) const {
    const auto &t = tracked[index(p)];
    const bool reusable = t.live && t.flag == bool(p.flag) && !atCommit(in, p) &&
                          !(t.cause && settled(in, p));
    return {reusable ? t.generation : in.cycle, p};
  }
  static int first(uint64_t mask) { return mask ? __builtin_ctzll(mask) : RenameWidth - 1; }
  void process(const Frame &in, Output *output) {
    const bool due = in.enable && countdown == 0;
    std::array<bool, 2 * RobSize> flushed{}, causeSettled{}, ending{};
    bool anyFlushed = false, anySettled = false;
    for (int i = 0; i < 2 * RobSize; ++i) {
      if (tracked[i].live) {
        const auto p = pointer(i);
        flushed[i] = in.flushValid && killed(in, p);
        causeSettled[i] = tracked[i].cause && settled(in, p);
        ending[i] = flushed[i] || causeSettled[i] || atCommit(in, p);
        anyFlushed |= flushed[i];
        anySettled |= causeSettled[i];
      }
    }
    const auto cause = in.flushValid ? in.flush : lastFlush;
    const bool stableCause = in.flushValid ? settled(in, cause.ptr) : flushSettled || settled(in, cause.ptr);
    const bool bind = waiting && in.valid && !in.flushValid;
    const auto &firstUop = in.uops[first(in.valid)];
    const auto firstRef = reference(in, firstUop.ptr);
    const auto causeRef = reference(in, cause.ptr);
    const bool survivingWait = waiting && !bind && !in.flushValid;
    const uint64_t nextWaitGeneration = survivingWait ? waitingGeneration : in.cycle;
    Output result{};
    result.valid = due;
    result.cycle = in.cycle;
    result.total = totalRetired;
    result.retired = in.retired;
    const uint64_t blocked = in.valid & ~in.fire;
    int fired[RenameWidth], fireCount = 0;
    for (int i = 0; i < RenameWidth; ++i) {
      if ((in.fire >> i) & 1) fired[fireCount++] = i;
    }
    for (int slot = 0; slot < RenameWidth; ++slot) {
      auto &s = result.slots[slot];
      if (in.flushValid || (recovering && !in.valid)) {
        s.state = 3;
        s.cause = cause.cause;
        s.pc = cause.pc;
        s.kind = stableCause ? 0 : 1;
        s.target = causeRef;
      } else if (slot < fireCount) {
        const auto &u = in.uops[fired[slot]];
        s.pc = u.pc;
        s.kind = 1;
        s.target = reference(in, u.ptr);
      } else if (blocked) {
        const auto &u = in.uops[first(blocked)];
        s.state = 1;
        s.cause = in.stall;
        s.pc = u.pc;
        s.kind = 1;
        s.target = reference(in, u.ptr);
      } else {
        s.state = in.stall ? 1 : 2;
        s.cause = in.stall;
        s.kind = 2;
        s.target.generation = nextWaitGeneration;
      }
    }
    auto &flushEvent = result.finals[2 * CommitWidth];
    flushEvent.valid = in.flushValid && (anyFlushed || waiting);
    flushEvent.kind = 1;
    flushEvent.target = causeRef;
    flushEvent.pc = cause.pc;
    flushEvent.cause = cause.cause;
    flushEvent.self = cause.self;
    auto &successor = result.finals[2 * CommitWidth + 1];
    successor.valid = bind;
    successor.kind = 2;
    successor.target.generation = waitingGeneration;
    successor.next = firstRef;
    successor.pc = firstUop.pc;
    successor.cause = (firstUop.psv & 3) ? firstUop.psv & 3 : 4;
    Commit latest = in.commits[0];
    for (int i = 1; i < 2 * CommitWidth; ++i) {
      const auto &c = in.commits[i];
      if (!latest.valid || (c.valid && c.ptr.after(latest.ptr))) latest = c;
    }
    auto &watermark = result.finals[2 * CommitWidth + 2];
    watermark.valid = anySettled || (flushEvent.valid && stableCause);
    watermark.kind = 3;
    watermark.target.ptr = latest.ptr;
    for (int i = 0; i < 2 * CommitWidth; ++i) {
      const auto &c = in.commits[i];
      const int idx = index(c.ptr);
      auto &f = result.finals[i];
      f.valid = c.valid && tracked[idx].live && tracked[idx].flag == bool(c.ptr.flag) && !flushed[idx];
      f.target.ptr = c.ptr;
      f.target.generation = tracked[idx].generation;
    }
    for (auto &f : result.finals) f.cycle = in.cycle;
    if (output) {
      *output = result;
      return;
    }

    countdown = !in.enable ? 0 : countdown == 0 ? (in.period ? in.period - 1 : 0) : countdown - 1;
    totalRetired += in.retired;
    if (in.flushValid) {
      recovering = true;
      lastFlush = in.flush;
      flushSettled = settled(in, in.flush.ptr);
    } else {
      if (settled(in, lastFlush.ptr)) flushSettled = true;
      if (in.valid) recovering = false;
    }
    bool newWait = false;
    for (const auto &s : result.slots) newWait |= due && s.kind == 2;
    waiting = survivingWait || newWait;
    if (newWait) waitingGeneration = nextWaitGeneration;
    std::array<bool, 2 * RobSize> touch{}, sampleCause{};
    std::array<Target, 2 * RobSize> refs{};
    if (due) {
      for (const auto &s : result.slots) {
        if (s.kind == 1) {
          const int i = index(s.target.ptr);
          if (touch[i]) {
            refs[i].generation |= s.target.generation;
            refs[i].ptr.flag |= s.target.ptr.flag;
          } else {
            refs[i] = s.target;
          }
          touch[i] = true;
          sampleCause[i] |= s.state == 3;
        }
      }
    }
    if (bind) {
      const int i = index(firstRef.ptr);
      touch[i] = true;
      refs[i] = firstRef;
    }
    if (flushEvent.valid && !stableCause) {
      const int i = index(causeRef.ptr);
      touch[i] = true;
      refs[i] = causeRef;
    }
    for (int i = 0; i < 2 * RobSize; ++i) {
      auto &t = tracked[i];
      const bool fromFlush = flushEvent.valid && !stableCause && index(causeRef.ptr) == i;
      const bool retainCause = t.live && !ending[i] && t.flag == bool(refs[i].ptr.flag) && t.cause;
      if (ending[i]) t.live = false;
      if (touch[i]) {
        t.live = true;
        t.generation = refs[i].generation;
        t.flag = refs[i].ptr.flag;
        t.cause = fromFlush || sampleCause[i] || retainCause;
      }
    }
  }

public:
  void evaluate(bool reset, bool phase, const uint64_t *words, uint64_t *out) {
    if (reset) {
      if (previousValid) {
        for (auto &t : tracked) t.live = false;
        countdown = totalRetired = 0;
        waiting = recovering = flushSettled = false;
        lastFlush = {};
        previousValid = false;
      }
      std::memset(out, 0, sizeof(Output));
      return;
    }
    Frame in;
    std::memcpy(&in, words, sizeof(in));
    // Apply the pending frame once per clock edge, even after repeated evaluations.
    if (previousValid && phase != previousPhase) process(previous, nullptr);
    previous = in;
    previousValid = true;
    previousPhase = phase;
    Output result;
    process(in, &result);
    std::memcpy(out, &result, sizeof(result));
  }
};
#endif
