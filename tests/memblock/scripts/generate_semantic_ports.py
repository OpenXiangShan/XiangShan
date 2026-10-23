#!/usr/bin/env python3
"""Generate the private semantic DUT boundary from its reviewed access mapping.

The existing expected_ports.json remains the interface manifest. This mapping
selects the harness's directly used pins and assigns stable domain names; the
existing generated drive/sample helpers remain the sole lane implementation.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import sys
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
IDENTIFIER = re.compile(r"[a-z][a-z0-9_]*\Z")
CPP_KEYWORDS = set("""
alignas alignof and and_eq asm auto bitand bitor bool break case catch char
char8_t char16_t char32_t class compl concept const consteval constexpr constinit
const_cast continue co_await co_return co_yield decltype default delete do double
dynamic_cast else enum explicit export extern false float for friend goto if
inline int long mutable namespace new noexcept not not_eq nullptr operator or
or_eq private protected public register reinterpret_cast requires return short
signed sizeof static static_assert static_cast struct switch template this
thread_local throw true try typedef typeid typename union unsigned using virtual
void volatile wchar_t while xor xor_eq
""".split())
ADAPTER_MEMBERS = {"initialize_clock", "refresh_comb", "step", "finish", "cycle",
                   "internal_signal", "impl_", "std", "generated"}


class SemanticPortError(ValueError):
    pass


def load_mapping(path: Path, manifest_path: Path) -> list[dict[str, Any]]:
    document = json.loads(path.read_text())
    if not isinstance(document, dict) or document.get("schema_version") != 1:
        raise SemanticPortError("unsupported semantic port mapping schema")
    entries = document.get("ports")
    if not isinstance(entries, list) or not entries:
        raise SemanticPortError("semantic port mapping is empty")
    manifest = {entry["name"]: entry for entry in json.loads(manifest_path.read_text())["ports"]}
    raw_names: set[str] = set()
    semantic_names: set[str] = set()
    for entry in entries:
        if not isinstance(entry, dict) or set(entry) != {"raw", "semantic", "direction", "width"}:
            raise SemanticPortError("invalid semantic port entry")
        raw, semantic = entry["raw"], entry["semantic"]
        if not isinstance(raw, str) or raw not in manifest or raw in raw_names:
            raise SemanticPortError(f"unknown or duplicate raw port: {raw!r}")
        if (not isinstance(semantic, str) or semantic in semantic_names
                or any(not IDENTIFIER.fullmatch(part) for part in semantic.split("."))
                or any(part in CPP_KEYWORDS or "__" in part for part in semantic.split("."))
                or len(semantic.split(".")) < 2):
            raise SemanticPortError(f"invalid or duplicate semantic path: {semantic!r}")
        if (entry["direction"] != manifest[raw]["direction"]
                or type(entry["width"]) is not int
                or entry["width"] != manifest[raw]["width"]):
            raise SemanticPortError(f"semantic mapping differs from interface manifest: {raw}")
        raw_names.add(raw)
        semantic_names.add(semantic)
    return sorted(entries, key=lambda entry: entry["semantic"])


def helper_contracts(generated: str) -> list[dict[str, Any]]:
    """Read signatures, never recreate the generated lane assignments."""
    value_types = set(re.findall(r"^struct (\w+) \{", (ROOT / "cpp/generated_interface_types.hpp").read_text(), re.MULTILINE))
    contracts = []
    pattern = re.compile(r"^inline ([^\n]+?) (\w+)\(\s*UTMemBlock &dut(?:,\s*([^)]*))?\)\s*\{", re.MULTILINE)
    for match in pattern.finditer(generated):
        result, name, arguments = match.groups()
        arguments = " ".join((arguments or "").split())
        for value_type in value_types:
            result = re.sub(r"\b" + value_type + r"\b", "generated::" + value_type, result)
            arguments = re.sub(r"\b" + value_type + r"\b", "generated::" + value_type, arguments)
        passed = []
        for argument in arguments.split(",") if arguments else []:
            argument_name = re.search(r"(\w+)\s*$", argument)
            if argument_name is None:
                raise SemanticPortError(f"unsupported generated helper argument: {name}")
            passed.append(argument_name[1])
        contracts.append({"name": name, "result": result, "arguments": arguments,
                          "passed": passed, "const": result != "void"})
    declared = set(re.findall(r"^inline [^\n]+? (\w+)\([\s\S]*?\)\s*\{", generated, re.MULTILINE))
    if not contracts or len({entry["name"] for entry in contracts}) != len(contracts):
        raise SemanticPortError("missing or duplicate generated DUT helper")
    # Pin-pattern value helpers have no DUT argument and intentionally stay out.
    unsupported = declared - {entry["name"] for entry in contracts} - {"pin_pattern_bytes", "pin_pattern_u64"}
    if unsupported:
        raise SemanticPortError("unsupported generated helpers: " + ", ".join(sorted(unsupported)))
    return contracts


def port_groups(entries: list[dict[str, Any]]) -> str:
    tree: dict[str, Any] = {}
    for entry in entries:
        node = tree
        parts = entry["semantic"].split(".")
        for part in parts[:-1]:
            if part in node and not isinstance(node[part], dict):
                raise SemanticPortError("port and group path collision")
            node = node.setdefault(part, {})
        if parts[-1] in node:
            raise SemanticPortError("port and group path collision")
        node[parts[-1]] = entry["direction"]

    def render(node: dict[str, Any], indent: int) -> list[str]:
        lines = []
        for name, value in sorted(node.items()):
            prefix = " " * indent
            if isinstance(value, dict):
                lines.extend([prefix + "struct {", *render(value, indent + 4), prefix + "} " + name + ";"])
            else:
                lines.append(prefix + ("DrivenPort" if value == "input" else "ReadPort") + " " + name + ";")
        return lines
    return "\n".join(render(tree, 4))


def render(entries: list[dict[str, Any]], helpers: list[dict[str, Any]]) -> tuple[str, str]:
    reserved_roots = ADAPTER_MEMBERS | {helper["name"] for helper in helpers}
    for entry in entries:
        if entry["semantic"].split(".")[0] in reserved_roots:
            raise SemanticPortError("semantic root collides with adapter member: " + entry["semantic"])
    header = '''// Generated by scripts/generate_semantic_ports.py; review config/semantic_ports.json.
#pragma once

#include "generated_interface_types.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace memblock {
class DutAdapter;

// Only DUT inputs have drive operations. Reading an input observes its currently
// driven value and does not cause a refresh, edge, mask, or implicit conversion.
class ReadPort {
public:
    ReadPort() = default;
    std::uint64_t value() const;
    bool boolean() const;
    std::vector<unsigned char> bytes() const;
    unsigned width() const { return width_; }
protected:
    void *handle_ = nullptr;
    unsigned width_ = 0;
    ReadPort(void *handle, unsigned width) : handle_(handle), width_(width) {}
    friend class DutAdapter;
};

class DrivenPort : public ReadPort {
public:
    DrivenPort() = default;
    void set(std::uint64_t value) const;
    void set_bytes(std::vector<unsigned char> &bytes) const;
private:
    DrivenPort(void *handle, unsigned width) : ReadPort(handle, width) {}
    friend class DutAdapter;
};

class DutAdapter {
public:
    DutAdapter(int argc, char **argv);
    ~DutAdapter();
    DutAdapter(const DutAdapter &) = delete;
    DutAdapter &operator=(const DutAdapter &) = delete;

    void initialize_clock();
    void refresh_comb();
    void step();
    void finish();
    std::uint64_t cycle() const;
    // Internal handles exist only for localization. Missing names return null.
    // Their port wrapper stays valid until this adapter is destroyed.
    ReadPort *internal_signal(const std::string &name);

'''
    for helper in helpers:
        header += f"    {helper['result']} {helper['name']}({helper['arguments']})" + (" const" if helper["const"] else "") + ";\n"
    header += "\n" + port_groups(entries) + '''

private:
    class Impl;
    std::unique_ptr<Impl> impl_;
};
} // namespace memblock
'''
    source = '''// Generated by scripts/generate_semantic_ports.py; review config/semantic_ports.json.
#include "dut_adapter.hpp"
#include "generated_port_defaults.hpp"
#include <unordered_map>

namespace memblock {
class DutAdapter::Impl {
public:
    Impl(int argc, char **argv) : dut(argc, argv) {}
    UTMemBlock dut;
    std::unordered_map<std::string, ReadPort> debug_signals;
};

std::uint64_t ReadPort::value() const { return static_cast<XData *>(handle_)->U(); }
bool ReadPort::boolean() const { return static_cast<XData *>(handle_)->B(); }
std::vector<unsigned char> ReadPort::bytes() const
{ return static_cast<XData *>(handle_)->GetBytes(); }
void DrivenPort::set(std::uint64_t value) const
{ static_cast<XData *>(handle_)->ImmSet(value); }
void DrivenPort::set_bytes(std::vector<unsigned char> &bytes) const
{ static_cast<XData *>(handle_)->ImmSetBytes(bytes); }

DutAdapter::DutAdapter(int argc, char **argv)
    : impl_(std::make_unique<Impl>(argc, argv))
{
'''
    for entry in entries:
        kind = "DrivenPort" if entry["direction"] == "input" else "ReadPort"
        source += f"    {entry['semantic']} = {kind}(&impl_->dut.{entry['raw']}, {entry['width']});\n"
    source += '''}
DutAdapter::~DutAdapter() = default;
void DutAdapter::initialize_clock() { impl_->dut.InitClock(impl_->dut.clock); }
void DutAdapter::refresh_comb() { impl_->dut.RefreshComb(); }
void DutAdapter::step() { impl_->dut.Step(); }
void DutAdapter::finish() { impl_->dut.Finish(); }
std::uint64_t DutAdapter::cycle() const { return impl_->dut.xclock.clk; }
ReadPort *DutAdapter::internal_signal(const std::string &name)
{
    const auto known = impl_->debug_signals.find(name);
    if (known != impl_->debug_signals.end()) return &known->second;
    auto *signal = impl_->dut.GetInternalSignal(name);
    if (signal == nullptr) return nullptr;
    auto [inserted, unused] = impl_->debug_signals.try_emplace(name);
    inserted->second = ReadPort(signal, 0);
    return &inserted->second;
}

'''
    for helper in helpers:
        source += f"{helper['result']} DutAdapter::{helper['name']}({helper['arguments']})" + (" const" if helper["const"] else "") + "\n{\n"
        source += f"    return generated::{helper['name']}(impl_->dut" + (", " if helper["passed"] else "") + ", ".join(helper["passed"]) + ");\n}\n\n"
    source += "} // namespace memblock\n"
    return header, source


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("generate", "check"))
    parser.add_argument("--mapping", type=Path, default=ROOT / "config/semantic_ports.json")
    parser.add_argument("--manifest", type=Path, default=ROOT / "config/expected_ports.json")
    parser.add_argument("--helpers", type=Path, default=ROOT / "cpp/generated_port_defaults.hpp")
    parser.add_argument("--output-dir", type=Path, default=ROOT / "cpp/environment")
    args = parser.parse_args()
    try:
        entries = load_mapping(args.mapping, args.manifest)
        helpers = helper_contracts(args.helpers.read_text())
        rendered = render(entries, helpers)
        for name, content in zip(("dut_adapter.hpp", "dut_adapter.cpp"), rendered):
            path = args.output_dir / name
            if args.action == "generate":
                path.parent.mkdir(parents=True, exist_ok=True)
                if not path.exists() or path.read_text() != content:
                    path.write_text(content)
            elif not path.exists() or path.read_text() != content:
                raise SemanticPortError(f"stale semantic DUT adapter: {path}")
        print(f"semantic ports: {len(entries)} mapped pins, {len(helpers)} forwarded helpers")
    except (OSError, ValueError, KeyError) as error:
        print(f"generate_semantic_ports.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
