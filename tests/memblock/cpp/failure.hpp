#pragma once

#include <cstdint>
#include <optional>
#include <ostream>
#include <source_location>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace memblock {

// A detected verification failure is untriaged. Its component/code identify
// ownership, not whether the CPU or the testbench is responsible.
struct Failure {
    std::string component;
    std::string code;
    std::string message;
    std::optional<std::uint64_t> cycle;
    std::string file;
    unsigned line = 0;
};

class FailureState {
public:
    explicit FailureState(std::string component, std::string code)
        : component_(std::move(component)), code_(std::move(code)) {}

    void set_cycle(std::uint64_t cycle) { cycle_ = cycle; }

    void record(std::string message,
                std::source_location site = std::source_location::current())
    {
        append(Failure{component_, code_, std::move(message), cycle_,
                       site.file_name(), site.line()});
    }

    void merge(const FailureState &other)
    {
        for (const auto &failure : other.records_) append(failure);
        truncated_ = truncated_ || other.truncated_;
    }

    bool empty() const { return records_.empty(); }
    const std::string &message() const { return first_message_; }
    const std::vector<Failure> &records() const { return records_; }
    bool truncated() const { return truncated_; }

    std::string json() const
    {
        std::ostringstream out;
        out << "{\"schema_version\":1,\"truncated\":" << (truncated_ ? "true" : "false")
            << ",\"failures\":[";
        bool first = true;
        for (const auto &failure : records_) {
            if (!first) out << ',';
            first = false;
            out << "{\"component\":" << quote(failure.component)
                << ",\"code\":" << quote(failure.code)
                << ",\"classification\":\"untriaged\",\"message\":"
                << quote(failure.message) << ",\"cycle\":";
            if (failure.cycle) out << *failure.cycle;
            else out << "null";
            out << ",\"file\":" << quote(failure.file)
                << ",\"line\":" << failure.line << '}';
        }
        out << "]}";
        return out.str();
    }

    friend std::ostream &operator<<(std::ostream &out, const FailureState &state)
    {
        return out << state.message();
    }

private:
    void append(const Failure &failure)
    {
        // Persistent component errors are collected every cycle; deduplicate
        // them without dropping distinct sites/messages or changing first cause.
        for (const auto &existing : records_) {
            if (existing.component == failure.component &&
                existing.code == failure.code && existing.file == failure.file &&
                existing.line == failure.line && existing.message == failure.message)
                return;
        }
        if (records_.size() == capacity) { truncated_ = true; return; }
        // error() historically returned a reference to a member string. Keep
        // that reference valid while later records grow the diagnostic vector.
        if (records_.empty()) first_message_ = failure.message;
        records_.push_back(failure);
    }

    static std::string quote(std::string_view text)
    {
        constexpr char hex[] = "0123456789abcdef";
        std::string result = "\"";
        for (unsigned char c : text) {
            if (c == '"' || c == '\\') {
                result += '\\'; result += static_cast<char>(c);
            } else if (c < 0x20) {
                result += "\\u00"; result += hex[c >> 4]; result += hex[c & 15];
            } else result += static_cast<char>(c);
        }
        result += '"';
        return result;
    }

    static constexpr unsigned capacity = 64;
    std::string component_;
    std::string code_;
    std::optional<std::uint64_t> cycle_;
    std::vector<Failure> records_;
    std::string first_message_;
    bool truncated_ = false;
};

} // namespace memblock
