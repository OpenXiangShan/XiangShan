#include "scenarios/registry.hpp"

namespace {
using memblock::scenarios::Options;
using memblock::scenarios::parse_u64;

Options parse_options(int argc, char **argv)
{
    Options options;
    for (int index = 1; index < argc; ++index) {
        const std::string_view argument(argv[index]);
        if (argument == "--list-tests") {
            options.list_tests = true;
        } else if (argument == "--test" && index + 1 < argc) {
            options.test = argv[++index];
        } else if (argument == "--seed" && index + 1 < argc) {
            options.seed = parse_u64(argv[++index], "--seed");
        } else if (argument == "--transactions" && index + 1 < argc) {
            options.transactions = static_cast<unsigned>(
                parse_u64(argv[++index], "--transactions"));
        } else if (argument == "--no-backpressure") {
            options.backpressure = false;
        } else if (argument == "--hunt-boundaries") {
            options.hunt_boundaries = true;
        } else if (argument == "--constraints" && index + 1 < argc) {
            options.constraint_profile = argv[++index];
        } else if (argument == "--constraint" && index + 1 < argc) {
            options.constraint_overrides.emplace_back(argv[++index]);
        }
    }
    return options;
}

} // namespace

int main(int argc, char **argv)
{
    try {
        const Options options = parse_options(argc, argv);
        const auto entries = memblock::scenarios::scenario_catalog();
        if (options.list_tests) {
            for (const auto &entry : entries) {
                std::cout << entry.name << '\n';
            }
            return 0;
        }
        for (const auto &entry : entries) {
            if (entry.name == options.test) {
                return entry.run(argc, argv, options);
            }
        }
        std::cerr << "unknown test: " << options.test << '\n';
        return 2;
    } catch (const std::exception &error) {
        std::cerr << "MEMBLOCK_ARGUMENT_ERROR reason=" << error.what() << '\n';
        return 2;
    }
}
