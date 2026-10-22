// Compiled by test_dcache_release_index.py against the production
// DcacheReleaseIndex. No DUT, Picker model, or simulation is needed.

static void require(bool condition, const char *reason)
{
    if (!condition) {
        throw std::runtime_error(reason);
    }
}

static void check_full_line_counts()
{
    memblock::DcacheReleaseIndex index;
    index.record(6, 6, 0x1000);
    index.record(7, 6, 0x1000);
    index.record(7, 6, 0x1040);
    require(index.line_count(0x1000, false).value_or(99) == 2,
            "full-line release count");
    require(index.line_count(0x1000, true).value_or(99) == 1,
            "full-line release data count");
    require(index.line_count(0x1040, true).value_or(99) == 1,
            "second full-line release data count");
    require(index.line_count(0x1080, false).value_or(99) == 0,
            "unseen line count");
}

static void check_nonfull_disables_index()
{
    memblock::DcacheReleaseIndex index;
    index.record(7, 6, 0x1000);
    index.record(7, 3, 0x1008);
    require(!index.line_count(0x1000, false).has_value(),
            "non-full release must disable the line index");
    require(!index.line_count(0x1000, true).has_value(),
            "non-full release must disable the data line index");
}

static void check_unaligned_query_disables_index()
{
    memblock::DcacheReleaseIndex index;
    index.record(6, 6, 0x1000);
    require(!index.line_count(0x1008, false).has_value(),
            "unaligned query must fall back to the range scan");
}

static void check_clear()
{
    memblock::DcacheReleaseIndex index;
    index.record(7, 6, 0x2000);
    index.record(7, 3, 0x2008);
    index.clear();
    require(!index.nonfull, "clear resets the non-full flag");
    require(index.lines.empty() && index.data_lines.empty(),
            "clear resets both counters");
    index.record(6, 6, 0x2000);
    require(index.line_count(0x2000, false).value_or(99) == 1,
            "record after clear");
}

int main(int argc, char **argv)
{
    const std::string name = argc > 1 ? argv[1] : "";
    if (name == "full-line") {
        check_full_line_counts();
    } else if (name == "nonfull") {
        check_nonfull_disables_index();
    } else if (name == "unaligned") {
        check_unaligned_query_disables_index();
    } else if (name == "clear") {
        check_clear();
    } else {
        std::cerr << "unknown case: " << name << '\n';
        return 2;
    }
    return 0;
}
