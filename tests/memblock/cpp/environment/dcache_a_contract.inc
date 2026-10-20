// Included inside memblock_env.hpp's memblock namespace. No DUT dependency.
struct DcacheARequest {
    std::uint8_t opcode = 0;
    std::uint8_t param = 0;
    std::uint8_t size = 0;
    std::uint8_t source = 0;
    std::uint64_t address = 0;
    bool keyword = false;
    std::uint8_t request_source = 0;
    // Raw A.user.vaddr: the producer's virtual index/address shifted by 6.
    // Hardware prefetch supplies only alias bits and the physical page offset.
    std::uint64_t virtual_address = 0;
    std::uint8_t alias = 0;
    bool need_hint = false;
    std::uint32_t mask = 0;
    bool corrupt = false;
    std::uint64_t cycle = 0;
    std::uint64_t index = 0;
};

inline const char *dcache_a_protocol_error(
    const DcacheARequest &request, unsigned miss_entries, unsigned cmo_source,
    unsigned source_count)
{
    const bool acquire = request.opcode == 6 || request.opcode == 7;
    const bool cmo = request.opcode >= 12 && request.opcode <= 14;
    if (request.opcode != 4 && !acquire && !cmo) {
        return "unsupported DCache TileLink A opcode";
    }
    // Check before any shift or mask calculation, including malformed records.
    if (request.size > 6) {
        return "oversized DCache TileLink A request";
    }
    if (request.corrupt) {
        return "DCache TileLink A request is corrupt";
    }
    if (cmo) {
        // CacheBlockOperation leaves param/user/mask/data/echo unspecified.
        if (request.size != 6 || request.source != cmo_source ||
            (request.address & 63U) != 0) {
            return "malformed DCache CMO request identity";
        }
        return nullptr;
    }
    if (acquire) {
        if (request.size != 6) {
            return "invalid DCache Acquire size";
        }
        if ((request.address & 63U) != 0) {
            return "unaligned DCache Acquire address";
        }
        if (request.source >= miss_entries) {
            return "invalid DCache Acquire source";
        }
        if (request.param > 2 ||
            (request.opcode == 7 && request.param == 0)) {
            return "invalid DCache Acquire grow param";
        }
        if (request.mask != std::uint32_t{0xffffffffU}) {
            return "invalid DCache Acquire mask";
        }
        if (request.request_source != 2 && request.request_source != 3 &&
            request.request_source != 4 && request.request_source != 6) {
            return "invalid DCache Acquire request source";
        }
        if (request.alias != ((request.virtual_address >> 6) & 3U)) {
            return "DCache Acquire alias disagrees with virtual address";
        }
        if (((request.address >> 6) & 63U) !=
            (request.virtual_address & 63U)) {
            return "DCache Acquire page offset disagrees with virtual address";
        }
        return nullptr;
    }
    if (request.source >= source_count) {
        return "invalid DCache Get source";
    }
    // Generic Get has no user-field contract on this interface. A.data is
    // irrelevant for all accepted opcodes; no fixed data/keyword/hint value.
    if (request.param != 0) {
        return "invalid DCache Get param";
    }
    const std::uint64_t bytes = std::uint64_t{1} << request.size;
    if ((request.address & (bytes - 1U)) != 0) {
        return "unaligned DCache Get address";
    }
    const std::uint32_t expected_mask = request.size >= 5
        ? std::uint32_t{0xffffffffU}
        : static_cast<std::uint32_t>(
              ((std::uint64_t{1} << bytes) - 1U) << (request.address & 31U));
    if (request.mask != expected_mask) {
        return "invalid DCache Get mask";
    }
    return nullptr;
}
