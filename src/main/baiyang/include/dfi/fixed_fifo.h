#ifndef DFI_FIXED_FIFO_H_
#define DFI_FIXED_FIFO_H_

#include <array>
#include <cstddef>
#include <utility>

namespace dfi {

template <typename T, size_t Capacity>
class fixed_fifo {
public:
    bool empty() const { return size_ == 0U; }
    size_t size() const { return size_; }
    T& front() { return storage_[read_]; }
    const T& front() const { return storage_[read_]; }

    void push_back(const T& value) {
        storage_[write_] = value;
        write_ = next(write_);
        ++size_;
    }

    void push_back(T&& value) {
        storage_[write_] = std::move(value);
        write_ = next(write_);
        ++size_;
    }

    void pop_front() {
        read_ = next(read_);
        --size_;
    }

    void clear() {
        read_ = 0U;
        write_ = 0U;
        size_ = 0U;
    }

private:
    static size_t next(size_t index) {
        ++index;
        return index == Capacity ? 0U : index;
    }

    std::array<T, Capacity> storage_;
    size_t read_ = 0U;
    size_t write_ = 0U;
    size_t size_ = 0U;
};

}  // namespace dfi

#endif  // DFI_FIXED_FIFO_H_
