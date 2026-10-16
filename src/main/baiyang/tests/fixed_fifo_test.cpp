#include "dfi/fixed_fifo.h"

#include <iostream>
#include <stdexcept>

namespace {

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

}  // namespace

int main() {
    try {
        dfi::fixed_fifo<int, 4> fifo;
        check(fifo.empty(), "new FIFO is not empty");
        for (int value = 0; value < 4; ++value) {
            fifo.push_back(value);
        }
        check(fifo.size() == 4U, "FIFO did not reach full capacity");
        for (int value = 0; value < 2; ++value) {
            check(fifo.front() == value, "FIFO order mismatch before wrap");
            fifo.pop_front();
        }
        fifo.push_back(4);
        fifo.push_back(5);
        for (int value = 2; value < 6; ++value) {
            check(fifo.front() == value, "FIFO order mismatch after wrap");
            fifo.pop_front();
        }
        check(fifo.empty(), "drained FIFO is not empty");
        fifo.push_back(7);
        fifo.clear();
        check(fifo.empty(), "clear did not reset FIFO");
        std::cout << "fixed_fifo_test: PASS\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "fixed_fifo_test: FAIL: " << error.what() << '\n';
        return 1;
    }
}
