// #include <am.h>
// #include <klib.h>

# define DEPTH 100

int nest(int depth) {
    if (depth == 0) return depth;
    nest(depth - 1);
    return depth;
}

int main () {
    nest(DEPTH);
    return 0;
}
