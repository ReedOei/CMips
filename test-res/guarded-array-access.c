int f(@Length(size) int* x, int size, int i) {
    if (i > 0 && i < size) {
        return x[i];
    }
}

