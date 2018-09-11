int f(@Length(size) int* x, int size, int i) {
    for (int i = 0; i < size; i++) {
        if (x[i] % 2 == 0) {
            return 1;
        }
    }

    return 0;
}

