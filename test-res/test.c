int square(int x) {
    return x*x;
}

int f(int *x) {
    int total = 0;
    int i = 0;

    for (int i = 0; i < total; i++) {
        total += square(i);
    }

    return total;
}

