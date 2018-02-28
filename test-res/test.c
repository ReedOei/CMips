int square(int x) {
    return x*x;
}

int f(int *x) {
    int i = 0;
    int test = 0;

    for (int i = 0; i < 10; i++) {
        x[i] = square(i);
        test += x[i];
    }

    return i;
}

