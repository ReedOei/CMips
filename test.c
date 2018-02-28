int f(int x) {
    int n = 4;

    for (int i = 0; i < 10; i++) {
        n += i;
    }

    return n + 2 + 6 - 4 % x;
}

