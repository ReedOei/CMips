int test(int y);

int f(int x) {
    int n = 4;

    while (n < 5) {
        n = n + 1;
    }

    if (n == 0) {
        return n + 2;
    }

    return n + 2 + 6 - 4 % x;
}

