int f(int x) {
    int y = 0;

    if (x == 0) {
        y = x;
    } else if (x > 0) {
        y = x + 1;
    } else {
        y = x / 2;
    }

    return y;
}

