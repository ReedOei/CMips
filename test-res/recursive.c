int mySum(int n, int k) {
    if (n == k) {
        return n;
    }

    return n + mySum(n + 1, k);
}

int fib(int n) {
    if (n <= 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

