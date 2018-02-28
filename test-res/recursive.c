int mySum(int n, int k) {
    if (n == k) {
        return 0;
    }

    return n + mySum(n + 1, k);
}

