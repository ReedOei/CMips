int mySum(int n, int k) {
    if (n == k) {
        return n;
    }

    return n + mySum(n + 1, k);
}

