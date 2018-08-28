void f(int *a, int x) {
    @Length(x)
    a[-1] = x;
}
