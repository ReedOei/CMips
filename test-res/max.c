unsigned max(unsigned *array, unsigned size) {
    unsigned currentMax = array[0];
    for (unsigned i = 1; i < size; ++ i) {
        if (array[i] > currentMax) {
            currentMax = array[i];
        }
    }
    return currentMax;
}
