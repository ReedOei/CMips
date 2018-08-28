unsigned int max(unsigned int *array, unsigned int size) {
    unsigned int currentMax = array[0];
    for (unsigned int i = 1; i < size; ++ i) {
        if (array[i] > currentMax) {
            currentMax = array[i];
        }
    }
    return currentMax;
}
