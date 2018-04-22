struct node_t {
    int * data; // pointer to an integer
    int test;
    node_t * next; // pointer to another node_t
};

int sum(node_t *node) {
    if (node == NULL) {
        return 0;
    }

    return node->data + sum(node->next);
}

struct temp_t {
    int a;
    int t[4];
};

int f(temp_t *v, int *xs) {
    return &(v->t[3]) + &(xs[4]);
}

