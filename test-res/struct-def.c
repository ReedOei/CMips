struct node_t {
    int * data; // pointer to an integer
    int test;
    node_t * next; // pointer to another node_t
};

int sum(node_t *node) {
    if (node == NULL) {
        return 0;
    }

    node->test = *(node->data) + sum(node->next);
    return node->test;
}

