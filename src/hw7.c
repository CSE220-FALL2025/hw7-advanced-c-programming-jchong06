#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (!root){
        bst_sf *new_node = malloc(sizeof(bst_sf));
        new_node->mat = mat;
        new_node->left_child = NULL;
        new_node->right_child = NULL;
        return new_node;
    }
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else if (mat->name > root->mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    //printf("inserted %c\n", mat->name);
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (!root) {
        return NULL;
    }
    if (name < root -> mat -> name){
        return find_bst_sf(name, root ->left_child);
    }
    else if (name > root -> mat -> name){
        return find_bst_sf(name, root -> right_child);
    }
    else {
        return root -> mat;
    }
}

void free_bst_sf(bst_sf *root) {
    if (!root) {
        return;
    }
    free_bst_sf(root -> left_child);
    free_bst_sf(root -> right_child);
    free(root -> mat);
    free(root);
}

matrix_sf *add_mats_sf(const matrix_sf *m1, const matrix_sf *m2) {
    unsigned int rows = m1 -> num_rows;
    unsigned int cols = m1 -> num_cols;
    matrix_sf *sum = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    if (!sum) {
        return NULL;
    }
    sum -> name = '#'; //temporary name for temporary matrix
    sum -> num_rows = rows;
    sum -> num_cols = cols;

    for (unsigned int i = 0; i < rows * cols; i++) {
        sum -> values[i] = m1 -> values[i] + m2 -> values[i];
    }
    return sum;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int r = mat1 -> num_rows;
    unsigned int k = mat1 -> num_cols;
    unsigned int c = mat2 -> num_cols;
    matrix_sf *product = malloc(sizeof(matrix_sf) + r * c * sizeof(int));
    if (!product) {
        return NULL;
    }
    product -> name = '#'; //temporary name for temporary matrix
    product -> num_rows = r;
    product -> num_cols = c;
    for (unsigned int i = 0; i < r; i++) {
        for (unsigned int j = 0; j < c; j++) {
            product -> values[i * c + j] = 0;
            for (unsigned int p = 0; p < k; p++) {
                product -> values[i * c + j] += mat1 -> values[i * k + p] * mat2 -> values[p * c + j];
            }
        }
    }
    return product;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned int rows = mat -> num_rows;
    unsigned int cols = mat -> num_cols;
    matrix_sf *transpose = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    if (!transpose) {
        return NULL;
    }

    transpose -> name = '#'; //temporary name for temporary matrix
    transpose -> num_rows = cols;
    transpose -> num_cols = rows;
    for (unsigned int i = 0; i < rows; i++) {
        for (unsigned int j = 0; j < cols; j++) {
            transpose -> values[j * rows + i] = mat -> values[i * cols + j];
        }
    }
    return transpose;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *ptr = expr;
    char *endptr;

    while (*ptr == ' ' || *ptr == '\t') {
        ptr++;
    }

    long numRows = strtol(ptr, &endptr, 10);
    ptr = endptr;
    while (*ptr == ' ' || *ptr == '\t') {
        ptr++;
    }
    long numCols = strtol(ptr, &endptr, 10);
    ptr = endptr;

    while (*ptr != '\0' && *ptr != '[') {
        ptr++;
    }
    if (*ptr == '[') {
        ptr++;
    }
    matrix_sf *m = malloc(sizeof(matrix_sf) + numRows * numCols * sizeof(int));
    if (m == NULL) {
        return NULL;
    }

    m -> name = name;
    m -> num_rows = (unsigned int)numRows;
    m -> num_cols = (unsigned int)numCols;

    unsigned int count = 0;
    unsigned int total = m-> num_rows * m->num_cols;

    while (count < total && *ptr != '\0') {
        while (*ptr == ' ' || *ptr == '\t' || *ptr == ';') {
            ptr++;
        }
        long val = strtol(ptr, &endptr, 10);
        m->values[count] = (int) val;
        count++;
        ptr = endptr;
    }
    return m;
}

static int prec(char op) {
    if (op == '\'') {
        return 3;
    } else if (op == '*') {
        return 2;
    } else if (op == '+') {
        return 1;
    }
    return 0;
}

char* infix2postfix_sf(char *infix) {
    size_t len = strlen(infix);
    char *out = malloc(len + 1);
    if (!out) {
        return NULL;
    }
    char stack[256];
    int top = -1;
    int outI = 0;

    for (size_t i = 0; i < len; i++) {
        char ch = infix[i];
        if (ch == ' ' || ch == '\t' || ch == '\n') {
            continue;
        }
        if (ch >= 'A' && ch <= 'Z') {
            out[outI] = ch;
            outI++;
        }
        else if (ch == '(') {
            top++;
            stack[top] = ch;
        }
        else if (ch == ')') {
            while (top >= 0 && stack[top] != '(') {
                out[outI] = stack[top];
                outI++;
                top--;
            }
            if (top >= 0 && stack[top] == '(') {
                top--;
            }
        }
        else {
            int thisPrec = prec(ch);
            while (top >= 0 && stack[top] != '(' &&
                   prec(stack[top]) >= thisPrec) {
                out[outI] = stack[top];
                outI++;
                top--;
            }
            top++;
            stack[top] = ch;
        }
    }
    while (top >= 0) {
        if (stack[top] != '(') {
            out[outI] = stack[top];
            outI++;
        }
        top--;
    }
    out[outI] = '\0';
    // printf("postfix = %s\n", out);
    return out;
}

static int isTemp(const matrix_sf *m) {
    return !(m->name >= 'A' && m->name <= 'Z');
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    if (!postfix) {
        return NULL;
    }

    matrix_sf *stack[256];
    matrix_sf **sp = stack;

    for (char *p = postfix; *p != '\0'; p++) {
        char t = *p;

        if (t == ' ' || t == '\t') {
            continue;
        }

        if (t >= 'A' && t <= 'Z') {
            matrix_sf *foundMatrix = find_bst_sf(t, root);
            *sp = foundMatrix;
            sp++;
        }
        else if (t == '\'') {
            sp--;
            matrix_sf *topMatrix = *sp;

            matrix_sf *transposedMatrix = transpose_mat_sf(topMatrix);

            if (isTemp(topMatrix)) {
                free(topMatrix);
            }

            *sp = transposedMatrix;
            sp++;
        }
        else if (t == '+' || t == '*') {
            sp--;
            matrix_sf *rightMatrix = *sp;
            sp--;
            matrix_sf *leftMatrix = *sp;

            matrix_sf *resultMatrix;
            if (t == '+') {
                resultMatrix = add_mats_sf(leftMatrix, rightMatrix);
            } else {
                resultMatrix = mult_mats_sf(leftMatrix, rightMatrix);
            }

            if (isTemp(leftMatrix)) {
                free(leftMatrix);
            }
            if (isTemp(rightMatrix)) {
                free(rightMatrix);
            }

            *sp = resultMatrix;
            sp++;
        }
    }

    free(postfix);

    sp--;
    matrix_sf *result = *sp;
    result->name = name;
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        return NULL;
    }

    bst_sf *root = NULL;
    char *line = NULL;
    size_t lineCap = 0;
    matrix_sf *last = NULL;

    while (getline(&line, &lineCap, fp) != -1) {
        char *curr = line;
        while (*curr == ' ' || *curr == '\t') {
            curr++;
        }
        if (*curr == '\0' || *curr == '\n') {
            continue;
        }
        char matrixName = *curr;
        while (*curr != '\0' && *curr != '=') {
            curr++;
        }
        if (*curr == '=') {
            curr++;
        }
        if (strchr(curr, '[') != NULL) {
            matrix_sf *new = create_matrix_sf(matrixName, curr);
            root = insert_bst_sf(new, root);
            last = new;
        } else {
            matrix_sf *new = evaluate_expr_sf(matrixName, curr, root);
            root = insert_bst_sf(new, root);
            last = new;
        }
    }
    free(line);
    fclose(fp);
    return last;
}



// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
