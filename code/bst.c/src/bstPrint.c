#ifndef BSTPRINT_C_
#define BSTPRINT_C_

#include <stdio.h>

#include "./bst.h"

void ansi(char * mode) { printf("%c[%sm", 27, mode); }

void printNode(pNode node, int level) {
  if (node) {
    int nextLevel = level+1;

    printNode(node->right, nextLevel);

#ifdef PRINT_BLACK_HEIGHT
    if(!node->color) level--;
#endif
    while (level-- > 0) printf("  ");

    ansi(node->color ? "30" : "31");
    printf("%i\n", node->value);
    ansi("0");

    printNode(node->left, nextLevel);
  }
}

#endif  // BSTPRINT_C_
