#ifndef LLRBPRINT_C_
#define LLRBPRINT_C_

#include <stdio.h>

#include "../llrb.h"

void llrb_ansi(char * mode) { printf("%c[%sm", 27, mode); }

void llrb_print_node(pLLRBNode node, int level) {
  if (node) {
    int nextLevel = level+1;

    llrb_print_node(node->right, nextLevel);

#ifdef PRINT_BLACK_HEIGHT
    if(!node->color) level--;
#endif
    while (level-- > 0) printf("  ");

    llrb_ansi(node->color ? "0" : "31");
    printf("%i\n", node->value);
    llrb_ansi("0");

    llrb_print_node(node->left, nextLevel);
  }
}

#endif  // LLRBPRINT_C_
