#ifndef NODE_H_
#define NODE_H_

#include <stdbool.h>

typedef enum { Red, Black } Color;

typedef struct Node {
  Color color;
  int value;
  struct Node * left;
  struct Node * right;
} Node;

#endif
