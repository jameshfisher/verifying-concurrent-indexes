#ifndef ANALYZE_H_
#define ANALYZE_H_

#include "./Node.h"

typedef int Height;

// data TreeType = ValidTree Color Height
//               | InvalidTree

// Data constructors
typedef struct ValidTree {
  Height height;
  Color color;
} ValidTree;

typedef struct InvalidTree {
} InvalidTree;


// Type constructor
typedef union ValidOrInvalidTree {
  ValidTree * validTree;
  InvalidTree * invalidTree;
} ValidOrInvalidTree;

typedef enum { ValidTreeT, InvalidTreeT } TreeTypeT;
typedef struct TreeType {
  TreeTypeT type;
  ValidOrInvalidTree tree;
} TreeType;


// Function declarations

typedef void * func_ValidTree(  void *, ValidTree   *);
typedef void * func_InvalidTree(void *, InvalidTree *);

void * match_TreeType(TreeType, void *, func_ValidTree*, func_InvalidTree*);

TreeType mkInvalidTree();
TreeType mkValidTree(Height height, Color color);

void printTreeType(TreeType);

TreeType analyze(Node *);



#endif
