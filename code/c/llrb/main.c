#include <stdio.h>

#include "./Node.h"
#include "./analyze.h"


int main(void) {
  TreeType t = mkValidTree(3, Red);
  printTreeType(t);
  return 0;
}
