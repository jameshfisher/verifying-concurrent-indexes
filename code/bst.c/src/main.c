#ifndef MAIN_C_
#define MAIN_C_

//#define RECURSIVE
//#define PRINT_BLACK_HEIGHT

#include "./llrb.h"
#include "./test/bstPrint.c"
#include "./llrb.c"
#include "./test/test.c"


int main(int argc, char ** argv) {
  test(100, 50, true);

  return 0;
}

#endif  // MAIN_C_
