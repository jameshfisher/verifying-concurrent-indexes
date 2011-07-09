#ifndef MAIN_C_
#define MAIN_C_

//#define RECURSIVE
//#define PRINT_BLACK_HEIGHT

#include "./llrb.h"
#include "./test/llrb_print.c"
#include "./llrb.c"
#include "./test/test.c"


int main(int argc, char ** argv) {
  test(15, 15, true);

  return 0;
}

#endif  // MAIN_C_
