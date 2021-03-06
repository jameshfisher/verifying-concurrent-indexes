#ifndef TEST_C_
#define TEST_C_

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#include "../llrb.h"
#include "../llrb.c"

#include "./is_llrb.c"
#include "./llrb_print.c"

//#define INSERT_ONLY

void llrb_print_reference(bool * ref, int NUM_VALUES) {
  printf("{");
  for(int i = 0; i < NUM_VALUES; i++) if (ref[i]) printf("%i,", i);
  printf("}\n");
}


bool test(int NUM_VALUES, int NUM_TESTS, bool DEBUG) {

  srand((unsigned) time(NULL)); 

  pLLRBNode testing = NULL;

  bool reference[NUM_VALUES];
  for(int i = 0; i < NUM_VALUES; i++) reference[i] = false;

  for(int i = 0; i < NUM_TESTS; i++) {

    int value = rand() % NUM_VALUES;

#ifdef INSERT_ONLY
    if (DEBUG) printf("Inserting %i...", value);
    reference[value] = true;
    testing = llrb_insert(testing, value);
#else
    if(rand() % 2) { // insert
      if (DEBUG) printf("Inserting %i...", value);
      reference[value] = true;
      testing = llrb_insert(testing, value);
    }
    else { // delete
      if (DEBUG) printf("Deleting %i...", value);

      reference[value] = false;
      testing = llrb_del(testing, value);
    }
#endif

    value = rand() % NUM_VALUES;

    if (DEBUG) {
      printf("done.\n");
    }

    if (DEBUG) {
      printf("Reference: ");       llrb_print_reference(reference, NUM_VALUES);
      printf("Implementation:\n"); llrb_print_node(testing, 0);
    }

    LLRBNodeInfo info = llrbNodeInfo(testing);
    if (DEBUG) {
      llrb_print_node_info(info);
      if (info.t != BLACK_LLRB) printf(".  Abort.\n");
      else printf(".\n");
    }
    if (info.t != BLACK_LLRB) exit(0);

    if (DEBUG) {
      printf("Searching for %i...", value);
    }

    bool inReference =      reference[value];
    bool inImplementation = llrb_search(testing, value);

    if(inReference == inImplementation) {
      if (DEBUG) printf("OK.\n");
    }
    else {
      printf("discrepancy. reference[%i]=%i. testing[%i]=%i. Abort.\n", value, inReference, value, inImplementation);
      return false;
    }

    if (DEBUG) printf("----\n");
  }

  if (DEBUG) printf("Tests passed.\n");

  return true;
}

#endif  // TEST_C_
