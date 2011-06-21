#ifndef TEST_C_
#define TEST_C_

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#include "./bst.h"
#include "./bst.c"
#include "./is_llrb.c"
#include "./bstPrint.c"

#define INSERT_ONLY

void printReference(bool * ref, int NUM_VALUES) {
  printf("{");
  for(int i = 0; i < NUM_VALUES; i++) if (ref[i]) printf("%i,", i);
  printf("}\n");
}


bool test(int NUM_VALUES, int NUM_TESTS, bool DEBUG) {

  srand((unsigned) time(NULL)); 

  pNode testing = NULL;

  bool reference[NUM_VALUES];
  for(int i = 0; i < NUM_VALUES; i++) reference[i] = false;

  for(int i = 0; i < NUM_TESTS; i++) {

    int value = rand() % NUM_VALUES;

#ifdef INSERT_ONLY
    if (DEBUG) printf("Inserting %i...", value);
    reference[value] = true;
    testing = insert(testing, value);
#else
    if(rand() % 2) { // insert
      if (DEBUG) printf("Inserting %i...", value);
      reference[value] = true;
      testing = insert(testing, value);
    }
    else { // delete
      if (DEBUG) printf("Deleting %i...", value);

      reference[value] = false;
      testing = del(testing, value);
    }
#endif

    value = rand() % NUM_VALUES;

    if (DEBUG) {
      printf("done.\n");
    }

    if (DEBUG) {
      printf("Reference: ");       printReference(reference, NUM_VALUES);
      printf("Implementation:\n"); printNode(testing, 0);
    }

    LLRBNodeInfo info = llrbNodeInfo(testing);
    if (info.t == BLACK_LLRB) {
      if (DEBUG) {
        printf("Valid LLRB; height %d\n", info.h);
      }
    }
    else {
      printf("Invalid LLRB: ");
      printLLRBNodeInfo(info);
      printf("\n");
      return false;
    }

    if (DEBUG) {
      printf("Searching for %i...", value);
    }

    bool inReference =      reference[value];
    bool inImplementation = search(testing, value);

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
