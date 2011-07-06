module test;

import index;

import std.c.stdio;
import std.stdio;

import std.random;

bool testAgainstReference(ReferenceIndex, ImplementationIndex)
  (int num_values, int num_tests, bool noisy, void function(ReferenceIndex) referencePrinter, void function(ImplementationIndex) implementationPrinter) {

  auto reference = new ReferenceIndex;
  auto implementation = new ImplementationIndex;

  for(int i = 0; i < num_tests; i++) {

    if (noisy) writeln("----");


    // DO AN INSERT OR REMOVE
    // ----------------------

    auto value = std.random.uniform(0, num_values);


    if(std.random.uniform(0, 2)) {
      if (noisy) writef("Inserting %d...", value);
      reference.insert(value);
      implementation.insert(value);
    }
    else {
      if (noisy) writef("Deleting %d...", value);
      reference.remove(value);
      implementation.remove(value);
    }
    if (noisy) writeln("done.");

    int[] referenceContents = reference.contents();
    int[] implementationContents = implementation.contents();

    if (noisy) {
      writeln("Reference:");
      referencePrinter(reference);
      writeln("Implementation:");
      implementationPrinter(implementation);
    }


    // SEE IF THE CONTENTS ARE THE SAME
    // --------------------------------

    if (referenceContents.length != implementationContents.length) {
      writefln("Number of elements differ.  Reference: %s, implementation: %s.  Stop.", referenceContents.length, implementationContents.length);
      return false;
    }
    else {
      for (int j = 0; j < referenceContents.length; j++) {
        if (referenceContents[j] != implementationContents[j]) {
          writefln("At index %s, reference contains %s, but implementation contains %s. Stop.", j, referenceContents[j], implementationContents[j]);
          return false;
        }
      }
    }


    // TEST A SEARCH
    // -------------

    int test_search = std.random.uniform(0, num_values);

    if (noisy) {
      writef("Searching for %d...", test_search);
    }

    bool inReference =      reference.search(test_search);
    bool inImplementation = implementation.search(test_search);

    if (inReference == inImplementation) {
      if (noisy) writeln("OK.");
    }
    else {
      writefln("discrepancy.  reference[%s] == %s.  implementation[%s] == %s.  Stop.", test_search, inReference, test_search, inImplementation);
      return false;
    }

    if (noisy) writeln("----\n");
  }

  writeln("Tests passed.");
  return true;
}
