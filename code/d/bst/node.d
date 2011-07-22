module bst.node;

struct Node {
  int value;   // One value in the set held in this subtree
  Node*[2] c;  // Pointers to the two subtrees (0 is left, 1 is right)

  this(int value) {
    this.value = value;  // The subtree pointers are initialized to null.
  }
}