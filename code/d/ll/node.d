module ll.node;

struct Node {
  int value;   // The lowest value in the set
  Node* tail;  // list containing all values greater than `value`
}