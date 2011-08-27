module ll.node;

struct Node {
  int value;   // The lowest value in the set
  Node* tail;  // list containing all values greater than `value`

  this(int value) {
    this.value = value;  // The tail pointer is initialized to null
  }

  this(int value, Node* tail) {
    this.value = value;
    this.tail = tail;
  }
}