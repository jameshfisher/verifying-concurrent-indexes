package main

import "fmt"
import "./rb"

type NodeType int
const (
  BLACK_TREE = iota
  RED_TREE
  INVALID_TREE
)

func printNodeType(n NodeType) {
  if n == BLACK_TREE {
    fmt.Print("Black tree")
  } else if n == RED_TREE {
    fmt.Print("Red tree")
  } else {
    fmt.Print("Invalid tree")
  }
}

func rbInfo(node *rb.Node) (NodeType, int) {
  if node == nil {
    return BLACK_TREE, 1
  }

  leftType, leftHeight := rbInfo(node.Link[0])
  rightType, rightHeight := rbInfo(node.Link[1])

  if leftType == INVALID_TREE || rightType == INVALID_TREE || leftHeight != rightHeight || (!node.Black && (leftType == RED_TREE || rightType == RED_TREE)) {
    return INVALID_TREE, 0
  }

  if node.Black {
    return BLACK_TREE, leftHeight + 1
  }
  return RED_TREE, leftHeight
}



func main() {
  root := rb.MakeTree()
  for i := 0; i < 75; i += 1 {
    root.Insert(i)
  }

  root.TreePrint()

  treeType, height := rbInfo(root.Root)
  printNodeType(treeType)
  fmt.Printf(", %v\n", height)
}
