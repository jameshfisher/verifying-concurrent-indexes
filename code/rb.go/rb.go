package rb

import "fmt"

type Node struct {
  Value int
  Link [2]*Node
  Black bool
}

func makeNode(value int) *Node {
  o := new(Node);
  o.Value = value;
  return o;
}


type Tree struct {
  Root *Node
}

func MakeTree() *Tree {
  return new(Tree)
}


func red(node *Node) bool {
  return (node != nil && !node.Black)
}


func not(i int) int {
  if i == 0 {
    return 1
  }
  return 0
}


func toInt(b bool) int {
  if b {
    return 1
  }
  return 0
}


func Single(oldRoot *Node, dir int) *Node {
  // oldRoot != nil && oldRoot[!dir] != nil
  newRoot := oldRoot.Link[not(dir)]
  oldRoot.Link[not(dir)] = newRoot.Link[dir]
  newRoot.Link[dir] = oldRoot

  newRoot.Black = true
  oldRoot.Black = false

  return newRoot
}


func Double(oldRoot *Node, dir int) *Node {
  oldRoot.Link[not(dir)] = Single(oldRoot.Link[not(dir)], not(dir))
  return Single(oldRoot, dir)
}




func (root *Node) Insert(value int) *Node {
  if root == nil {
    root = makeNode(value)
  } else if value != root.Value {
    dir := toInt(value > root.Value)
    root.Link[dir] = root.Link[dir].Insert(value)

    // rebalance
    if red(root.Link[dir]) {
      if red(root.Link[not(dir)]) {
        root.Black = false
        root.Link[0].Black = true
        root.Link[1].Black = true
      } else {
        if red(root.Link[dir].Link[dir]) {
          root = Single(root, not(dir))
        } else if red(root.Link[dir].Link[not(dir)]) {
          root = Double(root, not(dir))
        }
      }
    }
  }
  return root
}

func (tree *Tree) Insert(value int) {
  tree.Root = tree.Root.Insert(value)
  tree.Root.Black = true
}

//func Insert_recursive(root *Node, value int) *Node {
  
//}



func ansi(mode string) {
  fmt.Printf("%c[%vm", 27, mode)
}


func rbPrint(node *Node, depth int, pre string) {
  if node != nil {
    rbPrint(node.Link[1], depth+1, "╭╴")
    for i := depth; i > 0; i -= 1 {
      fmt.Print("  ")
    }
    fmt.Print(pre)
    if node.Black {
      ansi("0")
    } else {
      ansi("31")
    }
    fmt.Println(node.Value)
    ansi("0")
    rbPrint(node.Link[0], depth+1, "╰╴")
  }
}


func (tree *Tree) TreePrint() {
  rbPrint(tree.Root, -1, "")
}

