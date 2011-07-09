
var left = true;
var right = false;

var RED = 0;
var BLACK = 1;

function Node(value, left, right, parent, color) {
  this.value = value;
  this.c = {
    true: left || null,  // all values < value
    false: right || null // all values < value
  };
  this.parent = parent;
  this.color = color;
}

exports.Tree = function Tree() {
  this.nil = new Node(0, null, null, null, BLACK);
  this.root = this.nil;
}


//-----------------------------------------------------------------//
//                             printing                            //
//-----------------------------------------------------------------//

function map(a, f) {
  o = [];
  while(a) o.push(f(a.shift()));
  return o;
}




function nodeStr(tree, node) {
  if(node === tree.nil || node === null) return "";

  return "(" + nodeStr(tree, node.c[left]) + node.value + "," + (node.color ? "b" : "r") + nodeStr(tree, node.c[right]) + ")";
}

exports.str = function(tree) {
  return nodeStr(tree, tree.root);
}

function nodearr(node, tree) {
  if(node === tree.nil || node === null) {
    return [];
  }
  else {
    return nodearr(node.c[left], tree).concat([node.value]).concat(nodearr(node.c[right], tree));
  }
}

exports.arr = arr = function(tree) {
  return nodearr(tree.root, tree);
}


//-----------------------------------------------------------------//
//                             search                              //
//-----------------------------------------------------------------//

function searchNode(tree, value) {
  var node = tree.root;

  while(node !== tree.nil && node.value !== value) {
    node = node.c[value < node.value];
  }

  return node;
}


exports.search = function(tree, value) {
  return searchNode(tree, value) !== tree.nil;
}


//-----------------------------------------------------------------//
//                             insert                              //
//-----------------------------------------------------------------//

function rotate(tree, node, dir) {
  var y = node.c[!dir];
  node.c[!dir] = y.c[dir];

  if (y.c[dir] !== tree.nil) {
    y.c[dir].parent = node;
  }

  y.parent = node.parent;

  if (node.parent !== tree.nil) {
    tree.root = y;
  }
  else {
    if (node === node.parent.c[dir]) {
      node.parent.c[dir] = y;
    }
    else {
      node.parent.c[!dir] = y;
    }
  }

  y.c[dir] = node;
  node.parent = y;
}


function insertFixup(tree, node) {
  console.log("fixing up", node);
  // `node` is just inserted

  while (node.parent.color === RED) {
    var parent = node.parent;
    var gp = parent.parent;
    if(parent === gp.c[left]) {
      // parent is left child

      var uncle = gp.c[right];

      if (uncle.color === RED) {
        parent.color = BLACK;
        uncle.color = BLACK;
        gp.color = RED;
        node = gp;
      }
      else {
        // uncle.color !== RED
        // uncle.color === BLACK

        if (node === parent.c[right]) {
          // node is right child
          node = parent;
          rotate(tree, node, left);
        }

        parent.color = BLACK;
        gp.color = RED;
        rotate(tree, gp, right);
      }
    }
    else {
      // node.parent !== node.parent.parent.c[left]
      // node.parent === node.parent.parent.c[right]
      // node.parent is right child
      // do symmetrical case

      var uncle = parent.c[left];

      if (uncle.color === RED) {
        parent.color = BLACK;
        uncle.color = BLACK;
        gp.color = RED;
        node = gp;
      }
      else {
        // uncle.color !== RED
        // uncle.color === BLACK

        if (node === parent.c[left]) {
          // node is left child
          node = parent;
          rotate(tree, node, right);
        }

        parent.color = BLACK;
        gp.color = RED;
        rotate(tree, gp, left);
      }
    } // end symmetrical case
  } // end of while
  // node.parent.color !== RED
  tree.root.color = BLACK;
}


function insertNode(tree, node) {
  var par = tree.nil;
  var x = tree.root;

  // descend; `par` will be parent of position of `node`
  while (x !== tree.nil && x.value !== node.value) {
    // x !== tree.nil
    par = x;
    if (node.value < x.value) {
      x = x.c[left];
    }
    else {
      x = x.c[right];
    }
  }
  // x === tree.nil || x.value === node.value
  if(x !== tree.nil) {
    // x.value === node.value; do nothing
  }
  else {
    // x === tree.nil
    // node.value was not in tree

    node.parent = par;

    if (par === tree.nil) {
      tree.root = node;
    }
    else {
      if (node.value < par.value) {
        par.c[left] = node;
      }
      else {
        par.c[right] = node;
      }
    }
  }

  //insertFixup(tree, node);
}


exports.insert = function insert(tree, value) {
  var n = new Node(value, tree.nil, tree.nil, tree.nil, RED);
  insertNode(tree, n);
}


//-----------------------------------------------------------------//
//                             remove                              //
//-----------------------------------------------------------------//

function minimum(tree, node) {
  while(node.c[left] !== tree.nil) {
    node = node.c[left];
  }
  return node;
}


function successor(tree, node) {
  var o;
  if(node.c[right] !== tree.nil) {
    o = minimum(tree, node.c[right]);
  }
  else {
    o = node.parent;
    while (o !== tree.nil && node === o.c[right]) {
      node = o;
      o = o.parent;
    }
  }
  return o;
}


function removeNode(tree, node) {
  var rem;

  if (node.c[left] === tree.nil || node.c[right] === tree.nil ) {
    rem = node;
  }
  else {
    rem = successor(tree, node);
  }

  var child;

  if (rem.c[left] !== tree.nil) {
    child = rem.c[left];
  }
  else {
    child = rem.c[right];
  }

  child.parent = rem.parent;

  if (rem.parent === tree.nil) {
    tree.root = child;
  }
  else {
    if (rem === rem.parent.c[left]) {
      rem.parent.c[left] = child;
    }
    else {
      rem.parent.c[right] = child;
    }
  }

  if (rem !== node) {
    node.value = rem.value;
  }
  
  if (rem.color === BLACK) {
    // removeFixup(tree, child);
  }

  return rem;
}


exports.remove = function(tree, value) {
  var d = searchNode(tree, value);
  if(d !== tree.nil) removeNode(tree, d);
}
