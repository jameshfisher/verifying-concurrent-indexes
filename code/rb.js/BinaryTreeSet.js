
function Node(value, left, right) {
  this.value = value;
  this.left = left || null;   // all values < value
  this.right = right || null;   // all values < value
}


exports.arr = arr = function(node) {
  if(node === null) {
    return [];
  } else {
    return arr(node.left).concat([node.value]).concat(arr(node.right));
  }
}


exports.search = function search(node, value) {
  var o;

  if(node === null) {
    o = false;
  }
  else {
    if(value === node.value) {
      o = true;
    }
    else {
      if(value < node.value) {
        o = search(node.left, value);
      }
      else {
        o = search(node.right, value);
      }
    }
  }

  return o;
}


exports.insert = function insert(node, value) {
  var o;

  if(node === null) {
    o = new Node(value, null, null);
  }
  else {
    if(value === node.value) {
      o = node;
    } else {
      if(value < node.value) {
        node.left = insert(node.left, value);
      }
      else {
        node.right = insert(node.right, value);
      }
      o = node;
    }
  }

  return o;
}


function removeMax(node) {
  // node != null; node.right != null
  if(node.right.right === null) {
    var o = node.right.value;
    node.right = node.right.left;
    return o;
  } else {
    return removeMax(node.right);
  }
}


exports.remove = function remove(node, value) {
  var o;

  if(node === null) {
    o = null;
  }
  else {
    if(node.value === value) {
      if(node.left == null && node.right == null) {
        o = null;
      }
      else {
        if(node.left == null) {
          o = node.right;
        }
        else {
          if(node.right == null) {
            o = node.left;
          }
          else {
            if(node.left.right === null) {
              node.value = node.left.value;
              node.left = node.left.left;
            }
            else {
              node.value = removeMax(node.left);
            }
            o = node;
          }
        }
      }
    }
    else {
      if(value < node.value) {
        node.left = remove(node.left, value);
      }
      else {
        node.right = remove(node.right, value);
      }
      o = node;
    }
  }

  return o;
}
