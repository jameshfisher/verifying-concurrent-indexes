
function List(value, next) {
  this.value = value;
  this.next = next || null;   // all values in next are >= value.
}

exports.arr = arr = function(list) {
  if(list === null) {
    return [];
  } else {
    return [list.value].concat(arr(list.next));
  }
}

exports.search = function search(list, value) {
  var o;
  if(list === null) {
    o = false;
  }
  else {
    var v = list.value;
    if(v === value) {
       o = true;
    }
    else {
      o = search(list.next, value);
    }
  }
  return o;
}

exports.insert = function insert(list, value) {
  // List(list, L)
  var n;
  // List(list, L)
  if(list === null) {
    // L = {}
    n = new List(value, null);
    // List(n,      {value})
    // List(n, {} u {value})
    // List(n, L  u {value})
  }
  else {
    // list->v,next * List(list.value, N) and L = {v} u N
    var v = list.value;
    // list is {v} + next.
    // all values in next are >= v.
    if(v === value) {
      // List(list, L u {value})
      n = list;
      // List(n, L u {value})
    }
    else {
      // value != v
      // all values in next are > v.
      // value must be > v to be in list already
      if(value > v) {
        // value > v
        n = new List(v, insert(list.next, value));
        // List(n, L u {value})
      } else {
        // value < v
        n = new List(value, list);
        // List(n, L u {value})
      }
      // List(n, L u {value})
    }
  }
  // List(n, L u {value})
  return n;
}

exports.remove = function remove(list, value) {
  var n;
  if(list === null) {
    // value is not in list
    n = list;
  }
  else {
    var v = list.value;
    if(v === value) {
      n = list.next;
    }
    else {
      n = new List(v, remove(list.next, value));
    }
  }
  return n;
}
