
function List(value, next) {
  this.value = value;
  this.next = next || null;   // all values in next are >= value.
}

exports.arr = arr = function(list) {
  var o = [];
  while(list !== null) {
    o.push(list.value);
    list = list.next;
  }
  return o;
}

exports.search = function search(list, value) {
  var found = false;

  while(list !== null && list.value <= value) {
    if(list.value === value) {
      found = true;
    }
    list = list.next;
  }
  return found;
}

exports.insert = function insert(list, value) {
  var o;

  if(list === null || list.value > value) {
    o = new List(value, list);
  }

  else {
    if(list.value === value) {
      o = list;
    }

    else {
      // find latest node n such that
      // n.value < value.
      // Then if n.next exists and n.next.value == value, do nothing;
      // else splice in new node.

      var n = list;
      while(n.next !== null && n.next.value < value) {
        n = n.next;
      }

      if(n.next === null || n.next.value !== value) {
        // splice in new node
        n.next = new List(value, n.next);
      }

      o = list;
    }
  }

  return o;
}

exports.remove = function remove(list, value) {
  var o;

  if(list === null) {
    o = null;
  }

  else {
    if (list.value === value) {
      o = list.next;
    }

    else {
      // Find node n such that
      //   n !== null
      //   n.next !== null
      //   n.next.value === value
      // If we do, then
      //   n.next = n.next.next;

      var n = list;

      while (n.next !== null && n.next.value < value) {
        n = n.next;
      }
      // (n.next === null || n.next.value >= value)
      if (n.next !== null && n.next.value === value) {
        n.next = n.next.next;
      }

      o = list;
    }
  }

  return o;
}
