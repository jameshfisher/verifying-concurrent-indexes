(function() {

var debug = false;

var assert = require("assert");
var util = require("util");
var Set = require("./IterativeBinaryTreeSet");

var l = null;

var reference = {};

for(var i = 0; i < 5000; i++) {

  var val = Math.floor(Math.random()*20);

  if(Math.floor(Math.random()*2)) {
    if(debug) console.log("insert " + val);
    l = Set.insert(l, val);
    reference[val] = true;
  }
  else {
    if(debug) console.log("remove " + val);
    l = Set.remove(l, val);
    delete reference[val];
  }

  if(debug) {
    console.log(util.inspect(l, false, null));
    console.log(Set.arr(l));
    console.log(reference);
  }

  val = Math.floor(Math.random()*10);
  if(debug) console.log("search " + val);

  assert.equal(
    Set.search(l, val),
    reference[val] === true
    );
  if(debug) console.log("----");
}

}());
