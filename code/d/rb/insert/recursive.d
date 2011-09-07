module rb.insert.recursive;

import rb.node;
import rb.blacken;
import rb.insert.balance;

Node* insert_aux(Node* root, int w) {
  if (root == null) return new Node(w);
  else if (root.value == w) return root;
  int dir = root.value < w;
  root.c[dir] = insert_aux(root.c[dir], w);
  return balance(root, dir);
}


Node* insert(Node* root, int value) {
  // Function precondition.
  /// <e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred>

  Node* i = insert_aux(root, value);
  // Specification for <code>insert_aux</code>.
  /// <e:or>
  ///   <e:fst><e:pred name="RBT"><code>i</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>, <e:var n="h"/></e:fst>
  ///   <e:snd><e:pred name="RVT"><code>i</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>, <e:var n="h"/></e:snd>
  /// </e:or>

  Node* o = blacken(i);
  // Specification for <code>blacken</code>.
  /// <e:exists>
  ///   <e:fst><e:var n="nh"/></e:fst>
  ///   <e:snd><e:and>
  ///     <e:fst><e:pred name="BT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>, <e:var n="nh"/></e:fst>
  ///     <e:snd><e:in>
  ///       <e:fst><e:var n="nh"/></e:fst>
  ///       <e:snd><e:set>
  ///         <e:var n="h"/>,
  ///         <e:plus><e:fst><e:var n="h"/></e:fst><e:snd><e:const n="1"/></e:snd></e:plus>
  ///       </e:set></e:snd>
  ///     </e:in></e:snd>
  ///   </e:and></e:snd>
  /// </e:exists>

  return o;
}