module rb.insert.insert_aux;

import rb.node;
import rb.insert.balance;

Node* insert_aux(Node* root, int value) {
  Node* o;
  /// <e:or>
  ///   <e:fst>(<e:and>
  ///     <e:fst><e:pred name="BT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
  ///     <e:snd>black</e:snd>
  ///   </e:and>)</e:fst>
  ///   <e:snd>(<e:and>
  ///     <e:fst><e:pred name="RT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
  ///     <e:snd>¬black</e:snd>
  ///   </e:and>)</e:snd>
  /// </e:or>

  if (root == null) {
    /// <e:and>
    ///   <e:fst><e:pred name="EBT"><code>root</code>, <e:st n="S"/>, <e:var n="h"/></e:pred></e:fst>
    ///   <e:snd>black</e:snd>
    /// </e:and>
    o = new Node(value);
    /// <e:and>
    ///   <e:fst><e:pred name="RT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:fst>
    ///   <e:snd>black</e:snd>
    /// </e:and>

    // <m:orIntro/>.
    /// <e:or>
    ///   <e:fst>(<e:and>
    ///     <e:fst>(<e:or>
    ///       <e:fst><e:pred name="BT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:fst>
    ///       <e:snd><e:pred name="RT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:snd>
    ///     </e:or>)</e:fst>
    ///     <e:snd>black</e:snd>
    ///   </e:and>)</e:fst>
    ///   <e:snd><br/>(<e:and>
    ///     <e:fst>(<e:or>
    ///       <e:fst><e:pred name="RT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:fst>
    ///       <e:snd><e:pred name="RVT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:snd>
    ///     </e:or>)</e:fst>
    ///     <e:snd>¬black</e:snd>
    ///   </e:and>)</e:snd>
    /// </e:or>
  }
  else {
    if (root.value == value) {
      o = root;
    }
    else {
      int dir = root.value < value;
      root.c[dir] = insert_aux(root.c[dir], value);
      o =  balance(root, dir);
    }
  }
  /// <e:or>
  ///   <e:fst>(<e:and>
  ///     <e:fst>(<e:or>
  ///       <e:fst><e:pred name="BT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:fst>
  ///       <e:snd><e:pred name="RT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:snd>
  ///     </e:or>)</e:fst>
  ///     <e:snd>black</e:snd>
  ///   </e:and>)</e:fst>
  ///   <e:snd><br/>(<e:and>
  ///     <e:fst>(<e:or>
  ///       <e:fst><e:pred name="RT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:fst>
  ///       <e:snd><e:pred name="RVT"><code>o</code>, <e:union><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>, <e:var n="h"/></e:pred></e:snd>
  ///     </e:or>)</e:fst>
  ///     <e:snd>¬black</e:snd>
  ///   </e:and>)</e:snd>
  /// </e:or>

  return o;
}
