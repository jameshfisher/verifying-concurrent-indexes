module bst.search.recursive;

import bst.node;
import bst.descend;


bool search(Node* root, in int value) {
  bool o;
  // Function precondition.
  /// <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>

  if (root) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    bool eq = rootEq(root, value);
    // Specification for <code>rootEq</code>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
    ///     <e:snd><e:doubleimpl>
    ///       <e:fst><code>eq</code></e:fst>
    ///       <e:snd><e:eq>
    ///         <e:fst><e:var n="v"/></e:fst>
    ///         <e:snd><code>value</code></e:snd>
    ///       </e:eq></e:snd>
    ///     </e:doubleimpl></e:snd>
    ///   </e:and></e:snd>
    /// </e:exists>

    if (eq) {
      // Assert if-test.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ <code>eq</code>

      // Use <e:doubleimpl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq></e:snd>
      // </e:doubleimpl>.  Discard <code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Substitution.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>

      // Lemma: <e:logimpl>
      //   <e:fst><e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred></e:fst>
      //   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      // </e:logimpl>
      /// <e:and>
      ///   <e:fst><e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and>

      // Reintroduce <m:existsIntro/> on <code>value</code> as <e:var n="v"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Close <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>.
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and>

      o = true;
      // Assignment.
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      /// </e:and> ∧ <e:eq><e:fst><code>o</code></e:fst><e:snd><code>true</code></e:snd></e:eq>

      // ?
      /// <e:and>
      ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
      ///   <e:snd><e:doubleimpl>
      ///     <e:fst><code>o</code></e:fst>
      ///     <e:snd><e:in><e:fst><code>value</code></e:fst><e:snd><e:st n="S"/></e:snd></e:in></e:snd>
      ///   </e:doubleimpl></e:snd>
      /// </e:and>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ ¬<code>eq</code>

      // Use <e:doubleimpl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq></e:snd>
      // </e:doubleimpl>.  Discard ¬<code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:noteq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:noteq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      Node* next = descend(root, value);

      // Specification of <code>descend</code>.
      /// TopOfTree(root, v, S) ∧
      /// ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∧ |Q| &lt; |S|

      // Note recursion terminates because |Q| &lt; |S| and |Q| &gt;= 0
      o = search(next, value);

      // Specification of search.
      /// TopOfTree(root, v, S) ∧
      /// ∃Q. Tree(o, Q) ∧ (value∈Q ↔ value∈S)) ∧ |Q| &lt; |S| ∧ o ↔ (value ∈ Q)

      // Transitivity of ↔; escape ∃; weaken TopOfTree.
      /// Tree(root, S) ∧ o ↔ (value ∈ S)
    }
    // Transitivity of both if-branches.
    /// Tree(root, S) ∧ o ↔ (value ∈ S)
  }
  else {
    // Deny if-test.
    /// Tree(root, S) ∧ root=null

    // Null pointer is empty tree (lemma).
    /// EmptyTree(root, S)

    // Empty tree is empty set (lemma).
    /// EmptyTree(root, S) ∧ S = ∅

    // Element not in empty set.
    /// EmptyTree(root, S) ∧ value ∉ S

    o = false;

    // Assignment.
    /// EmptyTree(root, S) ∧ value ∉ S ∧ o = false

    // ???
    /// EmptyTree(root, S) ∧ o ↔ (value ∈ S)

    // Weaken EmptyTree.
    /// Tree(root, S) ∧ o ↔ (value ∈ S)
  }
  // Function postcondition.
  // Postcondition of both if-branches.
  /// Tree(root, S) ∧ o ↔ (value ∈ S)
  return o;
}
