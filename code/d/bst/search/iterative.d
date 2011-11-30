module bst.search.iterative;

import bst.node;
import bst.descend;


bool search(Node* root, in int value) {
  // Function precondition.
  /// <e:pred name="UnboundedTree"><code>root</code>, <e:st n="S"/></e:pred>

  // Open <e:pred name="UnboundedTree"><code>root</code>, <e:st n="S"/></e:pred>.
  /// <e:pred name="BoundedTree"><code>root</code><e:st n="S"/><m:neginf/><m:inf/></e:pred>

  // Introduce empty context.
  ///   <e:sep>
  ///     <e:pred name="EmptyContext"><code>root</code><m:empty/><m:neginf/><m:inf/><code>root</code></e:pred>
  ///     <e:pred name="BoundedTree"><code>root</code><e:st n="S"/><m:neginf/><m:inf/></e:pred>
  ///   </e:sep>

  Node* i = root;
  bool found = false;
  // Assignment.
  /// <e:and type="lines">
  ///   <e:sep>
  ///     <e:pred name="EmptyContext"><code>root</code><m:empty/><m:neginf/><m:inf/><code>i</code></e:pred>
  ///     <e:pred name="BoundedTree"><code>i</code><e:st n="S"/><m:neginf/><m:inf/></e:pred>
  ///   </e:sep>
  ///   <e:eq><code>found</code><code>false</code></e:eq>
  /// </e:and>

  // ??
  /// <e:and type="lines">
  ///   <e:sep>
  ///     <e:pred name="EmptyContext"><code>root</code><m:empty/><m:neginf/><m:inf/><code>i</code></e:pred>
  ///     <e:pred name="BoundedTree"><code>i</code><e:st n="S"/><m:neginf/><m:inf/></e:pred>
  ///   </e:sep>
  ///   <e:eq><code>found</code><code>false</code></e:eq>
  ///   <e:notin><e:var n="v"/><m:empty/></e:notin>
  /// </e:and>

  // Introduce existential quantification on <m:empty/>, <e:st n="S"/>, <m:neginf/>, and <m:inf/>.<br />
  // <e:logimpl><e:predicate>EmptyContext</e:predicate><e:predicate>Context</e:predicate></e:logimpl>.
  /// <e:exists>
  ///   <e:vars><e:st n="C"/><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:and type="lines">
  ///       <e:and>
  ///         <e:eq><e:union><e:st n="C"/><e:st n="T"/></e:union><e:st n="S"/></e:eq>
  ///         <e:notin><e:var n="v"/><e:st n="C"/></e:notin>
  ///       </e:and>
  ///       <e:impl><code>found</code><e:in><e:var n="v"/><e:st n="S"/></e:in></e:impl>
  ///       <e:sep>
  ///         <e:pred name="Context"><code>root</code><e:st n="C"/><e:var n="lb"/><e:var n="hb"/><code>i</code></e:pred>
  ///         <e:pred name="BoundedTree"><code>i</code><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
  ///       </e:sep>
  ///     </e:and>
  ///   </e:indent></e:expr>
  /// </e:exists>

  while (i != null && found == false) {
    // Loop invariant, and assert while-condition.
    /// <e:exists>
    ///   <e:vars><e:st n="C"/><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:and type="lines">
    ///       <e:and>
    ///         <e:eq><e:union><e:st n="C"/><e:st n="T"/></e:union><e:st n="S"/></e:eq>
    ///         <e:notin><e:var n="v"/><e:st n="C"/></e:notin>
    ///       </e:and>
    ///       <e:and>
    ///         <e:impl><code>found</code><e:in><e:var n="v"/><e:st n="S"/></e:in></e:impl>
    ///         <e:eq><code>found</code><code>false</code></e:eq>
    ///       </e:and>
    ///       <e:sep>
    ///         <e:pred name="Context"><code>root</code><e:st n="C"/><e:var n="lb"/><e:var n="hb"/><code>i</code></e:pred>
    ///         <e:pred name="BoundedTree"><code>i</code><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
    ///       </e:sep>
    ///       <e:noteq><code>i</code><m:null/></e:noteq>
    ///     </e:and>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // <e:logimpl>
    //   <e:and>
    //     <e:pred name="BoundedTree"><code>i</code><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
    //     <e:noteq><code>i</code><m:null/></e:noteq>
    //   </e:and>
    //   <e:pred name="NonEmptyBoundedTree"><code>i</code><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
    // </e:logimpl>.
    /// <e:exists>
    ///   <e:vars><e:st n="C"/><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:and type="lines">
    ///       <e:and>
    ///         <e:eq><e:union><e:st n="C"/><e:st n="T"/></e:union><e:st n="S"/></e:eq>
    ///         <e:notin><e:var n="v"/><e:st n="C"/></e:notin>
    ///       </e:and>
    ///       <e:and>
    ///         <e:impl><code>found</code><e:in><e:var n="v"/><e:st n="S"/></e:in></e:impl>
    ///         <e:eq><code>found</code><code>false</code></e:eq>
    ///       </e:and>
    ///       <e:sep>
    ///         <e:pred name="Context"><code>root</code><e:st n="C"/><e:var n="lb"/><e:var n="hb"/><code>i</code></e:pred>
    ///         <e:pred name="NonEmptyBoundedTree"><code>i</code><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
    ///       </e:sep>
    ///     </e:and>
    ///   </e:indent></e:expr>
    /// </e:exists>

    bool eq = rootEq(i, value);
    // Spec
    /// <e:exists>
    ///   <e:vars><e:st n="C"/><e:st n="T"/><e:var n="lb"/><e:var n="hb"/><e:var n="t"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:and type="lines">
    ///       <e:and>
    ///         <e:eq><e:union><e:st n="C"/><e:st n="T"/></e:union><e:st n="S"/></e:eq>
    ///         <e:notin><e:var n="v"/><e:st n="C"/></e:notin>
    ///       </e:and>
    ///       <e:and>
    ///         <e:impl><code>found</code><e:in><e:var n="v"/><e:st n="S"/></e:in></e:impl>
    ///         <e:eq><code>found</code><code>false</code></e:eq>
    ///       </e:and>
    ///       <e:sep>
    ///         <e:pred name="Context"><code>root</code><e:st n="C"/><e:var n="lb"/><e:var n="hb"/><code>i</code></e:pred>
    ///         <e:pred name="TopOfBoundedTree"><code>i</code><e:var n="t"/><e:st n="T"/><e:var n="lb"/><e:var n="hb"/></e:pred>
    ///       </e:sep>
    ///       <e:doubleimpl>
    ///         <code>eq</code>
    ///         <e:eq><code>value</code><e:var n="t"/></e:eq>
    ///       </e:doubleimpl>
    ///     </e:and>
    ///   </e:indent></e:expr>
    /// </e:exists>


    if (eq) {
      // Assert if-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q. TopOfTree(i, value, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      // Root value in set (lemma).
      /// Tree(root, S) ∧
      /// ∃Q.
      ///   TopOfTree(i, value, Q) ∧
      ///   (value ∈ Q) ↔ (value ∈ S) ∧
      ///   value ∈ Q
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      // →.  Weaken TopOfTree.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// value ∈ S ∧ ¬found

      found = true;

      // Assignment.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// value ∈ S ∧ found

      // →.  Re-establish invariant.
      /// Tree(root, S) ∧
      /// ∃Q. Tree(i, Q) ∧ (value ∈ Q) ↔ (value ∈ S)
      /// ∧ found → (value ∈ S)
    }
    else {
      // Assert else-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q, v. TopOfTree(i, v, Q) ∧ (value ∈ Q) ↔ (value ∈ S) ∧ v ≠ value
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      Node* next = descend(i, value);

      // Assert else-condition.  Equality.
      /// Tree(root, S) ∧
      /// ∃Q, v. TopOfTree(i, v, Q) ∧ (value ∈ Q) ↔ (value ∈ S) ∧ v ≠ value
      /// ∧ found → (value ∈ S)
      /// ∧ ¬found

      i = next;
    }
  }
  return found;
}

