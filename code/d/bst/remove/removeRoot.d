module bst.removeRoot;

import bst.node;
import bst.remove.removeMax.removeMax;
import bst.remove.removeMax.RemoveMaxRet;


Node* removeRoot(Node* root) {
  assert(root != null); // [snip]
  Node* o;
  // Function precondition.
  /// <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:sep>
  ///       <e:fcell>
  ///             <code>root</code>
  ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///           </e:fcell>
  ///       <e:sep>
  ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///       </e:sep>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:expr>
  /// </e:exists>

  if (root.c[0] == null) {
    // Assert if-condition.  Substitution.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<m:null/>,<e:var n="r"/></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><m:null/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:pred name="Tree"><m:null/>, <e:st n="S"/></e:pred>
    //   <e:pred name="EmptyTree"><m:null/>, <e:st n="S"/></e:pred>
    // </e:logimpl>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<m:null/>,<e:var n="r"/></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="EmptyTree"><m:null/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Open <e:pred name="EmptyTree"><m:null/>, <e:st n="L"/></e:pred>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<m:null/>,<e:var n="r"/></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <m:hemp/>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:st n="L"/><m:empty/></e:eq>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Use <e:sep><e:predicate>X</e:predicate><m:hemp/></e:sep> = <e:predicate>X</e:predicate>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<m:null/>,<e:var n="r"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:st n="L"/><m:empty/></e:eq>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Substitution.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="r"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<m:null/>,<e:var n="r"/></e:list>
    ///       </e:fcell>
    ///       <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    o = root.c[1];
    // Assignment.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<m:null/>,<code>o</code></e:list>
    ///           </e:fcell>
    ///       <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    delete root;
    // Free heap chunk.
    /// <e:exists>
    ///   <e:vars><e:st n="R"/></e:vars>
    ///   <e:expr>
    ///     <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred> ∧
    ///     <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    //   <e:eq><e:st n="R"/><e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:eq>
    // </e:logimpl>
    /// <e:exists>
    ///   <e:vars><e:st n="R"/></e:vars>
    ///   <e:expr>
    ///     <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred> ∧
    ///     <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:st n="R"/><e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:eq>
    ///   </e:expr>
    /// </e:exists>

    // Substitution.  Discard <e:pred name="TCompose"><m:empty/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    // and <e:eq><e:st n="R"/><e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:eq>.
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:noteq><e:var n="l"/><m:null/></e:noteq>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: .
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="NonEmptyTree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    if (root.c[1] == null) {
      // This branch mostly symmetrical to the previous…
      o = root.c[0];
      delete root;
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
    }
    else {
      // Deny if-condition.  Use lemma: .
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="NonEmptyTree"><e:var n="l"/>, <e:st n="L"/></e:pred>
      ///             <e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>
      
      RemoveMaxRet r = removeMax(root.c[0]); auto nv = r.max; auto newLeft = r.root;
      // Specification of <code>removeMax</code>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><code>newLeft</code>, <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus></e:pred>
      ///             <e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      root.value = max; root.c[0] = newLeft;
      // Assignment.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>nv</code>,<code>newLeft</code>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><code>newLeft</code>, <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus></e:pred>
      ///             <e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // <m:existsIntro/> on <code>newLeft</code> as <e:var n="l"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus></e:pred>
      ///             <e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // Weakening lemma: .
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // Lemma: <e:logimpl>
      //   <e:and>
      //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      //     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      //   </e:and>
      //   <e:pred name="TCompose"><e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
      // </e:logimpl>.<br />
      // Discard <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      // and <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>.
      /// <e:exists>
      ///   <e:vars><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // <m:existsIntro/> on <e:setminus><e:st n="L"/><e:set><code>nv</code></e:set></e:setminus> as <e:st n="L"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TCompose"><e:st n="L"/>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // Close <e:pred name="TopOfTree"><code>root</code>, <code>nv</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>nv</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>

      // <m:existsIntro/> on <code>nv</code> as <e:var n="v"/>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
      ///   </e:expr>
      /// </e:exists>

      // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>.
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>

      // Weakening
      /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>

      o = root;
      // Assignment.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
    }
  // If-rule.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
  }
  // If-rule.  Postcondition.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>

  return root;
}


// [snip:start]
unittest {
  auto a = new Node(5);
  a = removeRoot(a);
  assert(a == null);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a = removeRoot(a);
  assert(a);
  assert(a.value == 4);
  assert(!a.c[0]);
  assert(!a.c[1]);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a.c[1] = new Node(6);
  a = removeRoot(a);

  assert(a);
  assert(a.value == 4);
  assert(a.c[0] == null);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
// [snip:stop]