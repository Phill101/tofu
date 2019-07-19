package tofu.optics

/** classic names for types */
object classic {
  type Equality[A, B]          = PSame[A, A, B, B]
  type Iso[A, B]               = PEquivalent[A, A, B, B]
  type Prism[A, B]             = PSubset[A, A, B, B]
  type Lens[A, B]              = PContains[A, A, B, B]
  type Optional[A, B]          = PProperty[A, A, B, B]
  type NonEmptyTraversal[A, B] = PRepeated[A, A, B, B]
  type Traversal[A, B]         = PItems[A, A, B, B]
  type NonEmptyFold[A, B]      = PReduced[A, A, B, B]
  type View[A, B]              = PDowncast[A, A, B, B]
  type Review[A, B]            = PUpcast[A, A, B, B]
  type Getter[A, B]            = PExtract[A, A, B, B]
  type Fold[A, B]              = PFolded[A, A, B, B]
  type Setter[A, B]            = PUpdate[A, A, B, B]

  type PEquality[S, T, A, B]          = PSame[S, T, A, B]
  type PIso[S, T, A, B]               = PEquivalent[S, T, A, B]
  type PPrism[S, T, A, B]             = PSubset[S, T, A, B]
  type PLens[S, T, A, B]              = PContains[S, T, A, B]
  type POptional[S, T, A, B]          = PProperty[S, T, A, B]
  type PNonEmptyTraversal[S, T, A, B] = PRepeated[S, T, A, B]
  type PTraversal[S, T, A, B]         = PItems[S, T, A, B]
  type PNonEmptyFold[S, T, A, B]      = PReduced[S, T, A, B]
  type PView[S, T, A, B]              = PDowncast[S, T, A, B]
  type PReview[S, T, A, B]            = PUpcast[S, T, A, B]
  type PGetter[S, T, A, B]            = PExtract[S, T, A, B]
  type PFold[S, T, A, B]              = PFolded[S, T, A, B]
  type PSetter[S, T, A, B]            = PUpdate[S, T, A, B]
}
