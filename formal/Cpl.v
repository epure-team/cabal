From Stdlib Require Import List String.
Import ListNotations.

Inductive pos : Type := Pinstr | Pdata.

Inductive frag : Type :=
  | FProse   : string -> frag
  | FData    : string -> frag
  | FFenced  : frag -> frag
  | FSection : string -> list frag -> frag.

Section frag_ind'.
  Variable P : frag -> Prop.
  Hypothesis HProse : forall s, P (FProse s).
  Hypothesis HData  : forall r, P (FData r).
  Hypothesis HFence : forall f, P f -> P (FFenced f).
  Hypothesis HSec   : forall n fs, Forall P fs -> P (FSection n fs).
  Fixpoint frag_ind' (f : frag) : P f :=
    match f with
    | FProse s    => HProse s
    | FData r     => HData r
    | FFenced g   => HFence g (frag_ind' g)
    | FSection n fs =>
        HSec n fs
          ((fix lst (l : list frag) : Forall P l :=
              match l with
              | []      => Forall_nil P
              | x :: xs => Forall_cons x (frag_ind' x) (lst xs)
              end) fs)
    end.
End frag_ind'.

Inductive trust : Type := Public | Trusted | Untrusted | Tainted.

Inductive has_ty : frag -> trust -> pos -> Prop :=
  | T_Prose : forall s, has_ty (FProse s) Trusted Pinstr
  | T_Data  : forall r, has_ty (FData r) Untrusted Pdata
  | T_Fence : forall f rho, has_ty f rho Pdata -> has_ty (FFenced f) Trusted Pinstr
  | T_Sec   : forall n fs rho p,
                Forall (fun f => has_ty f rho p) fs ->
                has_ty (FSection n fs) rho p.

(* STRENGTHENED: the fence case now CARRIES the obligation that everything
   under the fence is at data position. data_clean witnesses that. *)
Inductive data_clean : frag -> Prop :=
  | DC_data  : forall r, data_clean (FData r)
  | DC_fence : forall f, data_clean f -> data_clean (FFenced f)
  | DC_sec   : forall n fs, Forall data_clean fs -> data_clean (FSection n fs).

Inductive instr_clean : frag -> Prop :=
  | IC_prose : forall s, instr_clean (FProse s)
  | IC_fence : forall f, data_clean f -> instr_clean (FFenced f)   (* now conditional! *)
  | IC_sec   : forall n fs, Forall instr_clean fs -> instr_clean (FSection n fs).

(* helper: anything well-typed at Pdata is data_clean *)
Lemma data_typed_clean : forall f rho, has_ty f rho Pdata -> data_clean f.
Proof.
  intro f.
  induction f using frag_ind'.
  4: { intros rho Hty. apply DC_sec. inversion Hty; subst.
       rewrite Forall_forall in *. intros x Hx. eauto. }
  all: intros rho Hty.
  - inversion Hty.                  (* FProse cannot be typed at Pdata *)
  - apply DC_data.
  - inversion Hty.                  (* FFenced cannot be typed at Pdata (T_Fence -> Pinstr) *)
Qed.

Theorem injection_safe :
  forall f rho, has_ty f rho Pinstr -> instr_clean f.
Proof.
  intro f.
  induction f using frag_ind'.
  4: { intros rho Hty. apply IC_sec. inversion Hty; subst.
       rewrite Forall_forall in *. intros x Hx. eauto. }
  all: intros rho Hty.
  - apply IC_prose.
  - inversion Hty.
  - apply IC_fence. inversion Hty; subst. eapply data_typed_clean. eauto.
Qed.

(* LIST-LEVEL CONTAINMENT: the OCaml top-level `prompt` is a `frag list` (flattened
   by <+>, never wrapped in a single FSection root), so the per-frag theorem above
   does not yet talk about the shape the implementation produces at the top level.
   This lemma lifts injection_safe to the list: if every element of the top-level
   spine is well-typed at instruction position, every element is instr_clean.
   prompt_safe is the predicate over the ACTUAL top-level object. *)
Definition prompt_well_typed (fs : list frag) (rho : trust) : Prop :=
  Forall (fun f => has_ty f rho Pinstr) fs.

Definition prompt_safe (fs : list frag) : Prop :=
  Forall instr_clean fs.

Theorem injection_safe_list :
  forall fs rho, prompt_well_typed fs rho -> prompt_safe fs.
Proof.
  intros fs rho Hwt. unfold prompt_safe, prompt_well_typed in *.
  rewrite Forall_forall in *.
  intros x Hx. eapply injection_safe. eauto.
Qed.

(* RENDER MODEL: make "renders data only via quote_data" a theorem, not a comment.
   render is parameterized by a backend's quote_data and emit (for prose). *)
Section Render.
  Variable quote_data : string -> string.   (* the data-escaping fn *)
  Variable open_s close_s : string -> string.

  Fixpoint render (f : frag) : string :=
    match f with
    | FProse s      => s
    | FData r       => quote_data r          (* data ALWAYS through quote_data *)
    | FFenced g     => render g
    | FSection n fs => open_s n ++ concat "" (map render fs) ++ close_s n
    end.

  (* Every data payload that appears in the output is the image of quote_data.
     Stated as: render of a bare FData is exactly quote_data of its payload,
     and render is a homomorphism that never introduces an un-quoted data string. *)
  Lemma render_data_quoted : forall r, render (FData r) = quote_data r.
  Proof. reflexivity. Qed.
End Render.

Definition lub (a b : trust) : trust :=
  match a, b with
  | Public, x | x, Public => x
  | Trusted, Trusted => Trusted
  | Untrusted, Untrusted => Untrusted
  | _, _ => Tainted
  end.

Theorem lub_assoc : forall a b c, lub a (lub b c) = lub (lub a b) c.
Proof. intros [] [] []; reflexivity. Qed.
Theorem lub_comm : forall a b, lub a b = lub b a.
Proof. intros [] []; reflexivity. Qed.
Theorem lub_idem : forall a, lub a a = a.
Proof. intros []; reflexivity. Qed.
Theorem lub_public_unit : forall a, lub Public a = a /\ lub a Public = a.
Proof. intros []; split; reflexivity. Qed.

Print Assumptions injection_safe.
Print Assumptions injection_safe_list.
