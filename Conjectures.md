# Conjectures

Not sure if I got something wrong. My current impresseion is that the soundness theorem, intuitive, says if a type system claims that given a well-typed program and an expression inside the program, if the type-system claims that the expression is of type (`T`), the expression is always reduced to a value of type (`T`) at runtime. In order to state this theorem, I think, we must have these concepts:

- `C`, context, which allows to circle an expression in a program
- `circle(e)`, a kind of expressions, which allows us to keep track of the circle at runtime

The `circle(e)` construct should be congruence (is this the right terminology?) in both the well-typedness judgment and in the reduction relation.

The theorem will be stated as followed,

```
for all `C₁` and `C₂` and `v` and `e` and `T`
if  `⊢ Cᵢ[e]`
and `⊢ Cᵢ : ⟨Γ, T⟩` (I mean the hole is under `Γ` and expects a `T`)
and `Γ ⊢ e : T`
and `C₁[circle(e)] --> C₂[circle(v)]`
then
  `⊢ v : T`
```

Ψ | Γ | Γ | e ↝ e- ⇒ T
Ψ | Γ | Γ | s ↝ s- ⇐ T+☠

```
for all `n` and `Program` and `Program-₁`
if `⊢ Program ↝ Program-₁`
then
  there exists an `Program-₂`
  such that
    `Program-₁ -->ⁿ Program-₂`
```

