# Subtleties

- `...` is not listed as a symbol even though `args...` is permitted syntax (see `grp/basic.gd`)
- Parsing permutations is complicated. We have the differences between `()`, `(bracketed expr)`, `(p1, p2, p3)`
  and `(p1, p2) (q1, q2)`.

# Progress
- successfully parse `grp/basic.gd`
- Can parse `grp/basicprm.gd`.
- Next, should parse `grp/basicprm.gi` to handle permutation notation.
- `lib/stbc.gd` and `lib/stbc.gi` for stabilizer chains for Schrier tree.
- `lib/stbcrand.gi` : random construction of stabilizer chains.
