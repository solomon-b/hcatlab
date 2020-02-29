# hCatLab

The goal of this project is to create a typechecked living documention of
algebraic structures in Haskell loosely inspired by ncatlab.org. 

All modules are literate haskell documents. Documentation should be done in
pandoc compatible markdown. Code implementations need not be performant;
preference is towards pedagogical clarity.

The end goal is both a library for exploratory coding along with a static
website generated from the LHS files with extensive educational material
surrounding the code.

CONTRIBUTIONS HIGHLY ENCOURAGED!

## TODO

- [ ] Numeric types
- [ ] Lenses
- [ ] Parser type
- [ ] Free Effects
- [ ] Redefine types in terms of their Monad Transformer counterparts
- [ ] MTL Typeclasses
- [ ] Many more typeclasses: 
  - [ ] MonadFail
  - [ ] MonadPlus
  - [ ] Arrow
  - [ ] Selective Applicative Functor
  - [ ] Witherable
  - [ ] Rings
  - [ ] Groups
  - ...
- [ ] Build script to generate static site via pandoc
- [ ] Extensive documentation everywhere
