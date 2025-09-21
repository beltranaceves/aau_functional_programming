{- Exercise 01:
Write down Haskell definitions of quango and tango that have the following types; it is
not important what the definitions do as long as they are type correct.

> quango :: a −> [a]

quango x = [x]

Yes this function is polymorphic by parametric polymorphism, it accepts as an input parameter values of any type. 

> tango :: Num p1 => (a, b) −> p2 −> p1

tango (_, _) _ = 1


Yes this function is polymorphic, both by parametric polymorphism and overloading, given the use of type variables and type classes

Are quango and tango polymorphic? If so, tell us if for each of them if this involves
parametric polymorphism or overloading (ad hoc polymorphism) or maybe both – and
how. If not, tell us why.
-}

{- Exercise 02:

Please note: This problem is about the λ-calculus (as presented in the short note) and
is not related to the content on types. Find a terminating reduction sequence of the λ-expression

(λx.xy)(λz.(λu.uu))

(λx.xy)uu
uuy

(λz.(λu.uu)) y


To do this, use the reduction rules of the note.
-}