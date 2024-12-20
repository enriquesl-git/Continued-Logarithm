# Continued-Logarithm
This is a simpler and more practical _"continued logarithm"_ inspired on the original concept of [Bill Gosper in 1978](https://perl.plover.com/classes/cftalk/INFO/gosper.txt). 

It is implemented as non-positional binary and as non-positional ternary. 


Nonpositional ternary ('Ternary') data type and its operations.

It is first defined a type Term, and then an ordered list of them
starting by the greatest one, Terms = `[Term]`, such that negative terms
are represented with the `-` sign behind the value (exponent of 3).
An example is:
```
  377 = [6, 5-, 4-, 3-, 0-]
      = - 3^0 - 3^3 - 3^4 - 3^5 + 3^6
      = 3^6 - 3^5 - 3^4 - 3^3 - 3^0
```
Then, the type Ternary is defined as a reversed list of the cummulative
Terms, such that an element value is the sum of them up to it:
```
  377 = NT [0-,3-,1- 1- 1]
      = 3^0*(-1 + 3^3*(-1 + 3^1*(-1 + 3^1*(-1 + 3^1))))
```
It should be an instance of `Integral` and `Signed`, so it should
implementmet methods for: `Ord`, `Num`, `Signed`, `Integral`

Integer division and square root, `quotRem`and `rootRem`, returns the closest one, 
so that remainder will be negative if it closer to zero than the positive one. 
