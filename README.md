# RegExSynthesis

## Matching the RegEx by building the NFA

In the directory [RegExNFA](./RegExNFA/). I wrote a simple RegEx engine by converting the RegExes to NFAs, and eliminating the epsilon transitions. The method was straightforward but it encountered severe scaling issue. It could not synthesize any non-trivial RegEx before using up the RAM of my PC.

Converting the RegEx to NFA is a simple job, and it is hard to write the function in a solver-friendly way. So I did not go deeper.

## Directly synthesize the DFA

To deal with the scalability issue, I tried to avoid the process of building the NFAs. To simplify the problem, I tried to synthesize the DFAs directly. The code is in [DFA](./DFA/).

Using a very simple encoding, I successfully synthesized some simple automata. The sketch is roughly a 2-d table for the transition function.

I implemented three interaction models.

1. The synthesizer proposes the strings, and the user identifies whether they are acceptable.
2. The synthesizer proposes two DFAs and a string that can only be recognized by one DFA.
3. Noninteractive model. Use a DFA as a reference implementation.

For the interactive synthesis, the user's response is simulated by the oracle. It seems that the two interaction models need roughly the same rounds to get an appropriate DFA. While using a reference implementation is much faster.

## Directly synthesize the RegEx, using the solver to match the RegEx with strings

Finally, it came to me that I did not need the automata. I could use the solver to directly decide whether the RegEx recognized the string.

The basic operators of the RegEx are concatenation, union and Kleene star. Nothing special needs to be done for the concatenation operator. For the union operator, the matcher will introduce a new boolean symbolic value and use it to decide whether the string should match the left part or the right part of the RegEx.

However, dealing with the Kleene star is a bit tricky. The key idea is that we can limit the input string's size, and ensure that no pattern repeats more than `starnum` times. Then the Kleene star `r*` can be reduced to

```
(eps)|r((eps)|r((eps)|r(...)))
```

By limiting the depth of the reduction, we can get a star-free regular expression with a reasonable size. The new regular expression is not equivalent to the original one, but they act the same on the input strings. So this reduction can ensure the correctness of the solution under the assumption of no pattern repeats more than `starnum` times.

I wrote two examples. One was a regular expression for 000-255, the other was for every positive number without leading zeros.

1. 000-255. I didn't implement the lookahead constructs so the handwritten regex is `[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]`. The sketch was defined as follows

```racket
(define (??num)
  (define (singlehole)
    (apply choose* '(0 1 2 3 4 5 6 7 8 9)))
  (choose*
   (single (singlehole))
   (fromto (singlehole) (singlehole))))
   
(define (??h1)
  (choose* (concat (??num) (concat (??num) (??num)))
           (concat (??num) (??num))
           (??num)))
           
(define ipsk (select (select (select (select (??h1) (??h1)) (??h1)) (??h1)) (??h1)))
```

The synthesizer successfully synthesized `(((([1-9][0-9]|2[0-4][0-9])|[0-9])|2[0-5][0-5])|1[0-9][0-9])` in 12s, with 25 examples.

2. Positive numbers without leading zeros. The handwritten regex was `0|[1-9][0-9]*`. The sketch was defined as follows

```racket
(define (??numstar)
  (choose* (??num) (star (??num))))

(define (??numstar2)
  (choose* (??numstar) (concat (??numstar) (??numstar))))

(define noleadingsk
  (choose* (??numstar2)
           (select (??numstar2) (??numstar2))))
```

The synthesizer successfully synthesized `([0-9]|[1-9]([0-9])*)` in 13s, with 11 examples.

I did not implement interactive synthesis for this method since it could be implemented with the same techniques for synthesizing the DFAs.

The code is in [RegExEncoding](./RegExEncoding/). It's a little tricky.

#### Update April 3, 2019:

##### Why I don't use the register form?

The register form, or the SSA-like form, is not suitable for current implementation. It is equivalent to defining the regex as ASTs and cannot improve efficiency.

The current implementation don't have a conventional 'interpreter' for the regexes. It introduces new symbolic values when trying to match the strings. Although the register form can reduce some same sub-expression into one instruction in the register form, it won't reduce the number of symbolic values or the complexity of solving, but make the things worse.

For example, part of the sketch could be as follows (where `0` means the instruction right before the current instruction, and `select a b` means `a|b`):

```racket
(list (...)
      (select 0 0)
      (select 0 0)
      (select 0 0)
      (...))
      (list ())
```

The sequence of n `select 0 0`s will cause the matcher to insert 2^n-1 fresh symbolic boolean values.

Take another example,

```racket
(list (...)
      (star 0)
      (star 0)
      (star 0)
      (...))
```

If we try to expand the Kleene star for depth `p`, the sequence of n `star 0`s will cause the matcher to insert (p-1)((p-1)^n-1)/(p-2)+1 fresh symbolic boolean values. It is O((p-1)^n). That is not acceptable.

Although the issue can be resolved by pruning the sketch space, such as ensuring the first component of `select` or `concat` instruction is not of the same opcode, and banning the nested star. But how to deal with this one:

```racket
(list (...)
      (select 0 1)
      (star 0)
      (select 0 1)
      (star 0)
      (...))
```

Or this one:

```racket
(list (...)
      (select 0 1)
      (concat 0 1)
      (star 0)
      (concat 0 1)
      (...))
```

We cannot filter out **all** the 'bad' sketches without the loss of the ability to express the whole program space. And if we allow the loss, it is still hard to develop a generic algorithm to prune the sketch space for different problems. So I hold the opinion that it is necessary to use a handcrafted sketch with user's knowledge in this problem, or at least in this implementation.


