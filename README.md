# RegExSynthesis

## Matching the RegEx by building the NFA

In the directory [RegExNFA](./RegExNFA/). I wrote a simple RegEx engine by converting the RegExes to NFAs, and eliminating the epsilon transitions. The method was straightforward but it encountered severe scalability issue. It could not synthesize any non-trivial RegEx before using up the RAM of my PC.

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

The code is in [RegExEncoding](./RegExEncoding/).
