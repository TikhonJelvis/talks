# My experience with SMT solvers:

I worked Chlorophyll, a synthesis-based optimizing compiler for the GreenArrays architecture that used Z3. You can read more about it in a PLDI 2014 paper (http://jelv.is/chlorophyll.pdf).

Most of the stuff in my talk is based on ideas from this project as well as a graduate seminar I took on program synthesis at Berkeley.

# Speaking Experience

I have experience giving talks at local meets and BayHac which has morphed into a Haskell conference over the last two year. My last two talks were recorded. The BayHac talk about "Thinking with Laziness" even garnered some general interest, getting to the Hacker News front page:

  * Functional Graph Algorithms: http://begriffs.com/posts/2015-09-04-pure-functional-graphs.html
  * Thinking with Laziness (BayHac 2015): http://begriffs.com/posts/2015-06-17-thinking-with-laziness.html

# Cool Stuff

We can use the solver to *run the program backwards*. Given an output, we can figure out what input(s) produce it with the given program. This can be a pretty cool demo.

Program synthesis is also really cool, but beyond the scope of this talk. I *might* have a demo of sketch-based synthesis, but probably not.

This talk will also help people understand **Liquid Haskell** and how it works—Liquid Haskell uses Z3. Liquid Haskell is a project that excites a lot of Haskellers but hasn't seen too much use; perhaps a bit of exposure to Z3 will also help people make the leap and try Liquid Haskell.
