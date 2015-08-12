var time = 400;

Raphael(function () {
  var r = Raphael("slides", 2000, 2000),
      slides = [
        atoms(r),
        predicates(r),
        facts(r),
        basicQueries(r),
        variables(r),
        listComp(r),
        variableRules(r),
        eqRule(r),
        compoundRules(r),
        andOr(r),
        numbers(r),
        lists(r),
        listFuns(r),
        term(r),
        predicate(r),
        rule(r),
        unification(r),
        unify(r),
        argNum(r),
        combine(r),
        varl(r),
        varr(r),
        preds(r),
        occurs(r),
        occursCheck(r)
      ],
      currSlide = slides.shift();

  currSlide.show();
  
  document.body.onclick = function () {
    if (currSlide) {
      var res = currSlide.step();

      if (!res) {
        currSlide.hide();
        currSlide = slides.shift();
        if (currSlide) currSlide.show();
      }
    }
  };
});

function atoms(r) {
  var atoms = [
        hiddenText(r, 800, 250, "x, y, z..."),
        hiddenText(r, 800, 350, "bob, pear, true, whatever...")
      ],
      ctors = [
        hiddenText(r, 800, 250, "X, Y, Z..."),
        hiddenText(r, 800, 350, "Bob, Pear, True, Whatever...")
      ];

  var step = revealItems(atoms);

  function toHask() {
    all(atoms, hidden)(true);
    all(ctors, shown)(true);
  }

  var steps = [step, step, toHask];

  return Slide(function () {}, all(ctors, hidden), steps);
}

function predicates(r) {
  var predicates = [
        hiddenText(r, 800, 250, "foo(x), bar(y, z)..."),
        hiddenText(r, 800, 350, "likes(bob, pear), noun(whatever)...")
      ],
      ctors      = [
        hiddenText(r, 800, 250, "Foo x, Bar y, Baz z..."),
        hiddenText(r, 800, 350, "Likes Bob Pear, Noun Whatever...")
      ];

  var step = revealItems(predicates);

  function toHask() {
    all(predicates, hidden)(true);
    all(ctors, shown)(true);
  }

  var steps = [step, step, toHask];

  return Slide(function () {}, all(ctors, hidden), steps);
}

function facts(r) {
  var facts = [
        hiddenText(r, 760, 100, "likes(alice, pear)."),
        hiddenText(r, 800, 200, "likes(alice, orange)."),
        hiddenText(r, 660, 300, "female(alice)."),
        hiddenText(r, 720, 400, "likes(bob, pear)."),
        hiddenText(r, 760, 500, "likes(bob, banana)."),
        hiddenText(r, 600, 600, "male(bob).")
      ];

  return Slide(all(facts, shown), all(facts, hidden), []);
}

function basicQueries(r) {
  var lines = [
        hiddenText(r, 800, 100, "?- likes(alice, pear)."),
        hiddenText(r, 800, 200, "Yes"),
        hiddenText(r, 800, 300, "?- likes(bob, orange)."),
        hiddenText(r, 800, 400, "No"),
        hiddenText(r, 800, 500, "?- male(charles)."),
        hiddenText(r, 800, 600, "No")
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function variables(r) {
  var lines = [
        hiddenText(r, 800, 100, "X, Y, Z, Fruit, Whatever..."),
        hiddenText(r, 800, 200, "?- likes(alice, Fruit)."),
        hiddenText(r, 800, 300, "Fruit = pear"),
        hiddenText(r, 800, 400, "Fruit = orange"),
        hiddenText(r, 800, 500, "?- likes(charles, Fruit)"),
        hiddenText(r, 800, 600, "No")
      ];
    
  return Slide(none, all(lines, hidden), revealAll(lines));
}

function listComp(r) {
  var pat  = hiddenText(r, 800, 300, "case Likes Alice x of {- ... -}"),
      comp = hiddenText(r, 800, 300, "[x | Likes Alice x <- universe]");

  function step1() {
    pat.animate(shown, time);
  }

  function step2() {
    pat.animate(hidden, time);
    comp.animate(shown, time);
  }

  return Slide(none, all([pat, comp], hidden), [step1, step2]);
}

function variableRules(r) {
    var lines = [
          hiddenText(r, 800, 100, "likes(charles, X)."),
          hiddenText(r, 800, 200, "?- likes(charles, apple)."),
          hiddenText(r, 800, 300, "Yes"),
          hiddenText(r, 800, 400, "?- likes(charles, X)."),
          hiddenText(r, 800, 500, "X = X_2")
        ];
    
  return Slide(none, all(lines, hidden), revealAll(lines));
}

function eqRule(r) {
    var lines = [
          hiddenText(r, 800, 100, "eq(X, X)."),
          hiddenText(r, 800, 200, "eq(pear, pear)."),
          hiddenText(r, 800, 300, "Yes"),
          hiddenText(r, 800, 400, "eq(pear, banana)."),
          hiddenText(r, 800, 500, "No"),
          hiddenText(r, 800, 600, "eq(foo, foo)."),
          hiddenText(r, 800, 700, "Yes"),
        ];
    
  return Slide(none, all(lines, hidden), revealAll(lines));
}

function compoundRules(r) {
  var fruit = hiddenText(r, 800, 100, "fruit(pear). fruit(apple). fruit(banana)."),
      rule  = hiddenText(r, 800, 200, "likes(charles, X) :- fruit(X)."),
      impl  = hiddenText(r, 800, 200, "likes(charles, X) ⇐ \u00a0fruit(X)."),
      lines = [
        hiddenText(r, 800, 300, "?- likes(charles, X)."),
        hiddenText(r, 800, 400, "pear"),
        hiddenText(r, 800, 500, "orange"),
        hiddenText(r, 800, 600, "banana")
      ];

  var steps = [ function () { rule.animate(shown, time) },
                function () {
                  rule.animate(hidden, time);
                  impl.animate(shown, time);
                },
                function () { lines[0].animate(shown, time) },
                function () { lines[1].animate(shown, time) },
                function () { lines[2].animate(shown, time) },
                function () { lines[3].animate(shown, time) }
              ]

  return Slide(all([fruit], shown), all([fruit, impl].concat(lines), hidden), steps);
}

function andOr(r) {
  var lines = [
        hiddenText(r, 800, 100, "likes(dan, X) :- fruit(X), yellow(X)."),
        hiddenText(r, 726, 300, "likes(eve, X) :- fruit(X)."),
        hiddenText(r, 800, 400, "likes(eve, X) :- vegetable(X)."),
        hiddenText(r, 800, 600, "likes(eve, X) :- fruit(X); vegetable(X).")
        ];

  return Slide(none, all(lines, hidden), revealAll(lines));  
}

function numbers(r) {
  var lines = [
        hiddenText(r, 800, 200, "number(zero)."),
        hiddenText(r, 800, 300, "number(N) :- eq(N, s(X)), number(X)."),

        hiddenText(r, 800, 500, "data ℕ = Z | S ℕ"),
    
        hiddenText(r, 800, 300, "number(s(X)) :- number(X)."),
    
        hiddenText(r, 800, 450, "data Z"),
        hiddenText(r, 800, 550, "data S x"),
      ],
      steps = [
        function () {
          lines[0].animate(shown, time);
          lines[1].animate(shown, time);
        },
        function () {
          lines[1].animate(hidden, time);
          lines[3].animate(shown, time);
        },
        function () {lines[2].animate(shown, time);},
        function () {
          lines[2].animate(hidden, time);
          lines[4].animate(shown, time);
          lines[5].animate(shown, time);
        }
      ];

  return Slide(none, all(lines, hidden), steps);  
}

function lists(r) {
  var lists = [
        hiddenText(r, 800, 100, "list(nil)."),
        hiddenText(r, 800, 200, "list(cons(X, XS) :- list(XS).")
      ],
      sugar = [
        hiddenText(r, 800, 100, "list([])."),
        hiddenText(r, 800, 200, "list([X|XS]) :- list(XS)."),
        hiddenText(r, 800, 300, "list([1, 2]) == cons(1, cons(2, nil)).")
      ]

  var steps = [
    function () {
      all(lists, hidden)(true);
      all(sugar, shown)(true);
    }
  ]

  return Slide(all(lists, shown), all(sugar, hidden), steps);
}

function listFuns(r) {
  var hsHead = hiddenText(r, 800, 200, "head (x:xs) = x"),
      plHead = hiddenText(r, 800, 200, "head([X|XS], X)."),
      lines  = [
        hiddenText(r, 800, 300, "?- head([1,2,3], X)."),
        hiddenText(r, 800, 400, "X = 1"),
        hiddenText(r, 800, 500, "?- head(XS, 1)."),
        hiddenText(r, 800, 600, "XS = [1|XS_2]")
      ]

  var steps = [
        function () {
          hsHead.animate(hidden, time);
          plHead.animate(shown, time);
        }
      ].concat(revealAll(lines));

  return Slide(all([hsHead], shown), all([plHead].concat(lines), hidden), steps);
}

function term(r) {
  var lines = [
    hiddenText(r, 800, 300, "data Term = Atom String\n\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0|\u00a0Var\u00a0String\n\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0|\u00a0Pred\u00a0Predicate"),
    hiddenText(r, 800, 500, "bob, X, likes(bob, X)")
  ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function predicate(r) {
  var lines = [
        hiddenText(r, 800, 200, "data Predicate = Predicate String [Term]"),
        hiddenText(r, 800, 400, "likes(bob, X)"),
        hiddenText(r, 800, 500, "eq(X, X)")
      ];

  var steps = [
    function () {
      lines[1].animate(moveTo(1300, 300));
    }
  ];

  return Slide(none, all(lines, hidden), revealAll(lines).concat(steps));
}

function rule(r) {
  var lines = [
    hiddenText(r, 800, 200, "data Rule = Rule Predicate [Predicate]"),
    hiddenText(r, 800, 500, "likes(dan, X) :- fruit(X), yellow(X)."),
    hiddenText(r, 800, 600, "eq(X, X).")
  ];

  var steps = [
    function () {
      lines[1].animate(moveTo(1300, 300));
    }
  ];

  return Slide(none, all(lines, hidden), revealAll(lines).concat(steps));
}

function unification(r) {
  var t1   = hiddenText(r, 400, 200, "foo(X, bar(a, b), b)"),
      t2   = hiddenText(r, 1200, 200, "foo(bar(X, b), Y, X)"),
      type = hiddenText(r, 800, 500, "type MGU = [(String, Term)]");
  //     name = r.rect(230, 75, 60, 150),
  //     num  = r.rect(303, 75, 253, 150);

  // name.attr({"fill" : "green", "opacity" : 0});
  // num.attr({"fill" : "green", "opacity" : 0});

  var steps = [
    function () {
      var a1 = moveTo(800, 150),
          a2 = moveTo(800, 250);

      t1.animate(a1);
      t2.animateWith(t1, a1, a2);
    },
    function () {
      type.animate(shown, time);
    }
  ]

  return Slide(all([t1, t2], shown), all([t1, t2, type], hidden), steps);
}

function unify(r) {
  var type  = hiddenText(r, 800, 100, "unify :: \nPredicate → Predicate → Maybe MGU"),
      fn    = hiddenText(r, 800, 250, "unify (Predicate n₁ b₁) (Predicate n₂ b₂)"),
      t1 = hiddenText(r, 800, 400, "foo(a, b, X, Y)."),
      t2 = hiddenText(r, 800, 500, "bar(A, b, Z, Y)."),
      fail = r.rect(515, 350, 100, 200),
      end = hiddenText(r, 800, 650, "| n₁ /= n₂ = Nothing");

  fail.attr({fill: "#BB2222", opacity : 0});

  var steps = [
    function () {
      t1.animate(shown, time);
      t2.animate(shown, time);
    },

    function () {
      fail.animate({opacity : 0.4}, time);
    },

    function () {
      end.animate({opacity : 1}, time);
    }
  ]

  return Slide(all([type, fn], shown), all([type, fn, t1, t2, fail, end], hidden), steps);
}

function argNum(r) {
  var t1 = hiddenText(r, 800, 200, "foo(a, b, X, Y)."),
      t2 = hiddenText(r, 740, 300, "foo(A, b, Z)."),
      fail = r.rect(650, 150, 400, 200),
      end = hiddenText(r, 800, 550, "| length b₁ /= length b₂ = Nothing");

  fail.attr({fill: "#BB2222", opacity : 0});

  var steps = [
    function () {
      fail.animate({opacity : 0.4}, time);
    },
    function () {
      end.animate(shown, time);
    }
  ];

  return Slide(all([t1, t2], shown), all([t1, t2, fail, end], hidden), steps);
}

function combine(r) {
  var lines = [
        hiddenText(r, 800, 200, "foldM combine [] $ zip b₁ b₂"),
        hiddenText(r, 800, 300, "combine :: Term → Term → Maybe MGU")
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function varl(r) {
  var lines = [
        hiddenText(r, 800, 200, "X"),
        hiddenText(r, 800, 300, "foo(a, b, bar(Y))"),
        hiddenText(r, 800, 500, "(Var l) r = Just $ (l, r) : mgu")
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function varr(r) {
  var lines = [
        hiddenText(r, 800, 200, "foo(a, b, bar(Y))"),
        hiddenText(r, 800, 300, "X"),
        hiddenText(r, 800, 500, "l (Var r) = Just $ (l, r) : mgu")
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function preds(r) {
  var lines = [
        hiddenText(r, 800, 200, "foo(X, Y)"),
        hiddenText(r, 800, 300, "foo(a, b)"),
        hiddenText(r, 800, 450, "(Pred l) (Pred r) =\n merge <$> unify l r <*> Just mgu"),
        hiddenText(r, 800, 650, "merge :: MGU → MGU → MGU"),
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function occurs(r) {
  var lines = [
        hiddenText(r, 800, 200, "X"),
        hiddenText(r, 800, 300, "cons(X, nil)"),
        hiddenText(r, 800, 450, "Oh no!").attr({"fill" : "red"}),
        hiddenText(r, 800, 550, "(Var l) r | not (r `contains` l) ="),
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function occursCheck(r) {
  var lines = [
        hiddenText(r, 800, 200, "let x = [x]"),
        hiddenText(r, 800, 350, "Occurs check:\n cannot construct the infinite type: t ~ [t]"),
      ];

  return Slide(none, all(lines, hidden), revealAll(lines));
}

function Slide(show, hide, transitions) {
  transitions = transitions || [];

  var next = transitions.slice(0);

  hide();

  return {
    show : function() {
      show(true);
    },

    step : function () {
      if (next.length == 0) return false;

      next.shift()();
      return true;
    },

    hide : function () {
      hide(true);
    }
  };
}

var shown  = {opacity : 1},
    hidden = {opacity : 0};

function none() {}

function all(elements, target) {
  return function (animate) {
    if (typeof animate == "undefined") animate = true;

    elements.forEach(function (e) {
      animate ? e.animate(target, time) : e.attr(target);
    });
  }
}

function revealItems(elements) {
  var i = 0;

  return function() {
    elements[i].animate(shown, time);
    i++;
  }
}

function revealAll(elements) {
  var step = revealItems(elements),
      ret  = [];

  for (var i = 0; i < elements.length; i++) ret.push(step);

  return ret;
}

function moveTo(x, y) {
  return Raphael.animation({x : x, y : y}, time);
}


function hiddenText(r, x, y, text) {
  var t = prolog(r, x, y, text);
  t.attr(hidden);
  return t;
}

function prolog(r, x, y, text) {
  var t = r.text(x, y, text);
  t.attr({"font-size" : 60, "font-family" : "monospace"});

  return t;
}
