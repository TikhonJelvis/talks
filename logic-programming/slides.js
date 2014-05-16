var time = 400;

Raphael(function () {
  var r      = Raphael("slides"),
      slides = [
        unification(r)
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

function unification(r) {
  var set  = r.set(),
      t1   = prolog(200, 150, "foo(X, bar(a, b), b)"),
      t2   = prolog(600, 150, "foo(bar(X, b), Y, X)"),
      name = r.rect(230, 75, 60, 150),
      num  = r.rect(303, 75, 253, 150);

  name.attr({"fill" : "green", "opacity" : 0});
  num.attr({"fill" : "green", "opacity" : 0});

  name.hide();
  num.hide();

  set.push(t1);
  set.push(t2);
  set.push(name);
  set.push(num);

  var steps = [
    function () {
      var a1 = moveTo(400, 100),
          a2 = moveTo(400, 200);

      t1.animate(a1);
      t2.animateWith(t1, a1, a2);
    },
    function () {
      name.show();
      name.animate({"opacity" : 0.45}, time);
    },
    function () {
      var a = Raphael.animation({"opacity" : 0}, time);
      
      num.show();
      name.animate(a);
      num.animateWith(name, a, {"opacity" : 0.45}, time);
    },
    function () {
      num.animate(hidden, time);
    }
  ]

  function show(animate) { all([t1, t2], shown, animate) }
  function hide(animate) { all([t1, t2, name, num], hidden, animate) }

  return Slide(show, hide, steps);

  function prolog(x, y, text) {
    var t = r.text(x, y, text);
    t.attr({"font-size" : 40});

    return t;
  }
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

function all(target, elements) {
  return function (animate) {
    elements.forEach(function (e) {
      e.show();
      animate ? e.animate(hidden, time) : e.attr(hidden);
    });
  }
}

function moveTo(x, y) {
  return Raphael.animation({x : x, y : y}, time);
}
