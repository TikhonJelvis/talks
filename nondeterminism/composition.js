var time = 400;

Raphael(function () {
  var r = Raphael("slides", 2000, 2000),
      slides = [
        composition(r)
      ],
      currSlide = slides.shift();

  currSlide.show();
  
  document.body.onkeypress = function (e) {
    if (e.keyCode == 13 || e.keyCode == 32) {
      if (currSlide) {
        var res = currSlide.step();

        if (!res) {
          currSlide.hide();
          currSlide = slides.shift();
          if (currSlide) currSlide.show();
        }
      }

      return false;
    }
  };
});

function composition(r) {
  var arrows = [
        funArrow(r, 400, 300, 100, "red", "green"),
        funArrow(r, 600, 300, 100, "green", "blue")
      ],  
      longArrow = funArrow(r, 450, 300, 200, "red", "blue"),
      text = [
        hiddenText(r, 560, 100, "Function Composition: g ∘ f")
      ];

  var steps = [
    all(arrows, shown),
    function () {
      arrows[0].animate(moveBy(50, 0));
      arrows[1].animate(moveBy(-50, 0));
    },
    function () {
      all(arrows, hidden)();
      all([longArrow], shown)();
    }
  ]

  return Slide(all(text, shown), all([longArrow].concat(text), hidden), steps);
}

function funArrow(r, x, y, len, a, b) {
  var set = r.set();

  set.push(r.rect(x + 10, y - 2, len - 30, 4).attr("fill", "black"));
  set.push(r.circle(x, y, 10).attr("fill", a));
  set.push(r.circle(x + len, y, 10).attr("fill", b));
  set.push(r.path(["M", x + len - 10, ",", y, "L", x + len - 20, ",", y + 10, "L", x + len - 20, ",", y - 10, "L", x + len - 10, ",", y].join("")).attr("fill", "black"));

  set.attr("opacity", 0);

  return set;
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


function moveBy(x, y) {
  return Raphael.animation({transform : ["t", x, ",", y].join("")}, time);
}

function hiddenText(r, x, y, text) {
  var t = prolog(r, x, y, text);
  t.attr(hidden);
  return t;
}

function prolog(r, x, y, text) {
  var t = r.text(x, y, text);
  t.attr({"font-size" : 40, "font-family" : "monospace"});

  return t;
}
