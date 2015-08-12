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
        listArrow(r, 400, 300, 100, "red", "green"),
        listArrow(r, 600, 300, 100, "green", "blue"),
        listArrow(r, 600, 300, 100, "green", "blue"),
        listArrow(r, 600, 300, 100, "green", "blue")
      ],  
      longArrow = longListArrow(r, 450, 300, 200, "red", "blue"),
      text = [
        hiddenText(r, 560, 100, "Kleisli Composition (for []): g <=< f")
      ];

  var steps = [
    all(arrows, shown),
    function () {
      arrows[0].forEach(function (e) { e.animate(moveBy(50, 0, e)) });
      arrows[1].forEach(function (e) { e.animate(moveBy(-50, 0, e)) });
      arrows[2].forEach(function (e) { e.animate(moveBy(-79, 71, e)) });
      arrows[3].forEach(function (e) { e.animate(moveBy(-79, -71, e)) });
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

  set.push(arrow(r, x, y, len));
  set.push(r.circle(x, y, 10).attr("fill", a));
  set.push(r.circle(x + len, y, 10).attr("fill", b));

  set.attr("opacity", 0);

  return set;
}

function listArrow(r, x, y, len, a, b) {
  var set  = r.set(),
      t1   = ["R45,", x, ",", y].join(""),
      t2   = ["R-45,", x, ",", y].join(""),
      move = ["t", len, ",", 0].join("");

  set.push(arrow(r, x, y, len).transform(t1));
  set.push(arrow(r, x, y, len));
  set.push(arrow(r, x, y, len).transform(t2));

  set.push(r.circle(x, y, 10).attr("fill", a));
  set.push(r.circle(x + len, y, 10).attr("fill", b));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t1 + move));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t2 + move));

  return set;
}

function longListArrow(r, x, y, len, a, b) {
  var set  = r.set(),
      t1   = ["R45,", x, ",", y].join(""),
      t2   = ["R-45,", x, ",", y].join(""),
      t3   = ["R23,", x, ",", y].join(""),
      t4   = ["R-23,", x, ",", y].join(""),
      move = ["t", len, ",", 0].join("");

  set.push(arrow(r, x, y, len).transform(t1));
  set.push(arrow(r, x, y, len).transform(t3));
  set.push(arrow(r, x, y, len));
  set.push(arrow(r, x, y, len).transform(t2));
  set.push(arrow(r, x, y, len).transform(t4));

  set.push(r.circle(x, y, 10).attr("fill", a));
  set.push(r.circle(x + len, y, 10).attr("fill", b));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t1 + move));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t2 + move));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t3 + move));
  set.push(r.circle(x, y, 10).attr("fill", b).transform(t4 + move));

  return set;
}

function arrow(r, x, y, len) {
  var set = r.set();

  set.push(r.rect(x + 10, y - 2, len - 30, 4).attr("fill", "black"));
  set.push(r.path(["M", x + len - 10, ",", y, "L", x + len - 20, ",", y + 10, "L", x + len - 20, ",", y - 10, "L", x + len - 10, ",", y].join("")).attr("fill", "black"));

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


function moveBy(x, y, element) {
  var old = element ? element.attr('transform').toString() : "";

  if (element.type == "set") {
    old = element[0].attr("transform").toString();
  }

  console.log("Transforming by:", old + ["T", x, ",", y].join(""));

  return Raphael.animation({transform : old + ["T", x, ",", y].join("")}, time);
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
