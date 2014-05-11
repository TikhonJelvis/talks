function go() {
  var u  = Raphael("unification"),
      t1 = prolog(200, 150, "foo(X, Y, bar(X, Y))"),
      t2 = prolog(600, 150, "foo(X, Y, bar(X, Y))");

  function prolog(x, y, text) {
    var t = u.text(x, y, text);
    t.attr({"font-size" : 40});

    return t;
  }

  t1.click(function () {
    var a1 = moveTo(400, 100),
        a2 = moveTo(400, 200);

    t1.animate(a1);
    t2.animateWith(t1, a1, a2);
  });
  
  function moveTo(x, y) {
    return Raphael.animation({x : x, y : y}, 400);
  }
}

window.onload = go;


