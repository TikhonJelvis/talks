var time = 400;

function go() {
  var u    = Raphael("unification"),
      t1   = prolog(200, 150, "foo(X, bar(a, b), b)"),
      t2   = prolog(600, 150, "foo(bar(X, b), Y, X)"),
      name = u.rect(230, 75, 60, 150),
      num  = u.rect(303, 75, 253, 150);

  name.attr({"fill" : "green", "opacity" : 0});
  num.attr({"fill" : "green", "opacity" : 0});

  function prolog(x, y, text) {
    var t = u.text(x, y, text);
    t.attr({"font-size" : 40});

    return t;
  }

  var steps = [
    function () {
      var a1 = moveTo(400, 100),
          a2 = moveTo(400, 200);

      t1.animate(a1);
      t2.animateWith(t1, a1, a2);
    },
    function () {
      name.animate({"opacity" : 0.45}, time);
    },
    function () {
      var a = Raphael.animation({"opacity" : 0}, time);
 
      name.animate(a);
      num.animateWith(name, a, {"opacity" : 0.45}, time);
    }
  ];

  document.body.onclick = function () {
    steps.shift()();
  };
  
  function moveTo(x, y) {
    return Raphael.animation({x : x, y : y}, time);
  }
}

window.onload = go;


