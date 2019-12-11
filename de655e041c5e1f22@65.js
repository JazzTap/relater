// https://observablehq.com/@mbostock/hello-tau-prolog@65
export default function define(runtime, observer) {
  const main = runtime.module();
  main.variable(observer()).define(["md"], function(md){return(
md`# Hello, Tau Prolog

Ref. http://tau-prolog.org/`
)});
  main.variable(observer()).define(["md"], function(md){return(
md`Here’s a Prolog program:`
)});
  main.variable(observer("program")).define("program", ["pl"], function(pl){return(
pl`
likes(sam, salad).
likes(dean, pie).
likes(sam, apples).
likes(dean, whiskey).
`
)});
  main.variable(observer()).define(["md"], function(md){return(
md`Here’s how you query the program:`
)});
  main.variable(observer()).define(["program"], function(program){return(
Array.from(program.query("likes(sam, X)."), s => s.links.X.id)
)});
  main.variable(observer()).define(["program"], function(program){return(
Array.from(program.query("likes(X, salad)."), s => s.links.X.id)
)});
  main.variable(observer()).define(["md"], function(md){return(
md`I wrapped the Tau Prolog API to support iterators rather than callbacks for queries, and to provide a tagged template literal for convenient parsing of programs.`
)});
  main.variable(observer("pl")).define("pl", ["require"], async function(require)
{
  const pl = await require("tau-prolog@0.2").catch(() => window.pl);

  class QueryIterator {
    constructor(session) {
      this._session = session;
      this._callback = x => this._value = x;
    }
    [Symbol.iterator]() {
      return this;
    }
    next() {
      this._session.answer(this._callback);
      return this._value ? {done: false, value: this._value} : {done: true};
    }
  }

  class Program {
    constructor(source) {
      this._session = pl.create();
      this._session.consult(source);
    }
    query(source) {
      this._session.query(source);
      return new QueryIterator(this._session);
    }
  }

  return function() {
    return new Program(String.raw.apply(String, arguments));
  };
}
);
  return main;
}
