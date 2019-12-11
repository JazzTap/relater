// https://observablehq.com/d/a02a42cb0118c640@1909
import define1 from "./de655e041c5e1f22@65.js";
import define2 from "./e93997d5089d7165@2200.js";

export default function define(runtime, observer) {
  const main = runtime.module();
  const fileAttachments = new Map([["seedrandom.min.js",new URL("./files/4b3c3bae9b0aac3a7a4af68ed432eb81297cc11ea95ad8b5d0e857684180dc6359711c6572813514f546d05b22dd34163ee6f0980b16ccb37061021128bad32f",import.meta.url)],["improv.js",new URL("./files/a00f7720ea9bf32f6558263eefe2acc0d79acbc94df74aec361417829d058d3cf013ca8acb4fe9f2122aec3fb29c5948f7d01ffe6a2459fbbcc9b35a6105943a",import.meta.url)]]);
  main.builtin("FileAttachment", runtime.fileAttachments(name => fileAttachments.get(name)));
  main.variable(observer()).define(["md"], function(md){return(
md`# relater`
)});
  main.variable(observer("viewof ics0")).define("viewof ics0", ["slider"], function(slider){return(
slider({min:0, max:1})
)});
  main.variable(observer("ics0")).define("ics0", ["Generators", "viewof ics0"], (G, _) => G.input(_));
  main.variable(observer("viewof ics1")).define("viewof ics1", ["slider"], function(slider){return(
slider({min:0, max:1})
)});
  main.variable(observer("ics1")).define("ics1", ["Generators", "viewof ics1"], (G, _) => G.input(_));
  main.variable(observer("viewof ics2")).define("viewof ics2", ["slider"], function(slider){return(
slider({min:0, max:1})
)});
  main.variable(observer("ics2")).define("ics2", ["Generators", "viewof ics2"], (G, _) => G.input(_));
  main.variable(observer("viewIcs")).define("viewIcs", ["html","viewof ics0","viewof ics1","viewof ics2"], function(html,$0,$1,$2){return(
html`${$0} ${$1} ${$2}`
)});
  main.variable(observer("root")).define("root", function(){return(
[Math.random(), Math.random(), Math.random()]
)});
  main.variable(observer("ics")).define("ics", ["ics0","ics1","ics2"], function(ics0,ics1,ics2){return(
[-Math.PI*ics0/8, -Math.PI*ics1/8, -Math.PI*ics2/8]
)});
  main.variable(observer("trace")).define("trace", ["root","ics","T"], function(root,ics,T)
{
  let trace = [[0,1,2].map(i => root[i] + ics[i])]
  for (let i = 1; i < T; ++i) {
    let v = trace[i-1],
        v_ = [v[0] + .02, // - .01*v[1], // if IJ is anti-synchronizing with JK
              v[1] + .03,
              v[2] + .05]
    v_ = v_.map(k => k % 1)
    trace.push(v_)
  }
  return trace
}
);
  main.variable(observer("viewof T")).define("viewof T", ["radio"], function(radio){return(
radio({
  title: 'Generate story',
  description: 'simulation steps',
  options: [10,15,20,25,30].map(i =>
            ({value: i, label: i})),
  value: 20
})
)});
  main.variable(observer("T")).define("T", ["Generators", "viewof T"], (G, _) => G.input(_));
  main.variable(observer("hint")).define("hint", ["html","spinner","trace","x","y","I","J","K","nameMap"], function(html,spinner,trace,x,y,I,J,K,nameMap)
{
  let ret = html`<div></div>`
  
  let row = html`<div></div>`
  for (let i of [0,1,2])
    ret.appendChild(spinner(trace.map(v => [x(v[i]), y(v[i])])))
  ret.appendChild(row)
  
  row = html`<div></div>`
  let traces = {I: I, J: J, K: K}
  for (let id of ['I','J','K'])
    ret.appendChild(spinner(traces[id], nameMap[id]))
  ret.appendChild(row)
  
  return ret
}
);
  main.variable(observer("story")).define("story", ["make_improv","spec","jointEvents","seedrandom","seed","md"], function(make_improv,spec,jointEvents,seedrandom,seed,md)
{
  let improv = make_improv(spec)
  let ret = jointEvents.map((e) => {
    let res = improv.gen('root', e)
    if (Number.isInteger(e.dt)) res = `(${res})`
    return res
  })
  let rng = new seedrandom(seed)
  return md`${ret.reduce((log, line, i) => rng() < (.9-.03*i) ?
                                        log+'  \n'+line : log+'\n\n'+line )}`
}
);
  main.variable(observer("viewof seed")).define("viewof seed", ["slider"], function(slider){return(
slider({min: 0, max: 7, step: 1,
                      description: 'rephrasing slider'})
)});
  main.variable(observer("seed")).define("seed", ["Generators", "viewof seed"], (G, _) => G.input(_));
  main.variable(observer("spinner")).define("spinner", ["html","d3","T","fade"], function(html,d3,T,fade){return(
(trace, label) => {
  let svg = html`<svg width=150 height=150></svg>`
  let sel = d3.select(svg).append('g')
                .attr('transform', 'translate(75, 75) scale(50, -50)')
  // reference axes
  sel.append('line').attr('x1', -1).attr('y1', 0)
                    .attr('x2', 1).attr('y2', 0)
  sel.append('line').attr('x1', 0).attr('y1', -1)
                    .attr('x2', 0).attr('y2', 1)
  sel.selectAll('line')
     .style('stroke', 'black').style('stroke-width', '.01px')
  
  sel.selectAll('circle')
    .data(trace)
    .join('circle')
      .attr('r', .05)
      .attr('cx', d => d[0]).attr('cy', d => d[1])
      .style('opacity', (d,i) => Math.pow(i/T, fade) )
  
  d3.select(svg).append('text')
    .attr('x', 75).attr('y', 145)
    .style('text-anchor', 'middle')
    .text(label)
  return svg
}
)});
  main.variable(observer("viewport")).define("viewport", ["html","d3","I","J","K","T","fade"], function(html,d3,I,J,K,T,fade)
{
  let svg = html`<svg width=150 height=150></svg>`
  let sel = d3.select(svg).append('g')
                .attr('transform', 'translate(75, 75) scale(50, -50)')
  
  // reference axes
  sel.append('line').attr('x1', -1).attr('y1', 0)
                    .attr('x2', 1).attr('y2', 0)
  sel.append('line').attr('x1', 0).attr('y1', -1)
                    .attr('x2', 0).attr('y2', 1)
  sel.selectAll('line')
     .style('stroke', 'black').style('stroke-width', '.01px')
  
  function color(vecs, c) {
    return vecs.map((u,t) => ({t: t, u: u, color: c}))
  }  
  sel.selectAll('circle')
    .data([...color(I, 'gray'), ...color(J, 'purple'), ...color(K, 'darkblue')])
    .join('circle')
      .attr('r', .05)
      .attr('cx', d => d.u[0]).attr('cy', d => d.u[1])
      .style('fill', d => d.color).style('opacity', d => Math.pow(d.t/T, fade) )
  
  // TODO: make debug plots like a Sims relationship panel, but sparklines.
  return svg
}
);
  main.variable(observer("viewof fade")).define("viewof fade", ["slider"], function(slider){return(
slider({min: .5, max: 2, step: .1,
                      description: 'timeseries fade'})
)});
  main.variable(observer("fade")).define("fade", ["Generators", "viewof fade"], (G, _) => G.input(_));
  main.variable(observer("viewof rawProgram")).define("viewof rawProgram", ["textarea"], function(textarea){return(
textarea({rows: 7, width: '70%', value:
`distress(T_, Delay, X, Y) :- 
  closer(T, X, X),
  Delay is T_ - T, Delay > -1.
`})
)});
  main.variable(observer("rawProgram")).define("rawProgram", ["Generators", "viewof rawProgram"], (G, _) => G.input(_));
  main.variable(observer("viewof rawBinding")).define("viewof rawBinding", ["textarea"], function(textarea){return(
textarea({rows: 3, width: '70%',
                     value: "(x,y) => ({agent: x, further: y})"
})
)});
  main.variable(observer("rawBinding")).define("rawBinding", ["Generators", "viewof rawBinding"], (G, _) => G.input(_));
  main.variable(observer("viewof rawExpressions")).define("viewof rawExpressions", ["textarea"], function(textarea){return(
textarea({rows: 3, width: '70%',
                     value: '**[cap agent] felt distressed by [further].**'
})
)});
  main.variable(observer("rawExpressions")).define("rawExpressions", ["Generators", "viewof rawExpressions"], (G, _) => G.input(_));
  main.variable(observer("viewOfInput")).define("viewOfInput", ["html","viewof rawProgram","viewof rawBinding","viewof rawExpressions"], function(html,$0,$1,$2){return(
html`
${$0}
${$1}
${$2}
`
)});
  main.variable(observer("input")).define("input", ["rawProgram","rawBinding","rawExpressions"], function(rawProgram,rawBinding,rawExpressions){return(
{program: rawProgram,
          binding: eval(rawBinding),
          expressions: rawExpressions.split('\n')}
)});
  main.variable(observer("inMatches")).define("inMatches", ["input","flatQuery"], function(input,flatQuery){return(
input.program ? flatQuery(input.program.split(':-')[0].trim()+'.') : []
)});
  main.variable(observer()).define(["md"], function(md){return(
md`#### numeric simulation`
)});
  main.variable(observer("idx")).define("idx", ["T"], function(T){return(
[...Array(T).keys()]
)});
  main.variable(observer("d3")).define("d3", ["require"], function(require){return(
require('d3@5')
)});
  main.variable(observer("x")).define("x", function(){return(
(angle) => Math.cos(angle * 2*Math.PI)
)});
  main.variable(observer("y")).define("y", function(){return(
(angle) => Math.sin(angle * 2*Math.PI)
)});
  main.variable(observer()).define(["md"], function(md){return(
md`<div style="font-size: 20px">
<span style="color: gray">I</span> <span style="color: purple">J</span> <span style="color: darkblue">K</span>
</div>

IJ, JK, KI are three relationships, each cycling at different frequencies.  
(Think of these as Romeo and Juliet phase spaces, settled into a stable limit cycle.)

Thus, the x-position of the first cycle is I's closeness to J,  
and the y-position of the third cycle is I's closeness to K.`
)});
  main.variable(observer("I")).define("I", ["trace","x","y"], function(trace,x,y){return(
trace.map(v => [x(v[0]), y(v[2])])
)});
  main.variable(observer("J")).define("J", ["trace","x","y"], function(trace,x,y){return(
trace.map(v => [x(v[1]), y(v[0])])
)});
  main.variable(observer("K")).define("K", ["trace","x","y"], function(trace,x,y){return(
trace.map(v => [x(v[2]), y(v[1])])
)});
  main.variable(observer()).define(["md"], function(md){return(
md`#### transition analysis`
)});
  main.variable(observer("getCrossings")).define("getCrossings", ["T"], function(T){return(
(pos, id) => // parse 'relationship events' out from each (n-1) dim phase space.
  pos.map(([u,v], i) => {
    if (i+1 == T) return ''
    
    let [u_,v_] = pos[i+1],
        ret = {up: '', down: ''} // look for axis-crossing events.
    if (u * u_ < 0) u_ > 0 ? ret.up += 'U' : ret.down += 'U'
    if (v * v_ < 0) v_ > 0 ? ret.up += 'V' : ret.down += 'V'
    
    // TODO: allow shifted axes per-relationship (check id).
    // STRETCH GOAL: region of meh ('closer, meh', 'closer, friends',
    //                              'further, meh', 'further, enemies')
    
    // ret.mood = u+v > 0 ? 'glad' : 'sad' // for convenience: in general, more flexible to look up by timestamp.
    return ret
  }).map((k,i) => [i,k,id])
    .filter(([i,{up: k, down: j},c]) => k || j)
)});
  main.variable(observer("timeline")).define("timeline", ["getCrossings","I","J","K"], function(getCrossings,I,J,K){return(
[...getCrossings(I, 'I'), ...getCrossings(J, 'J'), ...getCrossings(K, 'K')]
  .sort(([t,], [t_,]) => t - t_)
)});
  main.variable(observer("sample")).define("sample", ["I","J","K"], function(I,J,K){return(
function sample (who, when) {
  let sum = ([a,b]) => a+b
  let glad = {I: sum(I[when]) > 0,
              J: sum(J[when]) > 0,
              K: sum(K[when]) > 0}[who]
  return glad ? 'glad' : 'sad'
}
)});
  main.variable(observer("nameMap")).define("nameMap", function()
{  
  function shuffle(array) {
    var m = array.length, t, i;
    while (m) { // While there remain elements to shuffle…
      i = Math.floor(Math.random() * m--); // Pick a remaining element…
      t = array[m]; array[m] = array[i]; array[i] = t; // And swap it with the current element.
    } return array;
  }
  let major = shuffle(['Fool', 'Magician', 'High Priest', 'Empress', 'Emperor', 'Papess', 'Lover', 'Chariot', 'Lion Tamer', 'Hermit', 'Lady', 'Steed', 'Lord', 'Apprentice', 'Robot', 'Ritual', 'Tower', 'Shadow', 'Mage', 'Warrior', 'Thief'])
  
  return {I: 'the '+major[0], J: 'the '+major[1], K: 'the '+major[2]}
}
);
  main.variable(observer("events")).define("events", ["timeline","Identity","Predicate","sample","templates","Response","nameMap","flattenTags"], function(timeline,Identity,Predicate,sample,templates,Response,nameMap,flattenTags)
{  
  let dereferenceMap = {I: {U: 'J', V: 'K'},
                        J: {U: 'K', V: 'I'},
                        K: {U: 'I', V: 'J'}}
  
  return timeline.map(([t, {up: inc, down: dec, mood: mood}, id]) => {
    let faces = {}, tags = [],
        deref = dereferenceMap[id],
        me = new Identity(id),
        tone = new Predicate('feeling-self', sample(id, t))
    
    // closer, arity 1
    if (inc.length == 1 && dec.length == 0) {
      faces = {agent: id, closer: deref[inc[0]]}
      let theirMood = sample(faces.closer, t)
      tags = [templates['improve'], new Response(theirMood)] // closer, unary
    }
    // further, arity 1
    else if (inc.length == 0 && dec.length == 1) {
      faces = {agent: id, further: deref[dec[0]]}
      let theirMood = sample(faces.further, t)
      tags = [templates['reduce'], new Response(theirMood)]
    }
    // closer/further, arity 2
    else if (inc.length == 1 && dec.length == 1) {
      faces = {agent: id, closer: deref[inc[0]], further: deref[dec[0]]}
      tags = [templates['tradeoff']]
    }
    // closer, arity 2
    else if (inc.length == 2) {
      faces = {agent: id, closer: deref[inc[0]], alsoCloser: deref[inc[1]]}
      tags = [templates['improveBoth']] // closer, binary
    }
    // further, arity 2
    else if (dec.length == 2) {
      faces = {agent: id, further: deref[dec[0]], alsoFurther: deref[dec[1]]}
      tags = [templates['reduceBoth']]
    }
    return [{t: t, ...Object.fromEntries(Object.entries(faces).map(([k,v]) => [k, nameMap[v]]))},
            [...tags, me, tone /*, literal*/]]
    
  }).map(([cast, e]) => ({...cast, tags: flattenTags(e)}))
  
  /*let to = {agent: 'I', recipient: 'you'}, from = {agent: 'you', recipient: 'me'},
      I = new Identity('I'), J = new Identity('J')
  let ret = [
    [to, [new Approach('improve'), new Response('glad'), I]], [to, [new Approach('reduce'), new Response('glad'), I]], [from, [new Approach('reduce'), new Response('sad'), J]], [to, [new Approach('improve'), new Response('sad'), I]], [from, [new Approach('improve'), new Response('glad'), J]], [to, [new Approach('improve'), new Response('glad'), I]],
  ].map(([cast, e]) => ({...cast, tags: munge(e)})) */
}
);
  main.variable(observer()).define(["md"], function(md){return(
md`#### story sifting`
)});
  main.variable(observer("identifierMap")).define("identifierMap", ["nameMap"], function(nameMap){return(
Object.fromEntries(
  Object.values(nameMap).map(s => [s, s.toLowerCase().split(' ').slice(-1)[0]] )
)
)});
  main.variable(observer("nameOfIdentity")).define("nameOfIdentity", ["nameMap","identifierMap"], function(nameMap,identifierMap){return(
(s) => Object.values(nameMap)[Object.values(identifierMap).findIndex(v => v == s)]
)});
  main.variable(observer("program")).define("program", ["pl","events","identifierMap","programTail"], function(pl,events,identifierMap,programTail){return(
pl`
${events.map((e) => {
  let res = [],
      munge = (eventKind, deutergonist) => { res.push(
// e.g. further(sun, moon)
`${eventKind}(${e.t}, ${identifierMap[e.agent]}, ${identifierMap[deutergonist]})`
      )}  
  if (e.closer) {
    munge('closer', e.closer)
    if (e.alsoCloser)
      munge('closer', e.alsoCloser)
  }
  if (e.further) {
    munge('further', e.further)
    if (e.alsoFurther)
      munge('further', e.alsoFurther)
  }      
  return res.join('.\n')
}).filter(el => el).join('.\n') + '.'}

reciprocate(T_, Delay, Pov, Tar) :-
  closer(T, Tar, Pov),
  closer(T_, Pov, Tar),
  Delay is T_ - T, Delay > -1, Delay < 10.

distance(T_, Delay, Pov, Tar) :-
  further(T, Tar, Pov),
  further(T_, Pov, Tar),
  Delay is T_ - T, Delay > -1, Delay < 10.

neglect(T_, Delay, Pov, Down, Up) :-
  closer(T, Pov, Up),
  further(T_, Pov, Down),
  Delay is T_ - T, Delay > -1, Delay < 10.

reconcile(T_, Delay, Pov, Tar) :-
  further(T, Pov, Tar),
  closer(T_, Pov, Tar),
  Delay is T_ - T, Delay > -1.

reject(T_, Delay, Pov, Tar) :-
  closer(T, Pov, Tar),
  further(T_, Pov, Tar),
  Delay is T_ - T, Delay > -1.

${programTail}
`
)});
  main.variable(observer("programTail")).define("programTail", ["input"], function(input){return(
input ? input.program || '' : ''
)});
  main.variable(observer("inPattern")).define("inPattern", ["rawProgram"], function(rawProgram){return(
rawProgram.split('(')[0].trim()
)});
  main.variable(observer("flatQuery")).define("flatQuery", ["program"], function(program){return(
(q, f = (u) => Object.keys(u).map(k => // handle Entity and Num by default.
                                        u[k].id || u[k].value || (u[k].value == 0 ? 0 : u[k]) )
            ) => Array.from(program.query(q), s => f(s.links))
)});
  main.variable(observer("siftedTimeline")).define("siftedTimeline", ["flatQuery","input","inPattern"], function(flatQuery,input,inPattern)
{ // FIXME: generate from multiple patterns
  let ret = [ 
  ...flatQuery('reciprocate(T, DT, X, Y).').map(([t,dt,x,y]) =>
                [t, dt, 'reciprocate', {agent: x, closer: y}]),
  ...flatQuery('distance(T, DT, X, Y).').map(([t,dt,x,y]) =>
                [t, dt, 'distance', {agent: x, further: y}]),
  
  ...flatQuery('reconcile(T, DT, X, Y).').map(([t,dt,x,y]) =>
                [t, dt, 'reconcile', {agent: x, closer: y}]),
  ...flatQuery('reject(T, DT, X, Y).').map(([t,dt,x,y]) =>
                [t, dt, 'reject', {agent: x, further: y}]),
]
  if (input && input.program)
    ret.push(...flatQuery(input.program.split(':-')[0].trim()+'.').map(([t,dt,...u]) => [t, dt, inPattern, input.binding(...u)]))
  return ret.sort(([t,], [t_,]) => t-t_)
}
);
  main.variable(observer("siftedEvents")).define("siftedEvents", ["siftedTimeline","Predicate","sample","short","mid","long","nameOfIdentity","templates","flattenTags"], function(siftedTimeline,Predicate,sample,short,mid,long,nameOfIdentity,templates,flattenTags){return(
siftedTimeline.map(([t, dt, kind, u, id]) => {    
    let faces = {}, tags = [],
        // me = new Identity(id),
        tone = new Predicate('feeling-self', sample(id, t)),
        delay = dt < 3 ? short : dt < 7 ? mid : long
    
    let f = nameOfIdentity
    faces = {}
    for (let [k,v] of Object.entries(u))
      faces[k] = f(v)
    
    // faces = {agent: nameOfIdentity(id)}
    // if (u.up) faces.closer = nameOfIdentity(u.up)
    // if (u.down) faces.further = nameOfIdentity(u.down)
    
    return [{t: t, dt: dt, ...faces},
            [templates[kind], tone, delay]]
    
  }).map(([cast, e]) => ({...cast, tags: flattenTags(e)}))
)});
  main.variable(observer("jointEvents")).define("jointEvents", ["events","siftedEvents"], function(events,siftedEvents){return(
[...events, ...siftedEvents].sort(({t: t}, {t: t_}) => t-t_)
)});
  main.variable(observer("Template")).define("Template", ["Predicate"], function(Predicate){return(
class Template extends Predicate {
  constructor(instance) {
    super('template', instance)
  }
}
)});
  main.variable(observer("templates")).define("templates", ["inPattern","Template"], function(inPattern,Template){return(
Object.fromEntries(
`improve
reduce
tradeoff
improveBoth
reduceBoth
reciprocate
distance
reconcile
reject
${inPattern}`.split('\n')
.map(s => [s, new Template(s)])
)
)});
  main.variable(observer("spec")).define("spec", ["flattenTags","templates","ok","fail","short","mid","long","input","inPattern"], function(flattenTags,templates,ok,fail,short,mid,long,input,inPattern)
{  
  let ret = {}
  function spec (phrase, ...snippets) {
    ret[phrase] = {'groups': snippets}
  }
  // you'll notice some combinatorial explosion, even for our crude trace analysis.
  let snippet = (tags, ...phrases) => ({'tags': flattenTags(tags), 'phrases': phrases})
  let t = templates
  
  // the deutergonist's mood is a critical branch
  spec('root',
       snippet([t.improve, ok], '[cap agent] made [:plans-adj]plans with [closer].',
               '[cap agent] pestered [closer] into playing videogames.'),
       snippet([t.reduce, ok], '[cap agent] passed by [further] without saying anything.',
               '[cap agent] noticed [further]\'s error and said nothing.'),
       
       snippet([t.improve, fail], '[cap agent] invited [closer] to a boring party.'),
       snippet([t.reduce, fail], '[cap agent] left the room to avoid [further].'),
       
       snippet(t.tradeoff, '[cap agent] ditched [further] to hang out with [closer].'),       
       snippet(t.improveBoth, '[cap agent] helped [closer] and [alsoCloser] out on a project.'),
       snippet(t.reduceBoth, '[cap agent] imagined [further] and [alsoFurther] had their own secret language.'),
       
       /* basically redundant
       snippet([...neglect, short], '[cap agent] betrayed [cap further] suddenly, and was now with [cap closer] all the time.'),
       snippet([...neglect, mid], '[cap agent] spent more time with [cap closer], and seemed to forget about [cap further].'),
       snippet([...neglect, long], '[cap agent] drifted away from [cap further] toward [cap closer].'), */
       
       snippet([t.reciprocate, short], '[cap agent] immediately responded to [closer].'),
       snippet([t.reciprocate, mid], '[cap agent] enjoyed the company of [closer].'),
       snippet([t.reciprocate, long], '[cap agent] had warmed up over time to [closer].'),
       snippet([t.distance, short], '[cap agent] was cold to [further].'),
       snippet([t.distance, mid], '[cap agent] increasingly disregarded [further].'),
       snippet([t.distance, long], '[cap agent] had eventually given up on [further].'),
       
       snippet(t.reconcile, '[cap agent] made up with [closer].',
               '[cap agent] realized they had missed [closer].',
               '[cap agent] came to wish [closer] would return.'),
       snippet(t.reject, '[cap agent] turned on [further].',
               '[cap agent] spent less and less time with [further].',
               '[cap agent] drifted away from [further].'),
      ) // end of spec 'root'
  
  if (input && input.expressions)
    ret.root.groups.push(
      snippet(t[inPattern], ...input.expressions)
    )
  
  spec('plans-adj',
       snippet([], '', 'new ', 'secret ', 'unremarkable ')
       )
  return ret
}
);
  main.variable(observer()).define(["md"], function(md){return(
md`#### template expansion`
)});
  main.variable(observer("Predicate")).define("Predicate", function(){return(
class Predicate {
  constructor(kind, instance) {
    this.kind = kind
    this.instance = instance
  }
}
)});
  main.variable(observer("Response")).define("Response", ["Predicate"], function(Predicate){return(
class Response extends Predicate {
  constructor(instance) {
    super('feeling-respond', instance)
  }
}
)});
  main.variable(observer("ok")).define("ok", ["Response"], function(Response){return(
new Response('glad')
)});
  main.variable(observer("fail")).define("fail", ["Response"], function(Response){return(
new Response('sad')
)});
  main.variable(observer("Identity")).define("Identity", ["Predicate"], function(Predicate){return(
class Identity extends Predicate {
  constructor(instance) {
    super('identity', instance)
  }
}
)});
  main.variable(observer("Delay")).define("Delay", ["Predicate"], function(Predicate){return(
class Delay extends Predicate {
  constructor(instance) {
    super('delay', instance)
  }
}
)});
  main.variable(observer("short")).define("short", ["Delay"], function(Delay){return(
new Delay('short')
)});
  main.variable(observer("mid")).define("mid", ["Delay"], function(Delay){return(
new Delay('mid')
)});
  main.variable(observer("long")).define("long", ["Delay"], function(Delay){return(
new Delay('long')
)});
  main.variable(observer("flattenTags")).define("flattenTags", ["Predicate"], function(Predicate){return(
function flattenTags (tags) {
  if (!tags)
    return []
  else if (tags instanceof Predicate)
    return [[tags.kind, tags.instance]]
  else if (!(tags instanceof Array))
    return [tags]
  else if (tags[0] instanceof Predicate) // crude type inference
    return tags.map(t => [t.kind, t.instance])
}
)});
  main.variable(observer()).define(["md"], function(md){return(
md`#### libraries`
)});
  main.variable(observer("make_improv")).define("make_improv", ["Improv","seedrandom","seed"], function(Improv,seedrandom,seed){return(
(spec) => new Improv(spec, {
    filters: [// Improv.filters.dryness(), // surface variations don't suffice for non-repetition?
              Improv.filters.mismatchFilter()],
    // reincorporate: true,
    rng: new seedrandom(seed), // deterministic output 
  })
)});
  main.variable(observer("Improv")).define("Improv", ["FileAttachment","require"], async function(FileAttachment,require)
{ // model-driven textgen by @sequitur
  let response = FileAttachment("improv.js")
  // avait fetch('https://gist.githubusercontent.com/JazzTap/b07a4e613c981c35a45e92707ea4dcdd/raw/801adc40d5b4a15a735e361448ef739acc9888b7/improv.jsv
  const blob = await response.blob()
  return require(URL.createObjectURL(blob))
}
);
  main.variable(observer("seedrandom")).define("seedrandom", ["FileAttachment","require"], async function(FileAttachment,require)
{
  const response = FileAttachment("seedrandom.min.js")
  const blob = await response.blob()
  return require(URL.createObjectURL(blob))
}
);
  const child1 = runtime.module(define1);
  main.import("pl", child1);
  const child2 = runtime.module(define2);
  main.import("slider", child2);
  main.import("radio", child2);
  main.import("textarea", child2);
  return main;
}
