---
title: Rediscovering Erlang
description: In which I return to Erlang and discover OTP
tags: programming, erlang
...

## Erlang – My first functional language

My first introduction to functional programming was a university course simply
called "Programming Languages". At that time I already used quite a few
programming languages and  I then believed to them be very different. I
therefore assumed the class would be really easy. That did not turn out to be
the case, however. Faced with variables that didn't vary, *fold*s, *map*s and no
*for* loops, I was struggling for months until it finally, gradually started to
make sense. It was even...  elegant, and beautiful.

The language we used for the course was [the Oz programming
language](http://en.wikipedia.org/wiki/Oz_(programming_language)), which worked
nicely for teaching purposes, but it did not seem to be widely used. However,
one of the last lectures was a guest lecture by a Swedish Erlang programmer, and
having seen a language that was actually *used*, I decided that I should learn
Erlang to explore more of this new, strange world of functional programming.

I picked up "Programming Erlang" from the library and started to play around
with Erlang. I remember finding the syntax quite weird and the programming model
was different from anything I had seen before, but it was fun to play around
with. However, the my dalliance with Erlang ended when a friend suggested that I
should look at Haskell as well if my goal was to learn about functional
programming.  That was just before Christmas, so during my Christmas holiday I
started reading [the Haskell wiki book](http://en.wikibooks.org/wiki/Haskell).
Haskell -- even with its idiosyncrasies -- was pure beauty and elegance, and the
type system was more powerful than anything I had seen before.  But enough about
Haskell. This post is about how I recently started looking at Erlang again.

## Revisiting Erlang

I  picked up ["Introducing Erlang"](http://amzn.to/1c1uXrD) by Simon St.
Laurent last week, which turned out to be a great, quick introduction to Erlang
and OTP^[Short for "Open Telecom Platform" -- but not at all
telephony-specific.].  The Erlang syntax didn't bother me at all this time (I
remember finding it rather strange the first time I looked at it), and playing
around by writing small services was a lot of fun. I especially liked learning
about OTP which is (1) a set of libraries, (2) behaviours (basically
"interfaces" for Erlang) and (3) some conventions on how to write services.

One of the most commonly used OTP behaviours is `gen_server` (general server).
To use this behaviour you would specify that your program behaves like a
`gen_server` by adding the pragma `-behaviour(gen_server).`{.erlang} to your
source file. By doing this you tell the compiler that you intend to adhere to
this behaviour, which in practice means implementing a specific set of
functions. (This will be checked by the compiler.) The code for a process is
thus split into a **behaviour** (a part of OTP) and a **callback module** that
will implement the application-specific parts.

There is a [great post to the *erlang-quesitons*
list](http://erlang.org/pipermail/erlang-questions/2011-April/057777.html) from
2011 by Joe Armstrong, Erlang's creator, explaining how the `gen_server` OTP
behaviour works behind the scenes.

Besides some initialization work (basically registering under a given name and
then calling back to the callback module's `init` function), a `gen_server` runs
an infinite, tail-recursive function that calls itself, explicitly passing state
from one call to the next. Its initial state is what's returned by the calling
module's `init` function. This looping function will wait for messages and
whenever it receives a message it will call the callback module's `handle_call`
function and update its state to what was returned by that function.

The following `mini_gs` module is used to illustrate how `gen_server` works
behind the scenes^[I have slightly modified the code, mostly to add more
comments.]:

```erlang
-module(mini_gs).
-export([start_link/4, call/2]).

%% this module behaves just like the gen-server
%% for a sub-set of the gen_server commands

start_link({local,Name}, Mod, Args, _Opts) ->
    register(Name, spawn(fun() -> start(Mod, Args) end)).

% a call to the mini_gs server “Name” is passed on to that server, and we then
% wait for its reply and return it. `make_ref` returns a unique id, so that we
% can tell the messages apart.
call(Name, X) ->
    Name ! {self(), Ref = make_ref(), X},
    receive
     {Ref, Reply} -> Reply
    end.

% call the callback module's `init` function and use its return value as our
% initial state
start(Mod, Args) ->
   {ok, State} = Mod:init(Args),
   loop(Mod, State).

% loop forever, passing calls on to the callback module's `handle_call`
% function, update our state, and pass it on to the recursive call.
loop(Mod, State) ->
   receive
   {From, Tag, X} ->
      case Mod:handle_call(X, From, State) of
      {reply, R, State1} ->
          From ! {Tag, R},
          loop(Mod, State1)
      end
  end.
```

So that would be the **behaviour**. And the following key-value server is an
example of a callback module:

```erlang
-module(kv).
% -behaviour(mini_gs). % would work if `mini_gs` were a part of OTP

%% These define the client API
-export([start/0, store/2, lookup/1]).

%% these must be defined because they are called by mini_gs
-export([init/1, handle_call/3]).

%% define the client API
start()        -> mini_gs:start_link({local,someatom}, kv, foo, []).
store(Key,Val) -> mini_gs:call(someatom, {putval,Key,Val}).
lookup(Key)    -> mini_gs:call(someatom, {getval,Key}).

%% define the internal routines
init(foo) -> {ok, dict:new()}. % The initial state is an empty dictionary

handle_call({putval, Key, Val}, _From, Dict) ->
   {reply, ok, dict:store(Key, Val, Dict)};
handle_call({getval,Key}, _From, Dict) ->
  {reply, dict:find(Key, Dict), Dict}.
```

Why this is so cool is best explained by Joe Armstrong himself:

> So now we have turned a single process key-value store (using `dict`) into a
> global key-value store.  Note that `kv.erl` never uses the primitives
> `spawn_link`, `send`, `receive` or so on. i.e. `kv.erl` is written with pure
> *sequential* code.
>
> This is *why* we made the `gen_server` abstraction. You can write well-typed
> sequential code (the `handle_call` and `init` functions) to parametrize a
> concurrent behavior, i.e. you need to know nothing about concurrency to get
> the job done. We've "abstracted out" the concurrency.

(If this explanation was too terse, I recommend [Steve Vinoski's talk on OTP
from QCon New York
2012](http://www.infoq.com/presentations/Erlang-OTP-Behaviors). Actually, watch
it anyway -- it's a great talk.)

## (Not really a) conclusion

So, to summarize a bit: What I have seen so far in my recent re-visit of Erlang
has been really positive. Maybe most important of all: Playing around with it
has been *fun*! There are definitely some rough spots in the language, like the
lack of a real type system[^dialyzer]; a weak record system; and the slightly
annoying issue of having to change statement terminators (i.e. ".", "," and ";")
when moving code around, but being new to the language and the ecosystem, I'm
not even sure they will be valid complaints as I learn more.

[^dialyzer]: The [Erlang dialyzer](http://www.erlang.org/doc/man/dialyzer.html)
might ameliorate this to some extent. It's described as *a static analysis
tool that identifies software discrepancies such as definite type errors, code
which has become dead or unreachable due to some programming error, unnecessary
tests, etc.*
