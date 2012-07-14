-module(sbeezg).
-vsn('2002.0317').
-author('cpressey@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

-export([run/1, test/1]).

%%% BEGIN sbeezg.erl %%%

%%% A simple interpreter for the Sbeezg Programming Language.

%% Interpreter ---------------------------------------------------------
%% Call sbeezg:run("sbeezg-program") to interpret a Sbeezg program.
%% Empties the process dictionary as a side effect.

run(String) when list(String) ->
  {ok, Toks, N} = erl_scan:string(String),
  % io:fwrite("scan: ~s -> ~w~n", [String, Toks]),
  erase(), % clear process dictionary for parser
  case sbeezg_parser:parse(Toks ++ [{'$end',999}]) of
    {ok, Prog} -> 
      % io:fwrite("parse: ~s -> ~w~n", [String, Prog]),
      erase(), % destroy the garbage left in the process dictionary
      run(Prog);
    {error, {Line, Module, Message}}=Q ->
      io:fwrite("Error: ~s~n", [Message]),
      Q
  end;
run({assign, L, F, A}=C) ->
  % io:fwrite("~w~n", [C]),
  L0 = get_name(L),
  F0 = run(F),
  A0 = run(A),
  assign(L0, '$placeholder'),
  R0 = execute(F0, A0),
  assign(L0, R0);
run({assign, L, R}=C) ->
  % io:fwrite("~w~n", [C]),
  L0 = get_name(L),
  R0 = run(R),
  assign(L0, R0);
run({vlist, H, T}=C) ->
  % io:fwrite("~w~n", [C]),
  H0 = run(H),
  T0 = run(T),
  [H0 | T0];
run({lambda, N, A, R}=C) ->
  % io:fwrite("~w~n", [C]),
  {func, N, A, R};
run({alist, H, T}=C) ->
  % io:fwrite("~w~n", [C]),
  H0 = run(H),
  T0 = run(T),
  case T0 of
    [] -> H0;
    _  -> T0
  end;
run({lit,{atom,Line,Atom}}) -> Atom;
run({lit,{integer,Line,Int}}) -> Int;
run({name, {atom,Line,Atom}}) ->
  case get(Atom) of
    undefined ->
      io:fwrite("~w undefined!~n", [Atom]),
      Atom;
    N ->
      N
  end;
run(nil) ->
  [];
run(Q) ->
  io:fwrite("UNKNOWN ~w~n", [Q]),
  unknown.

get_name({nlist, H, T}=C) ->
  % io:fwrite("~w~n", [C]),
  H0 = get_name(H),
  T0 = get_name(T),
  [H0 | T0];
get_name({name, {atom,Line,Atom}}) -> Atom;
get_name(nil) -> [].

%% Utility -------------------------------------------------------------

assign(Name, Value) ->
  % io:fwrite("Assigning ~w to ~w~n", [Value, Name]),
  case get(Name) of
    NonExist when NonExist == undefined; NonExist == '$placeholder' ->
      put(Name, Value);
    Exist ->
      io:fwrite("ERROR: Multiple Assignment to ~w=~w (~w)~n",
        [Name, Exist, Value])
  end,
  Value.
  
execute(print, [H | T]) ->
  io:fwrite("~w", [H]),
  execute(print, T);
execute(print, []) -> ok;
execute(is, [A, B, T, F]) ->
  % io:fwrite("~w", [C]),
  case A of
    B -> T;
    _ -> F
  end;
execute(pred, [X]) ->
  % io:fwrite("pred ~w~n", [X]),
  X - 1;
execute(succ, [X]) -> X + 1;
execute(Func, Args) when atom(Func) ->
  io:fwrite("Unknown built-in '~w(~w)'~n", [Func, Args]),
  unknown;
execute({func, P, B, R}, A) ->
  P0 = fresh(P),
  B0 = fresh(B),
  P1 = get_name(P0),
  bind(P1, A),
  run(B0),
  run(R).

fresh({assign, L, F, A}=C) -> {assign, fresh(L), fresh(F), fresh(A)};
fresh({assign, L, R}=C)    -> {assign, fresh(L), fresh(R)};
fresh({vlist, H, T}=C)     -> {vlist, fresh(H), fresh(T)};
fresh({lambda, X, A}=C)    -> {lambda, fresh(X), fresh(A)};
fresh({nlist, H, T}=C)     -> {nlist, fresh(H), fresh(T)};
fresh({alist, H, T}=C)     -> {alist, fresh(H), fresh(T)};
fresh({lit,X}=C)           -> C;
fresh({name,{atom,Line,Atom}}=C)  ->
  case get(Atom) of
    undefined -> C;
    _ ->
      fresh({name, {atom, Line, list_to_atom([$_ | atom_to_list(Atom)])}})
  end;
fresh(X)                   -> X.

bind([],[]) -> ok;
bind([HN|TN],[HV|TV]) ->
  % io:fwrite("bind ~w to ~w~n", [HV,HN]),
  assign(HN,HV),
  bind(TN,TV).

%% User interface ------------------------------------------------------

test([N]) -> test(list_to_integer(N));
test(N) ->
  R = run(prg(N)),
  E = erase(),
  {R, E}.

prg(1) -> "a={b|p=*print;c=p(b)|b};d=a(*foo);e=a(*bar);f=a(*baz)";
prg(2) -> "a={b|p=*print;c=p(b)|b};d={e,f|g=e(f)|g};h=d(a,*hello)";
prg(3) -> "p=*print;i=*is;r=i(*a,*b,*troo,*fall);q=p(r)";
prg(4) -> "f={a,b|p=*print;g=p(a);k=b(a,b)|a};l=f(*hello,f)";
prg(5) -> "f={a,b|i=*is;s=*pred;p=*print;g=p(*beer);h=s(a);"
          "ln={x,m|z=x|x};lg={y,n|q=n(y,n)|y};j=i(h,0,ln,lg);"
	  "k=j(h,b)|a};l=f(99,f)";
prg(6) -> "f=*a;f=*b";  % intentionally erroneous
prg(_) -> unknown.

%%% END of sbeezg.erl %%%
