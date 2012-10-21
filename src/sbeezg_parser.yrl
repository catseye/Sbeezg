%%% -grammar(sbeezg_parser).
%%% -vsn('2002.0317').
%%% -author('cpressey@gmail.com').
%%% -copyright('This work is in the public domain; see UNLICENSE for more info').

%%% BEGIN sbeezg_parser.yrl %%%

Nonterminals vlist val nlist name alist appl.
Terminals '=' '*' '(' ')' '{' '|' '}' ',' ';' atom integer.
Rootsymbol alist.

%%% EBNF:
%%% Appl ::= Name "=" Val "(" Val  {"," Val} ")".
%%% Val  ::= Name | "*" Const | "{" Name {"," Name} "|" Appl {";" Appl} "|" Name "}".

%%% Recursive Version (no {}'s):
%%% Appl  ::= Name "=" Val ["(" Vlist ")"].
%%% Vlist ::= Val ["," Vlist].
%%% Val   ::= Name | "*" Name | "{" Nlist "|" Alist "|" Name "}".
%%% Nlist ::= Name ["," Nlist].
%%% Alist ::= Appl [";" Alist].

appl  -> name '=' val '(' vlist ')'       : new_name('$1'), {assign, '$1', '$3', '$5'}.
appl  -> name '=' val                     : new_name('$1'), {assign, '$1', '$3'}.
vlist -> val ',' vlist                    : {vlist, '$1', '$3'}.
vlist -> val                              : {vlist, '$1', nil}.
val   -> '{' nlist '|' alist '|' name '}' : {lambda, '$2', '$4', '$6'}.
val   -> '*' atom                         : {lit, '$2'}.
val   -> integer                          : {lit, '$1'}.
val   -> name                             : existing_name('$1'), '$1'.
nlist -> name ',' nlist                   : new_name('$1'), {nlist, '$1', '$3'}.
nlist -> name                             : new_name('$1'), {nlist, '$1', nil}.
alist -> appl ';' alist                   : {alist, '$1', '$3'}.
alist -> appl                             : {alist, '$1', nil}.
name  -> atom                             : {name, '$1'}.

Erlang code.

new_name({name,{atom,Line,Name}}=A) ->
  case get(A) of
    defn ->
      return_error(0, io_lib:format("Name '~w' already defined", [Name]));
    _ ->
      put(A, defn)
  end.

existing_name({name,{atom,Line,Name}}=A) ->
  case get(A) of
    undefined ->
      return_error(0, io_lib:format("Name '~w' is not yet defined", [Name]));
    _ ->
      ok
  end.

%%% END of sbeezg_parser.yrl %%%
