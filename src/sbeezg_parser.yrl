%%% -grammar(sbeezg_parser).
%%% -vsn('2002.0317').
%%% -author('cpressey@gmail.com').
%%% -copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%  1. Redistributions of source code must retain the above copyright
%%%     notices, this list of conditions and the following disclaimer.
%%%  2. Redistributions in binary form must reproduce the above copyright
%%%     notices, this list of conditions, and the following disclaimer in
%%%     the documentation and/or other materials provided with the
%%%     distribution.
%%%  3. Neither the names of the copyright holders nor the names of their
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

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
