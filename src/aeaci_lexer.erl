%%%-------------------------------------------------------------------
%%% @author gorbak25
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2020 13:51
%%%-------------------------------------------------------------------
-module(aeaci_lexer).

%% API
-export([string/1]).

%% To simplify expression parsing lex the string
%% This is not the FULL Sophia lexer - It's a simpler variant designed
%% for call data encoding/decoding
%% This library should avoid linking with aesophia... Don't reuse the existing erlang
%% Sophia lexer or parser which are unbelievably slow.
-type lex_token() ::
      paren_start
    | paren_end
    | list_start
    | list_end
    | record_start
    | record_end
    | minus_sign
    | multiply
    | type_annotation
    | {con, string()}
    | {id, string()}
    | {string, string()}
    | {char, char()}
    | {int, integer()}
    | {hex, integer()}
    | {bytes, binary()}
    | dot
    | comma
    | equal.

-type pos() :: integer().

-export_type([lex_token/0, pos/0]).

-spec string(string()) -> {ok, [{lex_token(), pos()}]} | {error, term()}.
%% TODO: Nice error messsages :P
string(String) ->
    case skip_whitespace(String, [], [], 0) of
        {ok, _} = Res ->
            Res;
        {error, Pos, Reason} ->
            {error, {lexer_error, io_lib:format("While scanning ~s\n\nLexer emitted ~s on pos ~p\n", [String, Reason, Pos])}}
    end.

-define(IS_FORBIDDEN_CHAR(CHAR), (CHAR =:= $\n)).
-define(IS_FORBIDDEN_OP(CHAR), (
    CHAR =:= $!
    orelse CHAR =:= $<
    orelse CHAR =:= $>
    orelse CHAR =:= $+
    orelse CHAR =:= $*
    orelse CHAR =:= $/
    orelse CHAR =:= $:
    orelse CHAR =:= $&
    orelse CHAR =:= $|
    orelse CHAR =:= $?
    orelse CHAR =:= $~
    orelse CHAR =:= $@
    orelse CHAR =:= $^
  )).
-define(IS_WS(CHAR), (CHAR >= 0 andalso CHAR =< $ )).
-define(IS_DIGIT(CHAR), (CHAR >= $0 andalso CHAR =< $9)).
-define(IS_HEXDIGIT(CHAR), (
    ?IS_DIGIT(CHAR)
    orelse (CHAR >= $a andalso CHAR =< $f)
    orelse (CHAR >= $A andalso CHAR =< $F))).
-define(IS_LOWER(CHAR), ((CHAR >= $a andalso CHAR =< $z) orelse CHAR =:= $_)).
-define(IS_UPPER(CHAR), (CHAR >= $A andalso CHAR =< $Z)).
-define(IS_CON(CHAR), ?IS_UPPER(CHAR); ?IS_LOWER(CHAR); ?IS_DIGIT(CHAR); CHAR =:= $_).
-define(IS_ID(CHAR), ?IS_CON(CHAR); CHAR =:= $').

-define(skip_whitespace(String, Stack, Acc), ?skip_whitespace(String, Stack, Acc, 1)).
-define(skip_whitespace(String, Stack, Acc, Delta), skip_whitespace(String, Stack, Acc, N+Delta)).
-define(lex_string_(State, String, Stack, Acc), ?lex_string_(State, String, Stack, Acc, 1)).
-define(lex_string_(State, String, Stack, Acc, Delta), lex_string_(State, String, Stack, Acc, N+Delta)).
-define(emit(Token, List), [{Token, N}|List]).

%% Skips whitespaces
-type stack() :: [{lex_token(), pos()}].
-spec skip_whitespace(string(), stack(), stack(), pos()) -> {ok, stack()} | {error, pos(), term()}.
skip_whitespace([], Stack, Acc, N) ->
    ?lex_string_(dispatch, [], Stack, Acc, 0);
skip_whitespace([Char | _], _Stack, _Acc, N) when ?IS_FORBIDDEN_CHAR(Char) ->
    {error, N, "Newline forbidden, please escape newlines"};
skip_whitespace([Char | String], Stack, Acc, N) when ?IS_WS(Char) ->
    ?skip_whitespace(String, Stack, Acc);
skip_whitespace(String, Stack, Acc, N) ->
    ?lex_string_(dispatch, String, Stack, Acc, 0).

%% Just hardcode a state machine :)
%% States: dispatch, comment, string, char, int, hex, bytes, id, con

%% Forbidden chars in a one line call expression - forbid them - there is no place for them
%% when encoding call data
lex_string_(_, [Char | _], _Stack, _Acc, N) when ?IS_FORBIDDEN_CHAR(Char) ->
    {error, N, "Newline forbidden, please escape newlines"};

%% Dispatch lexing operation
lex_string_(dispatch, [], [], Acc, _N) -> %% Lexing done
    {ok, lists:reverse(Acc)};
lex_string_(dispatch, [], [{paren_start, Pos} | _], _Acc, _N) ->
    {error, Pos, "Unclosed parentheses"};
lex_string_(dispatch, [], [{list_start, Pos} | _], _Acc, _N) ->
    {error, Pos, "Unclosed list"};
lex_string_(dispatch, [], [{record_start, Pos} | _], _Acc, _N) ->
    {error, Pos, "Unclosed record/map"};
%% Parantheses
lex_string_(dispatch, [$( | String], Stack, Acc, N) ->
    ?skip_whitespace(String, ?emit(paren_start, Stack), ?emit(paren_start, Acc));
lex_string_(dispatch, [$) | String], [{paren_start, _} | Stack], Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(paren_end, Acc));
%% Lists
lex_string_(dispatch, [$[ | String], Stack, Acc, N) ->
    ?skip_whitespace(String, ?emit(list_start, Stack), ?emit(list_start, Acc));
lex_string_(dispatch, [$] | String], [{list_start, _} | Stack], Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(list_end, Acc));
%% Records/Maps
lex_string_(dispatch, [${ | String], Stack, Acc, N) ->
    ?skip_whitespace(String, ?emit(record_start, Stack), ?emit(record_start, Acc));
lex_string_(dispatch, [$} | String], [{record_start, _} | Stack], Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(record_end, Acc));
%% Unclosed braces/lists/parantheses
lex_string_(dispatch, [Char|_], [{paren_start, Pos} | _], _Acc, _N) when Char =:= $); Char =:= $]; Char =:= $} ->
    {error, Pos, "Unclosed parentheses"};
lex_string_(dispatch, [Char|_], [{list_start, Pos} | _], _Acc, _N) when Char =:= $); Char =:= $]; Char =:= $} ->
    {error, Pos, "Unclosed list"};
lex_string_(dispatch, [Char|_], [{record_start, Pos} | _], _Acc, _N) when Char =:= $); Char =:= $]; Char =:= $} ->
    {error, Pos, "Unclosed record/map"};

%% Comment start
lex_string_(dispatch, [$/, $/ | _], _Stack, _Acc, N) ->
    {error, N, "Single line comments are disallowed when encoding call data"};
lex_string_(dispatch, [$/, $* | String], Stack, Acc, N) ->
    ?lex_string_(comment, String, Stack, Acc, 2);
%% Lambdas
lex_string_(dispatch, [$=, $> | _], _Stack, _Acc, N) ->
    {error, N, "Lambda expressions are forbidden when encoding call data"};
%% Expr
lex_string_(dispatch, [$- | _], _Stack, [{minus_sign, _} | _], N) ->
    {error, N, "Only one minus sign before numerals"};
lex_string_(dispatch, [$- | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(minus_sign, Acc));
lex_string_(dispatch, [$: | _], _Stack, [{type_annotation, _} | _], N) ->
    {error, N, "Expected type adnottation"};
lex_string_(dispatch, [$: | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(type_annotation, Acc));
lex_string_(dispatch, [$* | _], _Stack, [{multiply, _} | _], N) ->
    {error, N, "Expected typedef"};
lex_string_(dispatch, [$* | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(multiply, Acc));
lex_string_(dispatch, [$. | String], Stack, [{{con, _}, _} | _] = Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(dot, Acc));
lex_string_(dispatch, [$. | _], _Stack, _Acc, N) ->
    {error, N, "Expected qualifierier"};
lex_string_(dispatch, [$, | _], _Stack, [{Top, _} | _], N) when Top =:= dot; Top =:= comma ->
    {error, N, "Expected literal"};
lex_string_(dispatch, [$, | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(comma, Acc));
lex_string_(dispatch, [$= | String], Stack, [{{id, _}, _} | _] = Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(equal, Acc));
lex_string_(dispatch, [$= | String], Stack, [{list_end, _} | _] = Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit(equal, Acc));
lex_string_(dispatch, [$= | _], _Stack, _Acc, N) ->
    {error, N, "Expected named argument or map"};
lex_string_(dispatch, [Char | _], _Stack, _Acc, N) when ?IS_FORBIDDEN_OP(Char) ->
    {error, N, "Non literal expressions are forbidden"};
%% Hex
lex_string_(dispatch, [$0, $x | [Char | _] = String], Stack, Acc, N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(hex, String, Stack, ?emit({hex, 0}, Acc), 2);
%% Integer
lex_string_(dispatch, [Char | _] = String, Stack, Acc, N) when ?IS_DIGIT(Char) ->
    ?lex_string_(int, String, Stack, ?emit({int, 0}, Acc), 0);
%% Bytes
lex_string_(dispatch, [$# | [Char | _] = String], Stack, Acc, N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(bytes, String, Stack, ?emit({int, 0, 0}, Acc));
%% Con
lex_string_(dispatch, [Char | String], Stack, Acc, N) when ?IS_UPPER(Char) ->
    ?lex_string_(con, String, Stack, ?emit({con, [Char]}, Acc));
%% Id
lex_string_(dispatch, [Char | String], Stack, Acc, N) when ?IS_LOWER(Char) ->
    ?lex_string_(id, String, Stack, ?emit({id, [Char]}, Acc));
%% String
lex_string_(dispatch, [$" | String], Stack, Acc, N) ->
    ?lex_string_(string, String, Stack, ?emit({string, []}, Acc));
%% Char
lex_string_(dispatch, [$' | String], Stack, Acc, N) ->
    ?lex_string_(char, String, Stack, Acc);
%% Catch all - unhandled case
lex_string_(dispatch, _String, _Stack, _Acc, N) ->
    {error, N, "Unexpected character"};

%% Comment handling
lex_string_(comment, [$*, $/ | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, Acc, 2);
lex_string_(comment, [_ | String], Stack, Acc, N) ->
    ?lex_string_(comment, String, Stack, Acc);
lex_string_(comment, [], _Stack, _Acc, N) ->
    {error, N, "Got EOF while searching comment end"};

%% Integer handling
lex_string_(int, [Char | String], Stack, [{{int, Val}, Pos}|Acc], N) when ?IS_DIGIT(Char) ->
    ?lex_string_(int, String, Stack, [{{int, Val*10 + Char - $0}, Pos} | Acc]);
lex_string_(int, [$_ | [Char | _] = String], Stack, Acc, N) when ?IS_DIGIT(Char) ->
    ?lex_string_(int, String, Stack, Acc);
lex_string_(int, [$_ | _], _Stack, _Acc, N) ->
    {error, N, "Integer separator must precede numeric character"};
lex_string_(int, String, Stack, [{{int, Val}, Pos}, {minus_sign, _} | Acc], N) ->
    ?skip_whitespace(String, Stack, [{{int, -Val}, Pos} | Acc], 0);
lex_string_(int, String, Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, Acc, 0);

%% Hex handling
lex_string_(hex, [Char | String], Stack, [{{hex, Val}, Pos}|Acc], N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(hex, String, Stack, [{{hex, Val*16 + hex_char_to_val(Char)}, Pos} | Acc]);
lex_string_(hex, [$_ | [Char | _] = String], Stack, Acc, N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(hex, String, Stack, Acc);
lex_string_(hex, [$_ | _], _Stack, _Acc, N) ->
    {error, N, "Hex separator must precede hex character"};
lex_string_(hex, String, Stack, [{{hex, Val}, Pos}, {minus_sign, _} | Acc], N) ->
    ?skip_whitespace(String, Stack, [{{hex, -Val}, Pos} | Acc], 0);
lex_string_(hex, String, Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, Acc, 0);

%% Bytes handling
lex_string_(bytes, [Char | String], Stack, [{{int, Val, Len}, Pos} | Acc], N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(bytes, String, Stack, [{{int, Val*16 + hex_char_to_val(Char), Len + 1}, Pos} | Acc]);
lex_string_(bytes, [$_ | [Char | _] = String], Stack, Acc, N) when ?IS_HEXDIGIT(Char) ->
    ?lex_string_(hex, String, Stack, Acc);
lex_string_(bytes, [$_ | _], _Stack, _Acc, N) ->
    {error, N, "Bytes separator must precede hex character"};
lex_string_(bytes, String, Stack, [{{int, Val, Len}, Pos} | Acc], N) ->
    Digits = (Len + 1) div 2,
    ?skip_whitespace(String, Stack, [{{bytes, <<Val:Digits/unit:8>>}, Pos} | Acc], 0);

%% Con
lex_string_(con, [Char | String], Stack, [{{con, Con}, Pos} | Acc], N) when ?IS_CON(Char) ->
    ?lex_string_(con, String, Stack, [{{con, [Char | Con]}, Pos} | Acc]);
lex_string_(con, String, Stack, [{{con, Con}, Pos} | Acc], N) ->
    ?skip_whitespace(String, Stack, [{{con, lists:reverse(Con)}, Pos} | Acc], 0);

%% Id
lex_string_(id, [Char | String], Stack, [{{id, Id}, Pos} | Acc], N) when ?IS_ID(Char) ->
    ?lex_string_(id, String, Stack, [{{id, [Char | Id]}, Pos} | Acc]);
lex_string_(id, String, Stack, [{{id, Id}, Pos} | Acc], N) ->
    ?skip_whitespace(String, Stack, [{{id, lists:reverse(Id)}, Pos} | Acc], 0);

%% String
lex_string_(string, [$" | String], Stack, [{{string, S}, Pos} | Acc], N) ->
    ?skip_whitespace(String, Stack, [{{string, lists:reverse(S)}, Pos} | Acc]);
lex_string_(string, [$\\, $x, D1, D2 | String], Stack, [{{string, S}, Pos} | Acc], N) when ?IS_HEXDIGIT(D1), ?IS_HEXDIGIT(D2) ->
    C = list_to_integer([D1, D2], 16),
    ?lex_string_(string, String, Stack, [{{string, [C | S]}, Pos} | Acc], 4);
lex_string_(string, [$\\, Code | String], Stack, [{{string, S}, Pos} | Acc], N) ->
    Char = case Code of
        $'  -> $';
        $\\ -> $\\;
        $b  -> $\b;
        $e  -> $\e;
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $t  -> $\t;
        $v  -> $\v;
        _   -> {error, N, "\\"++[Code]++" Is an unknown escape code"}
    end,
    case Char of
        {error, _, _} = Err ->
            Err;
        _ ->
            ?lex_string_(string, String, Stack, [{{string, [Char | S]}, Pos} | Acc], 2)
    end;
lex_string_(string, [Char | String], Stack, [{{string, S}, Pos} | Acc], N) ->
    ?lex_string_(string, String, Stack, [{{string, [Char | S]}, Pos} | Acc]);
lex_string_(string, [], _Stack, [{{string, _}, Pos} | _], _N) ->
    {error, Pos, "Got EOF while searching string end"};

%% Char
lex_string_(char, [$\\, Code, $' | String], Stack, Acc, N) ->
    Char = case Code of
        $'  -> $';
        $\\ -> $\\;
        $b  -> $\b;
        $e  -> $\e;
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $t  -> $\t;
        $v  -> $\v;
        _   -> {error, N, "\\"++[Code]++" Is an unknown escape code"}
    end,
    case Char of
        {error, _, _} = Err ->
            Err;
        _ ->
            ?skip_whitespace(String, Stack, ?emit({char, Char}, Acc), 3)
    end;
lex_string_(char, [Char, $' | String], Stack, Acc, N) ->
    ?skip_whitespace(String, Stack, ?emit({char, Char}, Acc), 2);
lex_string_(char, [], _Stack, _Acc, N) ->
    {error, N, "Got EOF when scanning char"};
lex_string_(char, _String, _Stack, _Acc, N) ->
    {error, N, "Invalid char"};

lex_string_(State, String, Stack, Acc, N) ->
    {error, N, io_lib:format(
        "Lexer bug in state ~p when scanning ~p with braces stack ~p and token stack ~p. If you see this message then please submit a bug report.",
        [State, String, Stack, Acc])}.

hex_char_to_val(Char) when ?IS_DIGIT(Char) ->
    Char - $0;
hex_char_to_val(Char) ->
    string:to_lower(Char) - $a + 10.
