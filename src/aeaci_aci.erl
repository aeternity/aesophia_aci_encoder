%%%-------------------------------------------------------------------
%%% @author gorbak25
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%% Created : 10. Sep 2020 16:36
%%%-------------------------------------------------------------------
-module(aeaci_aci).

%% API
-export([file/1, lex_string/1]).

-record(scope, { functions :: #{ binary() => {[binary()], binary()} }
               , types :: #{ binary() => term() }
               }).
%% Data type for encoding/decoding call data for a contract using the provided ACI
-record(contract_aci, { scopes :: #{ binary() => #scope{} }, opts :: map() }).

-spec file(string()) -> #contract_aci{}.
file(Filename) ->
  file(Filename, []).

file(Filename, Opts) ->
    {ok, JText} = file:read_file(Filename),
    Contracts =
              case jsx:decode(JText, [{labels, atom}, return_maps]) of
                  JArray when is_list(JArray)  -> JArray;
                  JObject when is_map(JObject) -> [JObject];
                  _                            -> error(bad_aci_json)
              end,
    Scopes = [parse_contract(Contract) || Contract <- Contracts],
    #contract_aci{scopes = maps:from_list([S || S <- Scopes, is_tuple(S)]), opts = Opts}.

parse_contract(#{contract := #{name := ScopeName, functions := F0, type_defs := T0} = C}) ->
    MkTDef = fun(N, T) -> #{name => N, vars => [], typedef => T} end,
    T1 = [ MkTDef(<<"state">>, maps:get(state, C)) || maps:is_key(state, C) ] ++
         [ MkTDef(<<"event">>, maps:get(event, C)) || maps:is_key(event, C) ] ++ T0,
    ScopeTypes = [{TypeName, TypeDef} || #{name := TypeName, typedef := TypeDef} <- T1],
    F1 = [{Name, {[ArgType || #{type := ArgType} <- Args, is_binary(ArgType)], Type}} || #{arguments := Args, name := Name, returns := Type} <- F0, is_binary(Type)],
    F2 = maps:from_list(F1),
    %% Insert default init constructor
    F3 = case maps:is_key(<<"init">>, F2) of
           true ->
               F2;
           false ->
               maps:put(<<"init">>, {[], <<"unit">>}, F2)
         end,
    {ScopeName, #scope{types = maps:from_list(ScopeTypes), functions = F3}};
parse_contract(#{namespace := #{name := ScopeName, type_defs := Ts}}) when Ts /= [] ->
    ScopeTypes = [{TypeName, TypeDef} || #{name := TypeName, typedef := TypeDef} <- Ts],
    {ScopeName, #scope{types = maps:from_list(ScopeTypes), functions = maps:new()}};
parse_contract(#{namespace := _}) ->
    skip.

%% To simplify expression parsing lex the string
%% This is not the FULL Sophia lexer - It's a simpler variant designed
%% for call data encoding/decoding
%% This library should avoid linking with aesophia... Don't reuse the existing erlang
%% Sophia lexer or parser which are unbelievably slow.
-spec lex_string(string()) -> {error, term()} | [
    paren_start
    | paren_end
    | list_start
    | list_end
    | record_start
    | record_end
    | minus_sign
    | {con, string()}
    | {id, string()}
    | {string, string()}
    | {char, char()}
    | {int, integer()}
    | {hex, integer()}
    | {bytes, binary()}
    | dot
    | comma
    | equal
].
%% TODO: Nice error messsages :P
lex_string(String) ->
  skip_whitespace(String, [], []).

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

%% Skips whitespaces
skip_whitespace([], Stack, Acc) ->
    lex_string_(dispatch, [], Stack, Acc);
skip_whitespace([Char | _], _Stack, _Acc) when ?IS_FORBIDDEN_CHAR(Char) ->
    {error, forbidden_char};
skip_whitespace([Char | String], Stack, Acc) when ?IS_WS(Char) ->
    skip_whitespace(String, Stack, Acc);
skip_whitespace(String, Stack, Acc) ->
    lex_string_(dispatch, String, Stack, Acc).

%% Just hardcode a state machine :)
%% States: dispatch, comment, string, char, int, hex, bytes, id, con

%% Forbidden chars in a one line call expression - forbid them - there is no place for them
%% when encoding call data
lex_string_(_, [Char | _], _Stack, _Acc) when ?IS_FORBIDDEN_CHAR(Char) ->
    {error, forbidden_char};

%% Dispatch lexing operation
lex_string_(dispatch, [], [], Acc) -> %% Lexing done
    lists:reverse(Acc);
lex_string_(dispatch, [], _Stack, _Acc) -> %% Unmatched parantheses/braces/etc...
    {error, unmatched_parantheses};
%% Parantheses
lex_string_(dispatch, [$( | String], Stack, Acc) ->
    skip_whitespace(String, [paren_start | Stack], [paren_start | Acc]);
lex_string_(dispatch, [$) | String], [paren_start | Stack], Acc) ->
    skip_whitespace(String, Stack, [paren_end | Acc]);
lex_string_(dispatch, [$) | _], _Stack, _Acc) ->
    {error, unmatched_parantheses};
%% Lists
lex_string_(dispatch, [$[ | String], Stack, Acc) ->
    skip_whitespace(String, [list_start | Stack], [list_start | Acc]);
lex_string_(dispatch, [$] | String], [list_start | Stack], Acc) ->
    skip_whitespace(String, Stack, [list_end | Acc]);
lex_string_(dispatch, [$] | _], _Stack, _Acc) ->
    {error, unmatched_list};
%% Records/Maps
lex_string_(dispatch, [${ | String], Stack, Acc) ->
    skip_whitespace(String, [record_start | Stack], [record_start | Acc]);
lex_string_(dispatch, [$} | String], [record_start | Stack], Acc) ->
    skip_whitespace(String, Stack, [record_end | Acc]);
lex_string_(dispatch, [$} | _], _Stack, _Acc) ->
    {error, unmatched_record};
%% Comment start
lex_string_(dispatch, [$/, $/ | _], _Stack, _Acc) ->
    {error, forbidden_comment};
lex_string_(dispatch, [$/, $* | String], Stack, Acc) ->
    lex_string_(comment, String, Stack, Acc);
%% Lambdas
lex_string_(dispatch, [$=, $> | _], _Stack, _Acc) ->
    {error, forbidden_lambda};
%% Expr
lex_string_(dispatch, [$- | String], Stack, Acc) ->
    skip_whitespace(String, Stack, [minus_sign | Acc]);
lex_string_(dispatch, [$. | String], Stack, [{con, _} | _] = Acc) ->
    skip_whitespace(String, Stack, [dot | Acc]);
lex_string_(dispatch, [$. | _], _Stack, _Acc) ->
    {error, forbidden_expr};
lex_string_(dispatch, [$, | _], _Stack, [Top | _]) when Top =:= dot; Top =:= comma ->
    {error, forbidden_expr};
lex_string_(dispatch, [$, | String], Stack, Acc) ->
    skip_whitespace(String, Stack, [comma | Acc]);
lex_string_(dispatch, [$= | String], Stack, [{id, _} | _] = Acc) ->
    skip_whitespace(String, Stack, [equal | Acc]);
lex_string_(dispatch, [$= | String], Stack, [list_end | _] = Acc) ->
    skip_whitespace(String, Stack, [equal | Acc]);
lex_string_(dispatch, [$= | _], _Stack, _Acc) ->
    {error, forbidden_expr};
lex_string_(dispatch, [Char | _], _Stack, _Acc) when ?IS_FORBIDDEN_OP(Char) ->
    {error, forbidden_expr};
%% Hex
lex_string_(dispatch, [$0, $x | [Char | _] = String], Stack, Acc) when ?IS_HEXDIGIT(Char) ->
    lex_string_(hex, String, Stack, [{hex, 0} |  Acc]);
%% Integer
lex_string_(dispatch, [Char | _] = String, Stack, Acc) when ?IS_DIGIT(Char) ->
    lex_string_(int, String, Stack, [{int, 0} | Acc]);
%% Bytes
lex_string_(dispatch, [$# | [Char | _] = String], Stack, Acc) when ?IS_HEXDIGIT(Char) ->
    lex_string_(bytes, String, Stack, [{int, 0, 0} | Acc]);
%% Con
lex_string_(dispatch, [Char | String], Stack, Acc) when ?IS_UPPER(Char) ->
    lex_string_(con, String, Stack, [{con, [Char]} | Acc]);
%% Id
lex_string_(dispatch, [Char | String], Stack, Acc) when ?IS_LOWER(Char) ->
    lex_string_(id, String, Stack, [{id, [Char]} | Acc]);
%% String
lex_string_(dispatch, [$" | String], Stack, Acc) ->
    lex_string_(string, String, Stack, [{string, []} | Acc]);
%% Char
lex_string_(dispatch, [$' | String], Stack, Acc) ->
    lex_string_(char, String, Stack, Acc);

%% Comment handling
lex_string_(comment, [$*, $/ | String], Stack, Acc) ->
    skip_whitespace(String, Stack, Acc);
lex_string_(comment, [_ | String], Stack, Acc) ->
    lex_string_(comment, String, Stack, Acc);

%% Integer handling
lex_string_(int, [Char | String], Stack, [{int, Val}|Acc]) when ?IS_DIGIT(Char) ->
    lex_string_(int, String, Stack, [{int, Val*10 + Char - $0} | Acc]);
lex_string_(int, [$_ | [Char | _] = String], Stack, Acc) when ?IS_DIGIT(Char) ->
    lex_string_(int, String, Stack, Acc);
lex_string_(int, [$_ | _], _Stack, _Acc) ->
    {error, wrong_number};
lex_string_(int, String, Stack, [{int, Val}, minus_sign | Acc]) ->
    skip_whitespace(String, Stack, [{int, -Val} | Acc]);
lex_string_(int, String, Stack, Acc) ->
    skip_whitespace(String, Stack, Acc);

%% Hex handling
lex_string_(hex, [Char | String], Stack, [{hex, Val}|Acc]) when ?IS_HEXDIGIT(Char) ->
    lex_string_(hex, String, Stack, [{hex, Val*16 + hex_char_to_val(Char)} | Acc]);
lex_string_(hex, [$_ | [Char | _] = String], Stack, Acc) when ?IS_HEXDIGIT(Char) ->
    lex_string_(hex, String, Stack, Acc);
lex_string_(hex, [$_ | _], _Stack, _Acc) ->
    {error, wrong_number};
lex_string_(hex, String, Stack, [{hex, Val}, minus_sign | Acc]) ->
    skip_whitespace(String, Stack, [{hex, -Val} | Acc]);
lex_string_(hex, String, Stack, Acc) ->
    skip_whitespace(String, Stack, Acc);

%% Bytes handling
lex_string_(bytes, [Char | String], Stack, [{int, Val, Len} | Acc]) when ?IS_HEXDIGIT(Char) ->
    lex_string_(bytes, String, Stack, [{int, Val*16 + hex_char_to_val(Char), Len + 1} | Acc]);
lex_string_(bytes, [$_ | [Char | _] = String], Stack, Acc) when ?IS_HEXDIGIT(Char) ->
    lex_string_(hex, String, Stack, Acc);
lex_string_(bytes, [$_ | _], _Stack, _Acc) ->
    {error, wrong_number};
lex_string_(bytes, String, Stack, [{int, Val, Len} | Acc]) ->
    Digits = (Len + 1) div 2,
    skip_whitespace(String, Stack, [{bytes, <<Val:Digits/unit:8>>} | Acc]);

%% Con
lex_string_(con, [Char | String], Stack, [{con, Con} | Acc]) when ?IS_CON(Char) ->
    lex_string_(con, String, Stack, [{con, [Char | Con]} | Acc]);
lex_string_(con, String, Stack, [{con, Con} | Acc]) ->
    skip_whitespace(String, Stack, [{con, lists:reverse(Con)} | Acc]);

%% Id
lex_string_(id, [Char | String], Stack, [{id, Id} | Acc]) when ?IS_ID(Char) ->
    lex_string_(id, String, Stack, [{id, [Char | Id]} | Acc]);
lex_string_(id, String, Stack, [{id, Id} | Acc]) ->
    skip_whitespace(String, Stack, [{id, lists:reverse(Id)} | Acc]);

%% String
lex_string_(string, [$" | String], Stack, [{string, S} | Acc]) ->
    skip_whitespace(String, Stack, [{string, lists:reverse(S)} | Acc]);
lex_string_(string, [$\\, $x, D1, D2 | String], Stack, [{string, S} | Acc]) ->
    C = list_to_integer([D1, D2], 16),
    lex_string_(string, String, Stack, [{string, [C | S]} | Acc]);
lex_string_(string, [$\\, Code | String], Stack, [{string, S} | Acc]) ->
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
        _   -> {error, bad_char}
    end,
    case Char of
        {error, _} = Err ->
            Err;
        _ ->
            lex_string_(string, String, Stack, [{string, [Char | S]} | Acc])
    end;
lex_string_(string, [Char | String], Stack, [{string, S} | Acc]) ->
    lex_string_(string, String, Stack, [{string, [Char | S]} | Acc]);

%% Char
lex_string_(char, [$\\, Code, $' | String], Stack, Acc) ->
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
        _   -> {error, bad_char}
    end,
    case Char of
        {error, _} = Err ->
            Err;
        _ ->
            skip_whitespace(String, Stack, [{char, Char} | Acc])
    end;
lex_string_(char, [Char, $' | String], Stack, Acc) ->
    skip_whitespace(String, Stack, [{char, Char} | Acc]).

hex_char_to_val(Char) when ?IS_DIGIT(Char) ->
    Char - $0;
hex_char_to_val(Char) ->
    string:to_lower(Char) - $a + 10.
