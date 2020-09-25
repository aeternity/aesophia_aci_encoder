%%%-------------------------------------------------------------------
%%% @author gorbak25
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2020 13:57
%%%-------------------------------------------------------------------
-module(aeaci_parser).

%% API
-export([parse_call/1, parse_id_or_con/1]).

-include("aeaci_ast.hrl").

-type token_list() :: [{aeaci_lexer:lex_token(), aeaci_lexer:pos()}].
-spec parse_call(token_list()) -> #ast_call{} | {error, term()}.
parse_call(Tokens1) ->
    {#ast_id{namespace = []} = Name, [{paren_start, _} | Tokens2]} = parse_id_or_con(Tokens1),
    case parse_tuple(Tokens2) of
        {#ast_tuple{} = Args, []} -> %% Arity 0 and greater than 1
            #ast_call{what = Name, args = Args};
        {Literal, []} -> %% Arity 1
            #ast_call{what = Name, args = #ast_tuple{args = [Literal]}};
        _ ->
            {error, "Leftover tokens in entrypoint call"}
    end.

-spec parse_id_or_con(token_list()) -> {#ast_id{} | #ast_con{}, token_list()}.
parse_id_or_con(Tokens1) ->
    {[_|_] = Names, Tokens2} =
    lists:splitwith(fun({{con, _}, _}) -> true; ({{id, _}, _}) -> true; ({dot, _}) -> true; (_) -> false end, Tokens1),
    {Qualifiers, Last} = {
        lists:filtermap(fun({{con, S}, _}) -> {true, S}; ({dot, _}) -> false end, lists:droplast(Names)),
        lists:last(Names)},
    case Last of
        {{con, Name}, _} ->
            {#ast_con{namespace = Qualifiers, con = Name}, Tokens2};
        {{id, Name}, _} ->
            {#ast_id{namespace = Qualifiers, id = Name}, Tokens2}
    end.

-spec parse_literal(token_list()) -> {ast_literal(), token_list()}.
parse_literal([{{int, Number}, _} | Tokens]) ->
    {#ast_number{val = Number}, Tokens};
parse_literal([{{hex, Number}, _} | Tokens]) ->
    {#ast_number{val = Number}, Tokens};
parse_literal([{{string, String}, _} | Tokens]) ->
    {#ast_string{val = String}, Tokens};
parse_literal([{{char, Char}, _} | Tokens]) ->
    {#ast_char{val = Char}, Tokens};
parse_literal([{{bytes, Bytes}, _} | Tokens]) ->
    {#ast_bytes{val = Bytes}, Tokens};
parse_literal([{record_start, _}, {record_end, _} | Tokens]) ->
    {#ast_map{data = #{}}, Tokens};
parse_literal([{paren_start, _} | Tokens]) ->
    parse_tuple(Tokens);
parse_literal([{list_start, _} | Tokens]) ->
    parse_list(Tokens);
parse_literal([{record_start, _}, {list_start, _} | Tokens]) ->
    parse_map(Tokens);
parse_literal([{record_start, _} | Tokens]) ->
    parse_record(Tokens);
parse_literal([{{Type, _}, _} | _] = Tokens1) when Type =:= con; Type =:= id ->
    {Ast1, Tokens2} = parse_id_or_con(Tokens1),
    case {Ast1, Tokens2} of
        {#ast_id{namespace = [], id = [$a, $k, $_ | _] = SerializedPubkey}, _} ->
            {ok, Pubkey} = aeser_api_encoder:safe_decode(account_pubkey, list_to_binary(SerializedPubkey)),
            {#ast_account{pubkey = Pubkey}, Tokens2};
        {#ast_id{namespace = [], id = [$c, $t, $_ | _] = SerializedPubkey}, _} ->
            {ok, Pubkey} = aeser_api_encoder:safe_decode(contract_pubkey, list_to_binary(SerializedPubkey)),
            {#ast_contract{pubkey = Pubkey}, Tokens2};
        {#ast_id{namespace = [], id = [$o, $k, $_ | _] = SerializedPubkey}, _} ->
            {ok, Pubkey} = aeser_api_encoder:safe_decode(oracle_pubkey, list_to_binary(SerializedPubkey)),
            {#ast_oracle{pubkey = Pubkey}, Tokens2};
        {#ast_id{namespace = [], id = [$o, $q, $_ | _] = SerializedId}, _} ->
            {ok, Id} = aeser_api_encoder:safe_decode(oracle_query_id, list_to_binary(SerializedId)),
            {#ast_oracle_query{id = Id}, Tokens2};
        {#ast_id{namespace = [], id = "true"}, _} ->
            {#ast_bool{val = true}, Tokens2};
        {#ast_id{namespace = [], id = "false"}, _} ->
            {#ast_bool{val = false}, Tokens2};
        {#ast_id{} = Id, [{equal, _} | Tokens3]} ->
            %% For future proofing as user defined custom named args might get introduced to Sophia
            {LiteralAst, Tokens4} = parse_literal(Tokens3),
            {#ast_named_arg{name = Id, value = LiteralAst}, Tokens4};
        {#ast_con{} = Constructor, [{paren_start, _} | Tokens3]} ->
            %% Algebraic data type constructor
            case parse_tuple(Tokens3) of
                {#ast_tuple{} = Args, Tokens4} -> %% Arity 0 and greater than 1
                    {#ast_adt{con = Constructor, args = Args}, Tokens4};
                {Literal, Tokens4} -> %% Arity 1
                    {#ast_adt{con = Constructor, args = #ast_tuple{args = [Literal]}}, Tokens4}
            end;
        {#ast_con{} = Constructor, _} ->
            %% By default standalone Constructors have arity 0
            {#ast_adt{con = Constructor, args = #ast_tuple{args = []}}, Tokens2}
    end.

-spec parse_tuple(token_list()) -> {ast_literal(), token_list()}.
%% One tuples automatically reduce to literals
parse_tuple([{paren_end, _} | Tokens]) ->
    {#ast_tuple{args = []}, Tokens};
parse_tuple([{comma, _} | Tokens]) ->
    parse_tuple(Tokens);
parse_tuple(Tokens1) ->
    {LiteralAst, Tokens2} = parse_literal(Tokens1),
    {Tuple, Tokens3} = parse_tuple(Tokens2),
    case {Tuple, LiteralAst} of
        {#ast_tuple{args = []}, _} ->
            {LiteralAst, Tokens3};
        {#ast_tuple{args = Args}, _} ->
            {#ast_tuple{args = [LiteralAst | Args]}, Tokens3};
        {_, _} ->
            {#ast_tuple{args = [LiteralAst, Tuple]}, Tokens3}
    end.

-spec parse_list(token_list()) -> {#ast_list{}, token_list()}.
parse_list([{list_end, _} | Tokens]) ->
    {#ast_list{args = []}, Tokens};
parse_list([{comma, _} | Tokens]) ->
    parse_list(Tokens);
parse_list(Tokens1) ->
    {LiteralAst, Tokens2} = parse_literal(Tokens1),
    {#ast_list{args = List}, Tokens3} = parse_list(Tokens2),
    {#ast_list{args = [LiteralAst | List]}, Tokens3}.

-spec parse_map(token_list()) -> {#ast_map{}, token_list()}.
parse_map([{record_end, _} | Tokens]) ->
    {#ast_map{data = #{}}, Tokens};
parse_map([{comma, _}, {list_start, _} | Tokens]) ->
    parse_map(Tokens);
parse_map(Tokens1) ->
    {KeyAst, [{list_end, _}, {equal, _} | Tokens2]} = parse_literal(Tokens1),
    {ValueAst, Tokens3} = parse_literal(Tokens2),
    {#ast_map{data = Map}, Tokens4} = parse_map(Tokens3),
    {#ast_map{data = maps:put(KeyAst, ValueAst, Map)}, Tokens4}.

-spec parse_record(token_list()) -> {#ast_record{}, token_list()}.
parse_record([{record_end, _} | Tokens]) ->
    {#ast_record{data = []}, Tokens};
parse_record([{comma, _} | Tokens]) ->
    parse_record(Tokens);
parse_record(Tokens1) ->
    {NamedArgAst, Tokens2} = parse_named_arg(Tokens1),
    {#ast_record{data = Data}, Tokens3} = parse_record(Tokens2),
    {#ast_record{data = [NamedArgAst | Data]}, Tokens3}.

-spec parse_named_arg(token_list()) -> {#ast_named_arg{}, token_list()}.
parse_named_arg(Tokens1) ->
    {#ast_id{namespace = []} = Name, [{equal, _} | Tokens2]} = parse_id_or_con(Tokens1),
    {LiteralAst, Tokens3} = parse_literal(Tokens2),
    {#ast_named_arg{name = Name, value = LiteralAst}, Tokens3}.
