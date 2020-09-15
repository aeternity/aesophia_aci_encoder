%%%-------------------------------------------------------------------
%%% @author gorbak25
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2020 13:55
%%%-------------------------------------------------------------------

%% This is not the full Sophia CFG - this is only a subset designed for call data encoding
-record(ast_id, {namespace :: [string()], id :: string()}).
-record(ast_con, {namespace :: [string()], con :: string()}).
-record(ast_tuple, {args :: [ast_literal()]}).
-record(ast_list, {args :: [ast_literal()]}).
-record(ast_map, {data :: #{ast_literal() => ast_literal()}}).
-record(ast_named_arg, {name :: #ast_id{}, value :: ast_literal()}).
-record(ast_record, {data :: [#ast_named_arg{}]}).
-record(ast_number, {val :: integer()}).
-record(ast_bytes, {val :: binary()}).
-record(ast_char, {val :: string()}).
-record(ast_string, {val :: string()}).
-record(ast_account, {pubkey :: binary()}).
-record(ast_contract, {pubkey :: binary()}).
-record(ast_oracle, {pubkey :: binary()}).
-record(ast_bool, {val :: boolean()}).
-record(ast_oracle_query, {id :: binary()}).
-record(ast_adt, {con :: #ast_con{}, args :: #ast_tuple{}}).
-record(ast_call, {what :: #ast_id{}, args :: #ast_tuple{}}).

-type ast_literal() ::
    #ast_tuple{}
    | #ast_list{}
    | #ast_number{}
    | #ast_bytes{}
    | #ast_map{}
    | #ast_adt{}
    | #ast_account{}
    | #ast_contract{}
    | #ast_oracle{}
    | #ast_oracle_query{}
    | #ast_bool{}
    | #ast_named_arg{}.
