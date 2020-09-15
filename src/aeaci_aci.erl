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
-export([file/1, encode_call_data/2]).

-include("aeaci_ast.hrl").

-type aci_typedef() ::
    int
    | bool
    | {bytes, integer()}
    | bits %% It is impossible for a user to construct a literal of this type but leave it here just in case
    | string
    | char
    | address
    | contract
    | oracle_query_id
    | unbound_var
    | oracle_id
    | {list, aci_typedef()}
    | {map, aci_typedef(), aci_typedef()}
    | {tuple, [aci_typedef()]}
    | {variant, [{string(), [aci_typedef()]}]}
    | {record, [#{string() => aci_typedef()}]}.

-type json_type() :: #{ binary() => term() } | binary().

-record(scope, { functions :: #{ binary() => {[aci_typedef()], aci_typedef()} }
               , typedefs :: #{ binary() => {json_type(), [binary()]} }
               , is_contract :: boolean()
               }).
%% Data type for encoding/decoding call data for a contract using the provided ACI
-record(contract_aci, { scopes :: #{ binary() => #scope{} }, main_contract :: binary(), opts :: map() }).

-spec file(string()) -> #contract_aci{}.
file(Filename) ->
  file(Filename, #{backend => fate, compiler_version => v4_3_1}).

file(Filename, Opts) ->
    {ok, JText} = file:read_file(Filename),
    Contracts =
              case jsx:decode(JText, [{labels, binary}, return_maps]) of
                  JArray when is_list(JArray)  -> JArray;
                  JObject when is_map(JObject) -> [JObject];
                  _                            -> error(bad_aci_json)
              end,
    Scopes1 = [parse_contract(Contract) || Contract <- Contracts],
    Scopes2 = [S || S <- Scopes1, is_tuple(S)],
    {Default, _} = lists:last(Scopes2),
    Env1 = maps:from_list(Scopes2),
    Env2 = maps:map(
        fun(ScopeName, {Scope, FoldedFuns}) ->
            Scope#scope{functions = maps:map(
                fun(_, FBody) -> unfold_types_in_function(Env1, ScopeName, FBody)
                end, FoldedFuns)}
        end, Env1),
    #contract_aci{scopes = Env2, main_contract = Default, opts = Opts}.

parse_contract(#{<<"contract">> := #{<<"name">> := ScopeName, <<"functions">> := F0, <<"type_defs">> := T0} = C}) ->
    MkTDef = fun(N, T) -> #{<<"name">> => N, <<"vars">> => [], <<"typedef">> => T} end,
    T1 = [ MkTDef(<<"state">>, maps:get(<<"state">>, C)) || maps:is_key(<<"state">>, C) ] ++
         [ MkTDef(<<"event">>, maps:get(<<"event">>, C)) || maps:is_key(<<"event">>, C) ] ++ T0,
    ScopeTypes = [
        {TypeName, {TypeDef, [V || #{<<"name">> := V} <- TypeVars]}}
        || #{<<"name">> := TypeName, <<"typedef">> := TypeDef, <<"vars">> := TypeVars} <- T1],
    F1 = [{Name, {[ArgType || #{<<"type">> := ArgType} <- Args], Type}} || #{<<"arguments">> := Args, <<"name">> := Name, <<"returns">> := Type} <- F0],
    F2 = maps:from_list(F1),
    %% Insert default init constructor
    F3 = case maps:is_key(<<"init">>, F2) of
           true ->
               F2;
           false ->
               maps:put(<<"init">>, {[], <<"unit">>}, F2)
         end,
    {ScopeName, {#scope{typedefs = maps:from_list(ScopeTypes), functions = #{}, is_contract = true}, F3}};
parse_contract(#{<<"namespace">> := #{<<"name">> := ScopeName, <<"type_defs">> := Ts}}) when Ts /= [] ->
    ScopeTypes = [{TypeName, {TypeDef, [V || #{<<"name">> := V} <- TypeVars]}} || #{<<"name">> := TypeName, <<"typedef">> := TypeDef, <<"vars">> := TypeVars} <- Ts],
    {ScopeName, {#scope{typedefs = maps:from_list(ScopeTypes), functions = #{}, is_contract = false}, #{}}};
parse_contract(#{<<"namespace">> := _}) ->
    skip.

unfold_types_in_function(Env, ScopeName, {FArgs, FRet}) ->
    {[unfold_type(Env, ScopeName, T) || T <- FArgs], unfold_type(Env, ScopeName, FRet)}.

-spec unfold_type(#{binary() => {#scope{}, #{ binary() => {[json_type()], json_type()}}}}, binary(), json_type()) -> aci_typedef().
unfold_type(Env, ScopeName, TypeName) when is_binary(TypeName) ->
    io:format("ZZZZ: Scope: ~p ~p\n", [ScopeName, TypeName]),
    [First | _] = binary_to_list(TypeName),
    case binary:split(TypeName, <<".">>) of
        [<<"unit">>] -> {tuple, []};
        [<<"int">>] -> int;
        [<<"signature">>] -> {bytes, 64};
        [<<"hash">>] -> {bytes, 32};
        [<<"address">>] -> address;
        [<<"bits">>] -> bits;
        [<<"string">>] -> string;
        [<<"char">>] -> char;
        [<<"bool">>] -> bool;
        [_] when First =:= $' ->
            unbound_var;
        [_] when First >= $a andalso First =< $z ->
            {#scope{typedefs = Typedefs}, _} = maps:get(ScopeName, Env),
            %% If we have a non empty var list then the ACI is broken
            ResolvedType = maps:get(TypeName, Typedefs),
            unfold_type(Env, ScopeName, do_type_substitution(ResolvedType, []));
        [_] ->
            %% Remote contract
            {#scope{is_contract = true}, _} = maps:get(TypeName, Env),
            contract;
        [<<"Chain">>, <<"ttl">>] ->
            todo;
        [Namespace, Typename] -> unfold_type(Env, Namespace, Typename)
    end;
unfold_type(Env, ScopeName, TypeDef) when is_map(TypeDef), 1 =:= map_size(TypeDef) ->
    [{TypeName, JSONTypeArgs}] = maps:to_list(TypeDef),
    io:format("AAAA: Scope: ~p ~p ~p\n", [ScopeName, TypeName, JSONTypeArgs]),
    case TypeName of
        <<"map">> ->
            [KT, VT] = JSONTypeArgs,
            {map, unfold_type(Env, ScopeName, KT), unfold_type(Env, ScopeName, VT)};
        <<"variant">> ->
            todo;
        <<"record">> ->
            {record, maps:from_list([{KeyName, unfold_type(Env, ScopeName, KeyType)} || #{<<"name">> := KeyName, <<"type">> := KeyType} <- JSONTypeArgs])};
        <<"bytes">> ->
            {bytes, JSONTypeArgs};
        <<"list">> ->
            [T] = JSONTypeArgs,
            {list, unfold_type(Env, ScopeName, T)};
        <<"oracle_query">> ->
            oracle_query_id;
        <<"oracle">> ->
            oracle_id;
        <<"option">> ->
            [T1] = JSONTypeArgs,
            T2 = unfold_type(Env, ScopeName, T1),
            {variant, [{"None", []}, {"Some", [T2]}]};
        <<"tuple">> ->
            {tuple, [unfold_type(Env, ScopeName, T) || T <- JSONTypeArgs]};
        _ when is_binary(TypeName) ->
            io:format("BBB: ~p ~p\n", [TypeName, JSONTypeArgs]),
            %% Ok we encountered a general type substitution
            [Namespace, TName] = binary:split(TypeName, <<".">>),
            {#scope{typedefs = Typedefs}, _} = maps:get(Namespace, Env),
            ResolvedType = maps:get(TName, Typedefs),
            %% Ok we resolved the type, now do the substitution
            SubstitutedType = do_type_substitution(ResolvedType, JSONTypeArgs),
            unfold_type(Env, ScopeName, SubstitutedType)
    end.

do_type_substitution({Type, []}, []) when is_binary(Type) -> Type;
do_type_substitution({Type, TNames}, TArgs) when length(TNames) =:= length(TArgs) ->
    do_type_substitution_(Type, lists:zip(TNames, TArgs)).

do_type_substitution_(Type, []) -> Type;
do_type_substitution_(Type, [{TName, TVal} | Rules]) ->
    do_type_substitution_(apply_single_substitution_rule(Type, TName, TVal), Rules).

apply_single_substitution_rule(T, T, TVal) -> TVal;
apply_single_substitution_rule(#{<<"variant">> := Options1}, TName, TVal) ->
    Options2 = lists:map(
        fun(V) ->
            [{CName, CArgs}] = maps:to_list(V),
            #{CName => [change_if_equal(Arg, TName, TVal) || Arg <- CArgs]}
        end, Options1),
    #{<<"variant">> => Options2}.

change_if_equal(T, T, TVal) -> TVal;
change_if_equal(T, _, _) -> T.

encode_call_data(#contract_aci{} = Aci, Call) when is_binary(Call) ->
    encode_call_data(Aci, binary_to_list(Call));
encode_call_data(#contract_aci{scopes = Scopes, main_contract = ContractName} = Aci, Call) when is_list(Call) ->
    Tokens = aeaci_lexer:string(Call),
    #ast_call{what = #ast_id{namespace = [], id = What}, args = #ast_tuple{args = UserArgs}} = aeaci_parser:parse_call(Tokens),
    #scope{functions = Functions} = maps:get(ContractName, Scopes),
    case maps:find(list_to_binary(What), Functions) of
        {ok, {FunctionArgs, _}} when length(FunctionArgs) =:= length(UserArgs) ->
            %% Great! We found what we want to call
            %% Now we need to unfold typedefs and
            ok;
        _ ->
            {error, list_to_binary(io_lib:format("Undefined function ~s/~p in contract ~s", [What, length(UserArgs), ContractName]))}
    end.
