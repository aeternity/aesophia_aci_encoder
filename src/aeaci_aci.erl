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
-export([file/1, from_string/2, encode_call_data/2]).

-ifdef(TEST).
-export([generate_tests/3]).
-endif.

-include("aeaci_ast.hrl").

-type aci_typedef() ::
    int
    | bool
    | {bytes, integer()}
    | bits %% It is impossible for a user to construct a literal of this type but leave it here just in case
    | void
    | string
    | char
    | address
    | contract
    | oracle_query_id
    | {unbound_var, binary()}
    | function
    | oracle_id
    | {list, aci_typedef()}
    | {map, aci_typedef(), aci_typedef()}
    | {tuple, [aci_typedef()]}
    | {variant, [{string(), [aci_typedef()]}]}
    | {record, [{string(), aci_typedef()}]}.

-type json_type() :: #{ binary() => term() } | binary().

-record(scope, { functions :: #{ binary() => {[aci_typedef()], aci_typedef()} }
               , typedefs :: #{ binary() => {json_type(), [binary()]} }
               , is_contract :: boolean()
               }).
%% Data type for encoding/decoding call data for a contract using the provided ACI
-record(contract_aci, { scopes :: #{ binary() => #scope{} }, main_contract :: binary(), opts :: map() }).

-spec file(string()) -> #contract_aci{}.
file(Filename) ->
  file(Filename, #{backend => fate}).

file(Filename, Opts) ->
    {ok, JText} = file:read_file(Filename),
    from_string(JText, Opts).

from_string(JText, Opts) ->
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
    Env3 = maybe_strip_typedefs(Env2, maps:get(strip, Opts, true)),
    #contract_aci{scopes = Env3, main_contract = Default, opts = Opts}.

parse_contract(#{<<"contract">> := #{<<"name">> := ScopeName, <<"functions">> := F0, <<"typedefs">> := T0} = C}) ->
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
parse_contract(#{<<"namespace">> := #{<<"name">> := ScopeName, <<"typedefs">> := Ts}}) when Ts /= [] ->
    ScopeTypes = [{TypeName, {TypeDef, [V || #{<<"name">> := V} <- TypeVars]}} || #{<<"name">> := TypeName, <<"typedef">> := TypeDef, <<"vars">> := TypeVars} <- Ts],
    {ScopeName, {#scope{typedefs = maps:from_list(ScopeTypes), functions = #{}, is_contract = false}, #{}}};
parse_contract(C = #{<<"contract">> := #{<<"type_defs">> := T0} = C0}) ->
    parse_contract(C#{<<"contract">> => C0#{<<"typedefs">> => T0}});
parse_contract(N = #{<<"namespace">> := #{<<"type_defs">> := T0} = N0}) ->
    parse_contract(N#{<<"namespace">> => N0#{<<"typedefs">> => T0}});
parse_contract(#{<<"namespace">> := _}) ->
    skip.

maybe_strip_typedefs(Env, false) -> Env;
maybe_strip_typedefs(Env, true) ->
    maps:map(fun(_, Scope) -> Scope#scope{typedefs = #{}} end, Env).

unfold_types_in_function(Env, ScopeName, {FArgs, FRet}) ->
    {[unfold_type(Env, ScopeName, T) || T <- FArgs], unfold_type(Env, ScopeName, FRet)}.

-spec unfold_type(#{binary() => {#scope{}, #{ binary() => {[json_type()], json_type()}}}}, binary(), json_type()) -> aci_typedef().
unfold_type(Env, ScopeName, TypeName) when is_binary(TypeName) ->
    [First | _] = binary_to_list(TypeName),
    case binary:split(TypeName, <<".">>) of
        [<<"unit">>] -> {tuple, []};
        [<<"int">>] -> int;
        [<<"signature">>] -> {bytes, 64};
        [<<"hash">>] -> {bytes, 32};
        [<<"void">>] -> void;
        [<<"address">>] -> address;
        [<<"bits">>] -> bits;
        [<<"string">>] -> string;
        [<<"char">>] -> char;
        [<<"bool">>] -> bool;
        [TVar] when First =:= $' ->
            %% Fate Vm supports polimorphic entrypoints
            {unbound_var, TVar};
        [_] when First >= $a andalso First =< $z ->
            {#scope{typedefs = Typedefs}, _} = maps:get(ScopeName, Env),
            %% If we have a non empty var list then the ACI is broken
            ResolvedType = maps:get(TypeName, Typedefs),
            unfold_type(Env, ScopeName, do_type_substitution(ResolvedType, []));
        [_] ->
            %% Remote contract
            %% Old ACI's didn't include remote contracts :P
            %% TODO: optionally enable this check if the user wants
            %%{#scope{is_contract = true}, _} = maps:get(TypeName, Env),
            contract;
        %% Chain TTL was in sophia since the beginning
        [<<"Chain">>, <<"ttl">>] ->
            ttl_t();
        %% Types for GA account sophia contracts
        [<<"Chain">>, <<"paying_for_tx">>] ->
            paying_for_tx_t();
        [<<"Chain">>, <<"ga_meta_tx">>] ->
            ga_meta_tx_t();
        [<<"Chain">>, <<"base_tx">>] ->
            base_tx_t();
        [<<"Chain">>, <<"tx">>] ->
            {record,
                [{"paying_for", option_t(paying_for_tx_t())}, {"ga_metas", {list, ga_meta_tx_t()}},
                 {"actor", address}, {"fee", int}, {"ttl", int}, {"tx", base_tx_t()}]};
        % AENS types
        [<<"AENS">>, <<"pointee">>] ->
            pointee_t();
        [<<"AENS">>, <<"name">>] ->
            {variant, [{"Name", [address, ttl_t(), {map, string, pointee_t()}]}]};
        %% Fancy crypto primitives
        [<<"MCL_BLS12_381">>, <<"fr">>] -> {bytes, 32};
        [<<"MCL_BLS12_381">>, <<"fp">>] -> {bytes, 48};
        %% User defined type
        [Namespace, Typename] -> unfold_type(Env, Namespace, Typename)
    end;
unfold_type(Env, ScopeName, TypeDef) when is_map(TypeDef), 1 =:= map_size(TypeDef) ->
    [{TypeName, JSONTypeArgs}] = maps:to_list(TypeDef),
    case TypeName of
        <<"map">> ->
            [KT, VT] = JSONTypeArgs,
            {map, unfold_type(Env, ScopeName, KT), unfold_type(Env, ScopeName, VT)};
        <<"variant">> ->
            {variant,
                [{binary_to_list(K), V} || [{K,V}] <- [maps:to_list(maps:map(fun(_, TL) ->
                        [unfold_type(Env, ScopeName, T) || T <- TL]
                             end, Arg)) || Arg <- JSONTypeArgs]]
            };
        <<"record">> ->
            {record, [{KeyName, unfold_type(Env, ScopeName, KeyType)} || #{<<"name">> := KeyName, <<"type">> := KeyType} <- JSONTypeArgs]};
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
            option_t(unfold_type(Env, ScopeName, T1));
        <<"tuple">> ->
            {tuple, [unfold_type(Env, ScopeName, T) || T <- JSONTypeArgs]};
        <<"function">> ->
            %% Old compilers sometimes emitted bullshit in the ACI...
            %% This entrypoint will be uncallable
            %% but let's just leave it here as is :P
            function;
        _ when is_binary(TypeName) ->
            %% Ok we encountered a general type substitution
            [Namespace, TName] = binary:split(TypeName, <<".">>),
            {#scope{typedefs = Typedefs}, _} = maps:get(Namespace, Env),
            ResolvedType = maps:get(TName, Typedefs),
            %% Ok we resolved the type, now do the substitution
            SubstitutedType = do_type_substitution(ResolvedType, JSONTypeArgs),
            unfold_type(Env, ScopeName, SubstitutedType)
    end;
%% Old compilers
unfold_type(Env, ScopeName, [TypeDef]) ->
    unfold_type(Env, ScopeName, TypeDef).

ttl_t() ->
    {variant, [{"RelativeTTL", [int]}, {"FixedTTL", [int]}]}.
option_t(T) ->
    {variant, [{"None", []}, {"Some", [T]}]}.
paying_for_tx_t() ->
    {variant, [{"PayingForTx", [address, int]}]}.
ga_meta_tx_t() ->
    {variant, [{"GAMetaTx", [address, int]}]}.
base_tx_t() ->
    {variant,
    [ {"SpendTx",      [address, int, string]}
    , {"OracleRegisterTx",       []}
    , {"OracleQueryTx",          []}
    , {"OracleResponseTx",       []}
    , {"OracleExtendTx",         []}
    , {"NamePreclaimTx",         []}
    , {"NameClaimTx",            [string]}
    , {"NameUpdateTx",           [{bytes, 32}]}
    , {"NameRevokeTx",           [{bytes, 32}]}
    , {"NameTransferTx",         [address, {bytes, 32}]}
    , {"ChannelCreateTx",        [address]}
    , {"ChannelDepositTx",       [address, int]}
    , {"ChannelWithdrawTx",      [address, int]}
    , {"ChannelForceProgressTx", [address]}
    , {"ChannelCloseMutualTx",   [address]}
    , {"ChannelCloseSoloTx",     [address]}
    , {"ChannelSlashTx",         [address]}
    , {"ChannelSettleTx",        [address]}
    , {"ChannelSnapshotSoloTx",  [address]}
    , {"ContractCreateTx",       [int]}
    , {"ContractCallTx",         [address, int]}
    , {"GAAttachTx",             []}
    ]}.
pointee_t() ->
    {variant,
    [ {"AccountPt", [address]}
    , {"OraclePt", [address]}
    , {"ContractPt", [address]}
    , {"ChannelPt", [address]}
    ]}.

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
    #{<<"variant">> => Options2};
apply_single_substitution_rule(#{<<"record">> := NamedArgs}, TName, TVal) ->
    #{<<"record">> => [#{<<"name">> => N, <<"type">> => change_if_equal(T, TName, TVal)} || #{<<"name">> := N, <<"type">> := T} <- NamedArgs]};
apply_single_substitution_rule(#{<<"function">> := _} = T, _, _) ->
    %% Old compilers...
    T;
apply_single_substitution_rule(Map, TName, TVal) when is_map(Map) ->
    maps:map(fun(_, Types) -> [change_if_equal(T, TName, TVal) || T <- Types] end, Map).

change_if_equal(T, T, TVal) -> TVal;
change_if_equal(T, _, _) -> T.

-spec encode_call_data(#contract_aci{}, binary() | string()) -> {ok, binary()} | {error, term()}.
encode_call_data(#contract_aci{} = Aci, Call) when is_binary(Call) ->
    encode_call_data(Aci, binary_to_list(Call));
encode_call_data(#contract_aci{} = Aci, Call) when is_list(Call) ->
    case aeaci_lexer:string(Call) of
        {ok, Tokens} ->
            encode_call_data_tokens(Aci, Tokens);
        {error, _} = Err ->
            Err
    end.

encode_call_data_tokens(#contract_aci{scopes = Scopes, main_contract = ContractName, opts = Opts}, Tokens) ->
    #ast_call{what = #ast_id{namespace = [], id = What}, args = #ast_tuple{args = UserArgs}} = aeaci_parser:parse_call(Tokens),
    #scope{functions = Functions} = maps:get(ContractName, Scopes),
    case maps:find(list_to_binary(What), Functions) of
        {ok, {ArgTypes, RetType}} when length(ArgTypes) =:= length(UserArgs) ->
            %% Great! We found what we want to call
            case maps:get(backend, Opts, fate) of
                fate ->
                    put(var_constrains, #{}),
                    put(fresh_tvar, {unbound_var, 0}), %% User tvars use binaries :P
                    try
                        aeb_fate_abi:create_calldata(What, [type_encode_fate(T1, T2) || {T1, T2} <- lists:zip(ArgTypes, UserArgs)])
                    after
                        put(var_constrains, #{})
                    end;
                aevm ->
                    EncRetType = case {What, to_aevm_type(RetType)} of
                             {"init", E} ->
                                 {tuple, [typerep, E]};
                             {_, E} ->
                                 E
                    end,
                    aeb_aevm_abi:create_calldata(
                        What,
                        [type_encode_aevm(T1, T2) || {T1, T2} <- lists:zip(ArgTypes, UserArgs)],
                        [to_aevm_type(T) || T <- ArgTypes],
                        EncRetType)
            end;
        _ ->
            {error, list_to_binary(io_lib:format("Undefined function ~s/~p in contract ~s", [What, length(UserArgs), ContractName]))}
    end.

%% Please note that polymorphic entrypoints are not a priority and they are SLOW
%% Especially please avoid using polymorphic records or algebraic data types...
-spec type_encode_fate(aci_typedef(), ast_literal()) -> term().
type_encode_fate(int, #ast_number{val = Val}) -> aeb_fate_data:make_integer(Val);
type_encode_fate(bool, #ast_bool{val = Val}) -> aeb_fate_data:make_boolean(Val);
type_encode_fate(string, #ast_string{val = Val}) -> aeb_fate_data:make_string(Val);
type_encode_fate(char, #ast_char{val = Val}) -> aeb_fate_data:make_integer(Val);
type_encode_fate(address, #ast_account{pubkey = Pub}) -> aeb_fate_data:make_address(Pub);
type_encode_fate(contract, #ast_contract{pubkey = Pub}) -> aeb_fate_data:make_contract(Pub);
type_encode_fate(oracle_id, #ast_oracle{pubkey = Pub}) -> aeb_fate_data:make_oracle(Pub);
type_encode_fate(oracle_query_id, #ast_oracle_query{id = Id}) -> aeb_fate_data:make_oracle_query(Id);
type_encode_fate({list, LType}, #ast_list{args = LArgs}) ->
    aeb_fate_data:make_list([type_encode_fate(LType, Arg) || Arg <- LArgs]);
type_encode_fate({tuple, TList}, #ast_tuple{args = TArgs}) when length(TList) =:= length(TArgs) ->
    aeb_fate_data:make_tuple(list_to_tuple([type_encode_fate(T, Arg) || {T, Arg} <- lists:zip(TList, TArgs)]));
type_encode_fate({map, KT, VT}, #ast_map{data = Data}) ->
    aeb_fate_data:make_map(
        maps:from_list([
            {type_encode_fate(KT, K), type_encode_fate(VT, V)}
            || {K, V} <- maps:to_list(Data)]
        ));
type_encode_fate({bytes, Size}, #ast_bytes{val = Binary}) when byte_size(Binary) =:= Size ->
    aeb_fate_data:make_bytes(Binary);
type_encode_fate({record, Defs}, #ast_record{data = NamedArgs}) when length(Defs) =:= length(NamedArgs) ->
    ArgsMap = maps:from_list([{list_to_binary(Name), Val} || #ast_named_arg{name = #ast_id{namespace = [], id = Name}, value = Val} <- NamedArgs]),
    true = map_size(ArgsMap) =:= length(Defs),
    aeb_fate_data:make_tuple(list_to_tuple([type_encode_fate(Type, maps:get(Name, ArgsMap)) || {Name, Type} <- Defs]));
type_encode_fate({variant, Cons}, #ast_adt{con = #ast_con{namespace = _, con = ConName}, args = #ast_tuple{args = Args}}) ->
    {Ar, _, ConNum, ConArgsT} = lists:foldl(
        fun ({ConName1, ConArgTypes}, {Arities, Pos, _, _}) when ConName1 =:= ConName ->
                {[length(ConArgTypes)|Arities], Pos+1, Pos, ConArgTypes};
            ({_, ConArgTypes}, {Arities, Pos, FoundPos, FoundArgs}) ->
                {[length(ConArgTypes)|Arities], Pos+1, FoundPos, FoundArgs} end,
        {[], 0, error, error}, Cons),
    true = length(Args) =:= length(ConArgsT),
    aeb_fate_data:make_variant(lists:reverse(Ar), ConNum, list_to_tuple(
        [type_encode_fate(Type, Arg) || {Type, Arg} <- lists:zip(ConArgsT, Args)]));
type_encode_fate({unbound_var, TVar1}, Ast) ->
    case tvar_find(TVar1) of
        {ok, {unbound_var, TVar2}} ->
            InferredType = fate_type_inference(Ast),
            unify_types(InferredType, {unbound_var, TVar2}),
            type_encode_fate(InferredType, Ast);
        {ok, LiteralType} ->
            type_encode_fate(LiteralType, Ast);
        _ ->
            InferredType = fate_type_inference(Ast),
            unify_types(InferredType, {unbound_var, TVar1}),
            type_encode_fate(InferredType, Ast)
    end;
type_encode_fate(ACI_Type, Ast_Type) ->
    {error, io_lib:format("Could not encode ~p as ~p for FATE VM", [Ast_Type, ACI_Type])}.

fate_type_inference(#ast_number{}) -> int;
fate_type_inference(#ast_bool{}) -> bool;
fate_type_inference(#ast_string{}) -> string;
fate_type_inference(#ast_char{}) -> char;
fate_type_inference(#ast_account{}) -> address;
fate_type_inference(#ast_contract{}) -> contract;
fate_type_inference(#ast_oracle{}) -> oracle_id;
fate_type_inference(#ast_oracle_query{}) -> oracle_query_id;
fate_type_inference(#ast_bytes{val = Bytes}) -> {bytes, byte_size(Bytes)};
fate_type_inference(#ast_list{args = []}) ->
    {list, fresh_tvar()};
fate_type_inference(#ast_list{args = [H|T]}) ->
    {list, lists:foldl(fun(El, Type) ->
        unify_types(fate_type_inference(El), Type)
                end, fate_type_inference(H), T)};
fate_type_inference(#ast_tuple{args = Types}) ->
    {tuple, [fate_type_inference(T) || T <- Types]};
fate_type_inference(#ast_map{data = Map}) ->
    Keys = maps:keys(Map),
    Values = maps:values(Map),
    {list, KT} = fate_type_inference(#ast_list{args = Keys}),
    {list, VT} = fate_type_inference(#ast_list{args = Values}),
    {map, KT, VT};
fate_type_inference(#ast_record{}) ->
    io:format(user, "Polimorphism on records is currently unsupported", []),
    error(not_yet_implemented);
fate_type_inference(#ast_adt{con = #ast_con{namespace = Namespace, con = Name}, args = ArgList}) ->
    {tuple, Inferred} = fate_type_inference(ArgList),
    case {Namespace, Name, Inferred} of
        {[], "None", []} ->
            option_t(fresh_tvar());
        {[], "Some", [Type]} ->
            option_t(Type);
        {[], "RelativeTTL", [int]} ->
            ttl_t();
        {[], "FixedTTL", [int]} ->
            ttl_t();
        _ ->
            io:format(user, "Polimorphism on Algebraic Data Types is currently unsupported", []),
            error(not_yet_implemented)
    end.

unify_types(Type, Type) -> Type;
unify_types({unbound_var, T1}, {unbound_var, T2}) ->
    tvar_union(T1, T2),
    tvar_find(T1);
unify_types({unbound_var, T}, Type) ->
    case tvar_find(T) of
        {unbound_var, Rep} ->
            put(var_constrains, maps:put(Rep, Type, get(var_constrains))),
            Type;
        LiteralType ->
            unify_types(LiteralType, Type)
    end;
unify_types(Type, {unbound_var, _} = T) ->
    unify_types(T, Type).

tvar_find(TVar1) ->
    Constrains = get(var_constrains),
    case maps:find(TVar1, Constrains) of
        {ok, {unbound_var, TVar2}} ->
            Res = tvar_find(TVar2),
            put(var_constrains, maps:put(TVar1, Res, Constrains)),
            Res;
        {ok, FinalType} ->
            FinalType;
        _ ->
            {unbound_var, TVar1}
    end.
tvar_union(TVar1, TVar2) ->
    case {tvar_find(TVar1), tvar_find(TVar2)} of
        {T, T} -> ok;
        {{unbound_var, TVar}, Type} ->
            put(var_constrains, maps:put(TVar, Type, get(var_constrains)));
        {Type, {unbound_var, TVar}} ->
            put(var_constrains, maps:put(TVar, Type, get(var_constrains)))
    end.
fresh_tvar() ->
    {unbound_var, Ctr} = get(fresh_tvar),
    put(fresh_tvar, {unbound_var, Ctr+1}),
    {unbound_var, Ctr}.

to_aevm_type(int) -> word;
to_aevm_type(bool) -> word;
to_aevm_type({bytes, ByteSize}) when ByteSize =< 32 -> word;
to_aevm_type({bytes, ByteSize}) -> {tuple, [word || _ <- lists:seq(1, (ByteSize + 31) div 32)]};
to_aevm_type(string) -> string;
to_aevm_type(char) -> word;
to_aevm_type(address) -> word;
to_aevm_type(contract) -> word;
to_aevm_type(oracle_query_id) -> word;
to_aevm_type(oracle_id) -> word;
to_aevm_type({list, TDef}) -> {list, to_aevm_type(TDef)};
to_aevm_type({map, TK, TV}) -> {map, to_aevm_type(TK), to_aevm_type(TV)};
to_aevm_type({tuple, TDefs}) -> {tuple, [to_aevm_type(T) || T <- TDefs]};
to_aevm_type({variant, Cons}) -> {variant, [[to_aevm_type(T) || T <- TDefs] || {_, TDefs} <- Cons]};
to_aevm_type({record, TDefs}) -> {tuple,[to_aevm_type(T) || {_, T} <- TDefs]};
to_aevm_type(Type) ->
    {error, io_lib:format("Could not translate ~p as to AEVM type", [Type])}.

-spec type_encode_aevm(aci_typedef(), ast_literal()) -> term().
type_encode_aevm(int, #ast_number{val = Val}) -> Val;
type_encode_aevm(bool, #ast_bool{val = false}) -> 1;
type_encode_aevm(bool, #ast_bool{val = true}) -> 1;
type_encode_aevm(string, #ast_string{val = Val}) -> list_to_binary(Val);
type_encode_aevm(char, #ast_char{val = Val}) -> Val;
type_encode_aevm(address, #ast_account{pubkey = <<Pub:32/unit:8>>}) -> Pub;
type_encode_aevm(contract, #ast_contract{pubkey = <<Pub:32/unit:8>>}) -> Pub;
type_encode_aevm(oracle_id, #ast_oracle{pubkey = <<Pub:32/unit:8>>}) -> Pub;
type_encode_aevm(oracle_query_id, #ast_oracle_query{id = <<Id:32/unit:8>>}) -> Id;
type_encode_aevm({list, LType}, #ast_list{args = LArgs}) ->
    [type_encode_aevm(LType, Arg) || Arg <- LArgs];
type_encode_aevm({tuple, TList}, #ast_tuple{args = TArgs}) when length(TList) =:= length(TArgs) ->
    list_to_tuple([type_encode_aevm(T, Arg) || {T, Arg} <- lists:zip(TList, TArgs)]);
type_encode_aevm({map, KT, VT}, #ast_map{data = Data}) ->
        maps:from_list([
            {type_encode_aevm(KT, K), type_encode_aevm(VT, V)}
            || {K, V} <- maps:to_list(Data)]
        );
type_encode_aevm({bytes, Size}, #ast_bytes{val = Binary}) when byte_size(Binary) =:= Size ->
    case aeb_memory:binary_to_words(Binary) of
        [Word] -> Word;
        Words  -> list_to_tuple([W || W <- Words])
    end;
type_encode_aevm({record, Defs}, #ast_record{data = NamedArgs}) when length(Defs) =:= length(NamedArgs) ->
    ArgsMap = maps:from_list([{list_to_binary(Name), Val} || #ast_named_arg{name = #ast_id{namespace = [], id = Name}, value = Val} <- NamedArgs]),
    true = map_size(ArgsMap) =:= length(Defs),
    list_to_tuple([type_encode_aevm(Type, maps:get(Name, ArgsMap)) || {Name, Type} <- Defs]);
type_encode_aevm({variant, Cons}, #ast_adt{con = #ast_con{namespace = _, con = ConName}, args = #ast_tuple{args = Args}}) ->
    {_, _, ConNum, ConArgsT} = lists:foldl(
        fun ({ConName1, ConArgTypes}, {Arities, Pos, _, _}) when ConName1 =:= ConName ->
                {[length(ConArgTypes)|Arities], Pos+1, Pos, ConArgTypes};
            ({_, ConArgTypes}, {Arities, Pos, FoundPos, FoundArgs}) ->
                {[length(ConArgTypes)|Arities], Pos+1, FoundPos, FoundArgs} end,
        {[], 0, error, error}, Cons),
    true = length(Args) =:= length(ConArgsT),
    {variant, ConNum, [type_encode_aevm(Type, Arg) || {Type, Arg} <- lists:zip(ConArgsT, Args)]};
type_encode_aevm(ACI_Type, Ast_Type) ->
    {error, io_lib:format("Could not encode ~p as ~p for AEVM", [Ast_Type, ACI_Type])}.

-ifdef(TEST).
generate_tests(EncodeCallCachePath, DecodeCallCachePath, ResPath) ->
    {ok, ET} = ets:file2tab(EncodeCallCachePath),
    {ok, DT} = ets:file2tab(DecodeCallCachePath),
    D = [#{<<"source">> => Source, <<"fun_name">> => FunName, <<"version">> => VM, <<"outcome">> => Outcome, <<"call_res">> => CallRes, <<"decoded">> => Res}
        || {decode_call_cache_entry, {decode_call_id, Source, {FunName, VM}, Outcome, CallRes}, Res} <- ets:tab2list(DT)],
    E = [#{<<"source">> => Source, <<"version">> => Version, <<"vm">> => VM, <<"call">> => Call, <<"calldata">> => Value} || {encode_call_cache_entry, {encode_call_id, Version, Source, _, Call, VM}, {ok, Value}} <- ets:tab2list(ET)],
    {R, _} = lists:foldl(
        fun (#{<<"source">> := Source0
            , <<"version">> := Version
            , <<"vm">> := VM
            , <<"call">> := Call
            , <<"calldata">> := Value}, {R, A}) ->
            try
                Source = binary:replace(to_bin(Source0), <<"include \"String.aes\"\n">>, <<"">>),
                Aci = case maps:find(Source, A) of
                          {ok, Aci1} -> Aci1;
                          _ -> gen_aci(Source, VM)
                      end,

                CaseType = maps:get(encode, R, #{}),
                VmType = maps:get(VM, CaseType, #{}),
                SophiaVer = maps:get(Version, VmType, #{}),
                Examples = maps:get(Aci, SophiaVer, []),
                {
                    maps:put(encode,
                    maps:put(VM,
                    maps:put(Version,
                    maps:put(Aci,
                            [#{<<"call">> => to_bin(Call), <<"calldata">> => aeser_api_encoder:encode(contract_bytearray, Value)}|Examples]
                            , SophiaVer), VmType), CaseType), R),
                    maps:put(Source, Aci, A)
                }
            catch error:Err ->
                io:format("Warning: ~p\n Skipping contract ~p\n\n\n", [Err, Source0]),
                {R, A}
            end;
            (#{ <<"source">> := Source0
              , <<"fun_name">> := FunName
              , <<"version">> := VM
              , <<"outcome">> := Outcome
              , <<"call_res">> := CallRes
              , <<"decoded">> := Res}, {R, A}) ->
                Source = binary:replace(to_bin(Source0), <<"include \"String.aes\"\n">>, <<"">>),
                Aci = case maps:find(Source, A) of
                          {ok, Aci1} -> Aci1;
                          _ -> gen_aci(Source, VM)
                      end,
                CaseType = maps:get(decode, R, #{}),
                VmType = maps:get(VM, CaseType, #{}),
                Examples = maps:get(Aci, VmType, []),
                {
                    maps:put(decode,
                    maps:put(VM,
                    maps:put(Aci,
                            [#{ <<"fun_name">> => to_bin(FunName),
                                <<"outcome">> => to_bin(Outcome),
                                <<"call_res">> => to_bin(CallRes),
                                <<"decoded">> => Res}|Examples]
                            , VmType), CaseType), R),
                    maps:put(Source, Aci, A)
                }
        end, {#{}, #{}}, E++D),
    file:write_file(ResPath, io_lib:format("~s", [jsx:encode(R)])).

to_bin(ok) -> <<"ok">>;
to_bin(error) -> <<"error">>;
to_bin(revert) -> <<"revert">>;
to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(Str) when is_list(Str) -> list_to_binary(Str).

gen_aci(Source, VM) ->
    try
        {ok, Enc} = aeso_aci:contract_interface(json, Source, [{backend, VM}]),
        jsx:encode(Enc)
    catch error:_ ->
        %% Try to generate the aci with aesophia_cli :P
        file:write_file("/tmp/.aesophia", Source),
        Res = os:cmd(code:priv_dir(aesophia_cli) ++ "/bin/v4.3.1/aesophia_cli --create_json_aci /tmp/.aesophia -b " ++ atom_to_list(VM)),
        file:delete("/tmp/.aesophia"),
        list_to_binary(Res)
    end.
-endif.
