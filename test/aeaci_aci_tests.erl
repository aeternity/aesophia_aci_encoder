-ifdef(TEST).
-module(aeaci_aci_tests).

-include_lib("eunit/include/eunit.hrl").

encoder_test_() ->
    [{ "Test FATE encoder from ACI fixtures", fun test_fate_encoder_fixtures/0}
    ,{ "Test AEVM encoder from ACI fixtures", fun test_aevm_encoder_fixtures/0}].

fixtures() ->
    [B, _] = string:split(code:priv_dir(aesophia_aci_encoder), "_build"),
    {ok, JText} = file:read_file(filename:join([B , "test", "tests.json"])),
    jsx:decode(JText, [{labels, binary}, return_maps]).

test_fate_encoder_fixtures() ->
    #{<<"encode">> := #{<<"fate">> := FateFixtures}} = fixtures(),
    [begin
        [begin
             CompiledAci = aeaci_aci:from_string(Aci, #{backend => fate}),
             [begin
                  {ok, Calldata} = aeser_api_encoder:safe_decode(contract_bytearray, ClientCalldata),
                  {ok, EncodedCall} = aeaci_aci:encode_call_data(CompiledAci, Call),
                  %% io:format(user, "~p ~p\n", [EncodedCall, Calldata]),
                  ?assertEqual(Calldata, EncodedCall)
              end || #{<<"call">> := Call, <<"calldata">> := ClientCalldata} <- Tests]
         end || {Aci, Tests} <- maps:to_list(Cases)]
     end || {_Ver, Cases} <- maps:to_list(FateFixtures)],
    ok.

test_aevm_encoder_fixtures() ->
    #{<<"encode">> := #{<<"aevm">> := AEVMFixtures}} = fixtures(),
    [begin
        [begin
             CompiledAci = aeaci_aci:from_string(Aci, #{backend => aevm}),
             [begin
                  {ok, Calldata} = aeser_api_encoder:safe_decode(contract_bytearray, ClientCalldata),
                  {ok, EncodedCall} = aeaci_aci:encode_call_data(CompiledAci, Call),
                  %% io:format(user, "~s ~p \n", [Aci, Call]),
                  ?assertEqual(Calldata, EncodedCall)
              end || #{<<"call">> := Call, <<"calldata">> := ClientCalldata} <- Tests]
         end || {Aci, Tests} <- maps:to_list(Cases)]
     end || {_Ver, Cases} <- maps:to_list(AEVMFixtures)],
    ok.

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
