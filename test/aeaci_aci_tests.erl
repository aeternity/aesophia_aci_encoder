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
                  io:format(user, "~s ~p \n", [Aci, Call]),
                  ?assertEqual(Calldata, EncodedCall)
              end || #{<<"call">> := Call, <<"calldata">> := ClientCalldata} <- Tests]
         end || {Aci, Tests} <- maps:to_list(Cases)]
     end || {_Ver, Cases} <- maps:to_list(AEVMFixtures)],
    ok.
