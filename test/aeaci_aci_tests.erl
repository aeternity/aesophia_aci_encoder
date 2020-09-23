-ifdef(TEST).
-module(aeaci_aci_tests).

-include_lib("eunit/include/eunit.hrl").

encoder_test_() ->
    [ { "Test FATE encoder from ACI fixtures on Iris contracts", fun test_fate_encoder_fixtures_iris/0}
    , { "Test AEVM encoder from ACI fixtures on Iris contracts", fun test_aevm_encoder_fixtures_iris/0}
    , { "Test FATE encoder from ACI fixtures on Lima contracts", fun test_fate_encoder_fixtures_lima/0}
    , { "Test AEVM encoder from ACI fixtures on Lima contracts", fun test_aevm_encoder_fixtures_lima/0}
    ].

fixtures_iris() ->
    aeaci_test_utils:fixture("tests_iris.json").

fixtures_lima() ->
    aeaci_test_utils:fixture("tests_lima.json").

test_fate_encoder_fixtures_iris() ->
     test_fate_encoder_fixtures(fixtures_iris()).

test_aevm_encoder_fixtures_iris() ->
    test_aevm_encoder_fixtures(fixtures_iris()).

test_fate_encoder_fixtures_lima() ->
     test_fate_encoder_fixtures(fixtures_lima()).

test_aevm_encoder_fixtures_lima() ->
    test_aevm_encoder_fixtures(fixtures_lima()).

test_fate_encoder_fixtures(#{<<"encode">> := #{<<"fate">> := FateFixtures}}) ->
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

test_aevm_encoder_fixtures(#{<<"encode">> := #{<<"aevm">> := AEVMFixtures}}) ->
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
-endif.
