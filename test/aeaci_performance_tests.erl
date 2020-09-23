-ifdef(TEST).
-module(aeaci_performance_tests).

-include_lib("eunit/include/eunit.hrl").

performance_test_() ->
  {timeout, 60,
    [ {"Test speedup compated to aesophia", fun test_speed/0}
    ]}.

test_speed() ->
  Tests = aeaci_test_utils:fixture("tests_performance.json"),
  [test_speed(Test) || Test <- Tests],
  ok.

test_speed(#{<<"source">> := Source, <<"encodes">> := Encodes}) ->
  io:format(user, "\n\nStarting Performance Test\n", []),
  {ok, AeSoAci} = aeso_aci:contract_interface(json, Source, [{backend, fate}]),
  {ok, StubAci} = aeso_aci:contract_interface(string, Source, [{backend, fate}]),
  BinAeSoAci = jsx:encode(AeSoAci),
  Aci = aeaci_aci:from_string(BinAeSoAci, #{backend => fate}),
  [ begin
      StrFunArgs = lists:map(fun binary_to_list/1, FunArgs),
      AciCall = binary_to_list(FunName) ++ "(" ++ string:join(StrFunArgs, ",") ++ ")",
      io:format(user, "\nTesting encoding of ~p\n", [AciCall]),
      {ok, Calldata} = aeser_api_encoder:safe_decode(contract_bytearray, ClientCalldata),
      timed_execute(
        "Testing compiler with original contract",
        fun() -> aeso_compiler:create_calldata(binary_to_list(Source), binary_to_list(FunName), StrFunArgs, [{backend, fate}]) end,
        {ok, Calldata}, 10),
      timed_execute(
        "Testing compiler with stub contract",
        fun() -> aeso_compiler:create_calldata(binary_to_list(StubAci), binary_to_list(FunName), StrFunArgs, [{backend, fate}]) end,
        {ok, Calldata}, 100),
      timed_execute(
        "Testing aci library",
        fun() -> aeaci_aci:encode_call_data(Aci, AciCall) end,
        {ok, Calldata}, 10000)
    end || #{<<"call">> := [FunName, FunArgs], <<"calldata">> := ClientCalldata} <- Encodes],
  ok.

timed_execute(Msg, What, Expected, N) when is_function(What, 0) ->
  io:format(user, "~s N=~p\n", [Msg, N]),
  {Time, _} = timer:tc(fun() -> [begin
     ?assertEqual(Expected, What())
   end || _ <- lists:seq(1, N)] end),
  io:format(user, "Execution took: ~p us\nMean time: ~p us\n",
    [Time, Time/N]).

-endif.
