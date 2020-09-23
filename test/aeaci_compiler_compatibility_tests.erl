-ifdef(TEST).
-module(aeaci_compiler_compatibility_tests).

-include_lib("eunit/include/eunit.hrl").

compiler_test_() ->
    [ {"Test v2.1.0 compiler generated ACI", fun() -> test_compiler_aci(<<"v2_1_0">>) end}
    , {"Test v3.2.0 compiler generated ACI", fun() -> test_compiler_aci(<<"v3_2_0">>) end}
    ].

test_compiler_aci(Version) ->
    #{Version := Tests} = aeaci_test_utils:fixture("test_compatibility.json"),
    [begin
         aeaci_aci:from_string(Test, #{backend => fate})
     end || Test <- Tests].

-endif.
