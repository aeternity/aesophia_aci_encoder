-ifdef(TEST).
-module(aeaci_test_utils).

-export([fixture/1]).

fixture(What) ->
    [B, _] = string:split(code:priv_dir(aesophia_aci_encoder), "_build"),
    {ok, JText} = file:read_file(filename:join([B , "test", What])),
    jsx:decode(JText, [{labels, binary}, return_maps]).

-endif.
