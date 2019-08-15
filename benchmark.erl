-module(benchmark).

-export([
    run_base64_encode/0,
    run_base64_decode/0,
    run_tolower/0,
    random_string/0
]).

-define(spawn_and_get_response(Fun),
    fun() ->
        MyPid = self(),
        spawn(
            fun() ->
                Result = Fun,
                MyPid ! Result
            end
        ),
        receive
            Result when is_binary(Result) -> Result
        after 100
            -> erlang:display("Did not receive result")
        end
    end
).

run_base64_encode() ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Encode", ?spawn_and_get_response(erl_string_encoders:base64_encode(random_string())), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Encode", ?spawn_and_get_response(base64:encode(random_string())), 200),
    eministat:x(95.0, DataSetC, DataSetErl).

run_base64_decode() ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Decode", ?spawn_and_get_response(erl_string_encoders:base64_decode(base64:encode(random_string()))), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Decode", ?spawn_and_get_response(base64:decode(base64:encode(random_string()))), 200),
    eministat:x(95.0, DataSetC, DataSetErl).

run_tolower() ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Lower", ?spawn_and_get_response(erl_string_encoders:to_lower(random_string())), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Lower", ?spawn_and_get_response(string:lowercase(random_string_upper())), 200),
    eministat:x(95.0, DataSetC, DataSetErl).

random_string() ->
    binary:list_to_bin(get_random_string(30, [
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
    ])).

random_string_upper() ->
    binary:list_to_bin(get_random_string(30, [
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
    ])).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
