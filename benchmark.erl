-module(benchmark).

-export([
    run_base64_encode/2,
    run_base64_decode/2,
    run_tolower/2
]).

exhaust_mail_box(0) ->
    ok;
exhaust_mail_box(SpawnAmount) ->
    receive
        Result when is_binary(Result) -> exhaust_mail_box(SpawnAmount - 1)
    end.

-define(spawn_and_get_response(Fun),
    fun (Mode, SpawnAmount) ->
        fun() ->
            case Mode of
                parallel ->
                    MyPid = self(),
                    [ spawn(
                        fun() ->
                            Result = Fun,
                            MyPid ! Result
                        end
                    ) || _A <- lists:seq(1, SpawnAmount)],
                    exhaust_mail_box(SpawnAmount);
                sequential ->
                    Fun
            end
        end
    end
).

run_base64_encode(Length, {TestMode, Amount}) ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Encode", ?spawn_and_get_response(erl_string_encoders:base64_encode(random_string(Length)))(TestMode, Amount), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Encode", ?spawn_and_get_response(base64:encode(random_string(Length)))(TestMode, Amount), 200),
    eministat:x(95.0, DataSetC, DataSetErl);
run_base64_encode(Length, TestMode) ->
    run_base64_encode(Length, {TestMode, 1}).

run_base64_decode(Length, {TestMode, Amount}) ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Decode", ?spawn_and_get_response(erl_string_encoders:base64_decode(base64:encode(random_string(Length))))(TestMode, Amount), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Decode", ?spawn_and_get_response(base64:decode(base64:encode(random_string(Length))))(TestMode, Amount), 200),
    eministat:x(95.0, DataSetC, DataSetErl);
run_base64_decode(Length, TestMode) ->
    run_base64_decode(Length, {TestMode, 1}).

run_tolower(Length, {TestMode, Amount}) ->
    rand:seed(exs64, os:timestamp()),
    DataSetC = eministat:s("CBase64Lower", ?spawn_and_get_response(erl_string_encoders:to_lower(random_string_upper(Length)))(TestMode, Amount), 200),
    timer:sleep(5000),
    DataSetErl = eministat:s("ErlangBase64Lower", ?spawn_and_get_response(string:lowercase(random_string_upper(Length)))(TestMode, Amount), 200),
    eministat:x(95.0, DataSetC, DataSetErl);
run_tolower(Length, TestMode) ->
    run_tolower(Length, {TestMode, 1}).

random_string(Length) ->
    binary:list_to_bin(get_random_string(Length, [
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
    ])).

random_string_upper(Length) ->
    binary:list_to_bin(get_random_string(Length, [
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
    ])).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
