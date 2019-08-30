-module(benchmark).

-export([
    run_c_base64_encode/0,
    run_erlang_base64_encode/0,
    run_c_base64_decode/0,
    run_erlang_base64_decode/0,
    run_c_tolower/0,
    run_erlang_tolower/0,
    run_c_web_safe_base64_encode/0,
    run_erlang_web_safe_base64_encode/0,
    run_c_web_safe_base64_decode/0,
    run_erlang_web_safe_base64_decode/0,
    generate_inputs/1,
    generate_uppercase_inputs/1,
    compare_two_runs/2
]).

-define(INPUTS_FILE, "inputs").
-define(AMOUNT_INPUTS, 1000).

-define(run(Fun),
    fun(Data) ->
        fun() ->
            Fun(lists:nth(rand:uniform(?AMOUNT_INPUTS), Data))
        end
    end
).

-define(generate_benchmark(BenchmarkName, Fun), 
    rand:seed(exs64, os:timestamp()),
    InputData = read_file(?INPUTS_FILE),
    DataSet = eministat:s(BenchmarkName, Fun(InputData), 200),
    {ok, Fd} = file:open(BenchmarkName, [write]),
    io:fwrite(Fd, "~s", [erlang:term_to_binary(DataSet)])
).

run_c_base64_encode() ->
    ?generate_benchmark("CBase64Encode", ?run(fun erl_string_encoders:base64_encode/1)).

run_erlang_base64_encode() ->
    ?generate_benchmark("ErlangBase64Encode", ?run(fun base64:encode/1)).

run_c_base64_decode() ->
    ?generate_benchmark("CBase64Decode", ?run(fun (FunData) ->
        Base64Data = base64:encode(FunData),
        erl_string_encoders:base64_decode(Base64Data)
    end)).

run_erlang_base64_decode() ->
    ?generate_benchmark("ErlangBase64Decode", ?run(fun (FunData) ->
        Base64Data = base64:encode(FunData),
        base64:decode(Base64Data)
    end)).

run_c_tolower() ->
    ?generate_benchmark("CLower", ?run(fun erl_string_encoders:to_lower/1)).

run_erlang_tolower() ->
    ?generate_benchmark("ErlangLower", ?run(fun string:lowercase/1)).

run_c_web_safe_base64_encode() ->
    ?generate_benchmark("CWebSafeBase64Encode", ?run(fun erl_string_encoders:web_safe_base64_encode/1)).

run_erlang_web_safe_base64_encode() ->
    ?generate_benchmark("ErlangWebSafeBase64Encode", ?run(fun base64_encode/1)).

run_c_web_safe_base64_decode() ->
    ?generate_benchmark("CWebSafeBase64Decode", ?run(fun (FunData) ->
        Base64Data = base64_encode(FunData),
        erl_string_encoders:web_safe_base64_decode(Base64Data)
    end)).

run_erlang_web_safe_base64_decode() ->
    ?generate_benchmark("ErlangWebSafeBase64Decode", ?run(fun (FunData) ->
        Base64Data = base64_encode(FunData),
        base64_decode(Base64Data)
    end)).

generate_inputs(Length) ->
    {ok, Fd} = file:open(?INPUTS_FILE, [write]),
    Inputs = [random_string(Length) || _A <- lists:seq(1, ?AMOUNT_INPUTS)],
    io:fwrite(Fd, "~s", [erlang:term_to_binary(Inputs)]).

generate_uppercase_inputs(Length) ->
    {ok, Fd} = file:open(?INPUTS_FILE, [write]),
    Inputs = [random_string_upper(Length) || _A <- lists:seq(1, ?AMOUNT_INPUTS)],
    io:fwrite(Fd, "~s", [erlang:term_to_binary(Inputs)]).

compare_two_runs(FileOne, FileTwo) ->
    ResultOne = read_file(FileOne),
    ResultTwo = read_file(FileTwo),
    eministat:x(99.0, ResultOne, ResultTwo).

% Private

read_file(Filename) ->
    {ok, File} = file:read_file(Filename),
    binary_to_term(File).

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

-spec base64_decode(binary()) -> binary().
base64_decode(Bin) ->
    Bin2 = binary:replace(Bin, <<" ">>, <<"+">>, [global]),
    Bin3 = binary:replace(Bin2, <<"-">>, <<"+">>, [global]),
    Bin4 = binary:replace(Bin3, <<"_">>, <<"/">>, [global]),
    Padding = case byte_size(Bin4) rem 4 of
        2 ->
            <<"==">>;
        3 ->
            <<"=">>;
        _ ->
            <<"">>
    end,
    base64:decode(<<Bin4/binary, Padding/binary>>).

-spec base64_encode(binary()) -> binary().
base64_encode(Bin) ->
    Bin2 = base64:encode(Bin),
    Bin3 = binary:replace(Bin2, <<"/">>, <<"_">>, [global]),
    binary:replace(Bin3, <<"+">>, <<"-">>, [global]).
