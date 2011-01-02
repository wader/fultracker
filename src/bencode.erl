% Authors:
% Mattias Wadman <mattias.wadman@galaxen.se>
%
% Bencoding is the format used in responses from the
% tracker, its also the format used in .torrent files.
% http://wiki.theory.org/BitTorrentSpecification
%
% Bencode = integer() | 
%           BencodeString |
%           {list, BencodeList}
%           {dict, BencodeDict}
% BencodeString = string() | binary()
% BencodeList = [Bencode]
% BencodeDict = [BencodeDictKeyValue]
% BencodeDictKeyValue= {BencodeString, Bencode}
%
% decode(string()) -> {ok, Bencode} | {error, Reason}
%
% encode(Bencode) -> Result
% encode(Bencode), Opts) -> Result
% Result = {ok, string()} | {ok, IoList} | {error, Reason}
% Opts = [Opt]
% Opt = sort | flatten
%
% sort, sort dictionaries, assume not already sorted (slower)
% flatten, flatten iolist (slower)
%
% TODO:
% use binaries somehow? should work with iolists?
% noreverse? assume reversed dicts and lists
%
% $Id$

-module(bencode).

-export([decode/1, encode/1, encode/2]).
-export([regress/0, test_file/1, test_dir/1, generate_struct/1,
         decode_random/1]).


decode(L) ->
    case
        case catch decode_aux(L) of
        {'EXIT', R}  ->
            {error, R};
        {error, _} = R ->
            R;
        {V, []} ->
            {ok, V};
        {V, Rest} ->
            {error, {decode_stray_data, V, Rest}}
        end
    of
    {error, Reason} ->
        {error, {bencode, Reason}};
    {ok, _} = Return ->
        Return
    end.

decode_aux([$i|T] = L) ->
    case T of
    [H|_T] when H >= $0, H =< $9; H == $- ->
        case string:to_integer(T) of
        {Num, [$e|Rest]} ->
            {Num, Rest};
        _ ->
            throw({error, {decode_invalid_integer, L}})
        end;
    _ ->
        throw({error, {decode_invalid_integer, L}})
    end;

decode_aux([$l|T]) ->
    decode_aux_list(T, []);

decode_aux([$d|T]) ->
    decode_aux_dict(T, []);

decode_aux([H|_T] = L) when H >= $0, H =< $9 ->
    case string:to_integer(L) of
    {Num, [$:|Rest]} when Num =< length(Rest) ->
        lists:split(Num, Rest);
    _ ->
        throw({error, {decode_invalid_string, L}})
    end;

decode_aux(L) ->
    throw({error, {invalid_type, L}}).

decode_aux_list([$e|T], Acc) ->
    {{list, lists:reverse(Acc)}, T};
decode_aux_list(L, Acc) ->
    {Value, Rest} = decode_aux(L),
    decode_aux_list(Rest, [Value|Acc]).

decode_aux_dict([$e|T], Acc) ->
    {{dict, lists:reverse(Acc)}, T};

decode_aux_dict(L, Acc) ->
    case decode_aux(L) of
    {Key, Rest} when is_list(Key) ->
        {Value, Rest2} = decode_aux(Rest),
        decode_aux_dict(Rest2, [{Key, Value}|Acc]);
    _ ->
        throw({error, {decode_invalid_dict_key, L}})
    end.


encode(V) ->
    encode(V, []).
encode(V, Opts) ->
    case
        case catch encode_aux(V, Opts) of
        {'EXIT', _} = R ->
            R;
        {error, _} = R ->
            R;
        L ->
            {ok,
            case lists:member(flatten, Opts) of
            true -> lists:flatten(L);
            false -> L
            end
            }
        end
    of
    {error, Reason} ->
        {error, {bencode, Reason}};
    {ok, _} = Return ->
        Return
    end.
    

encode_aux(V, _Opts) when is_integer(V) ->
    [$i, integer_to_list(V), $e];

encode_aux(V, _Opts) when is_list(V); is_binary(V) ->
    [integer_to_list(iolist_size(V)), $:, V];

encode_aux({list, V}, Opts) when is_list(V) ->
    encode_aux_list(V, [], Opts);
encode_aux({dict, V}, Opts) when is_list(V) ->
    encode_aux_dict(
        case lists:member(sort, Opts) of
        true ->
            case catch lists:keysort(1, V) of
            {'EXIT', _Reason} ->
                throw({error, {encode_invalid_dict, V}});
            S ->
                S
            end;
        false -> V
        end,
        [], Opts);
encode_aux(V, _Opts) ->
    throw({error, {encode_invalid_value, V}}).

encode_aux_list([H|T], Acc, Opts) ->
    encode_aux_list(T, [encode_aux(H, Opts)|Acc], Opts);
encode_aux_list([], Acc, _Opts) ->
    [$l, lists:reverse(Acc), $e].

encode_aux_dict([{Key, Value}|T], Acc, Opts)
    when is_list(Key); is_binary(Key) ->
    % key, value in reverse order, list is reversed in base case
    encode_aux_dict(T, [encode_aux(Value, Opts),
                        encode_aux(Key, Opts)|Acc], Opts);
encode_aux_dict([], Acc, _Opts) ->
    [$d, lists:reverse(Acc), $e];
encode_aux_dict(V, _Acc, _Opts) ->
    throw({error, {encode_invalid_dict, V}}).
    



% only benchmark and test code below

test_file(Path) ->
    io:fwrite("~s~n", [Path]),
    case file:read_file(Path) of
    {error, Reason} ->
        io:fwrite("read failed~n~p~n", [Reason]);
    {ok, Bin} ->
        case decode(binary_to_list(Bin)) of
        {error, Reason2} ->
            io:fwrite("decode failed~n~p~n", [Reason2]);
        {ok, U} ->
            case encode(U) of
            {error, Reason3} ->
                io:fwrite("encode failed~n~p~n", [Reason3]);
            {ok, E} ->
                BinE = list_to_binary(E),
                if Bin == BinE ->
                    io:fwrite("ok~n", []);
                true ->
                    io:fwrite("mismatch~n", [])
                end
            end
        end
    end.

test_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(
        fun test_file/1,
        lists:filter({filelib,is_regular},
                     [filename:join([Dir, "/", E]) || E <- Files])
        ).

decode_random(N) ->
    R = utils:random_list(N),
    D = decode(R),
    case
        case D of
        {error, {bencode, _}} -> true;
        {error, _} -> false;
        _V -> true
        end
    of
    true -> ok;
    false ->
        io:fwrite("R=~p D=~p~n", [R, D])
    end.

generate_struct(N) ->
    {list,
     lists:duplicate(N,
                     {dict, [
                        {"ip", "255.255.255.255"},
                        {"port", "65535"},
                        {"peer_id", "WIFJDSFJODSFKOKEWFOKEWFOKEWFKOEWFKOK"}]}
                        )
    }.

test(Encoded, Decoded) ->
    {ok, Encoded} = encode(Decoded),
    {ok, Decoded} = decode(Encoded).

regress() ->
    test("i0e", 0),
    test("0:", ""),
    test("le", {list, []}),
    test("de", {dict, []}),
    test("i123e", 123),
    test("i-123e", -123),
    test("4:test", "test"),
    test("li1ei2ei3ee", {list, [1,2,3]}),
    test("d4:key1i1e4:key2i2ee", {dict, [{"key1", 1}, {"key2", 2}]}),
    test("li123e3:abcli1ei2ei3eed3:123i123e3:abc3:abc4:dictd3:123i123ee4:listli1ei2ei3eeee",
         {list, [123, "abc", {list, [1,2,3]},
          {dict, [
            {"123", 123},
            {"abc", "abc"},
            {"dict", {dict, [{"123", 123}]}},
            {"list", {list, [1,2,3]}}]}]}
        ),
    ok.

