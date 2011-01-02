
-module(utils).

-export([queue_in_list/2, queue_out_list/2]).
-export([shuffle/1]).
-export([max/2]).
-export([clamp/3]).
-export([nowsec/0]).
-export([private_net/1]).
-export([peer_ip_to_string/1, peer_ip_to_tuple/1,
         peer_port_to_string/1, peer_port_to_integer/1,
         peer_id_to_client/1]).
-export([hex_string/1]).
-export([random_list/1]).
-export([list_join/2]).
-export([fuzzy_ago/1]).
-export([fuzzy_size/1]).


% same as in/out for queue but with list of values

queue_in_list([], Queue) ->
    Queue;
queue_in_list([H|T], Queue) ->
    queue_in_list(T, queue:in(H, Queue)).

queue_out_list(Num, Queue) ->
    queue_out_list(Num, Queue, []).
queue_out_list(0, Queue, Acc) ->
    {Acc, Queue};
queue_out_list(Num, Queue, Acc) ->
    case queue:out(Queue) of
    {empty, NewQueue} ->
        {Acc, NewQueue};
    {{value, Value}, NewQueue} ->
        queue_out_list(Num - 1, NewQueue, [Value|Acc])
    end.


% this is probably not a "perfect shuffle", to be perfect i think the
% tag numbers need to be unique, else duplicated tags will not have
% a probability to be swaped.
shuffle(List) ->
    [X || {_, X} <-
        lists:keysort(1, [{random:uniform(), X} || X <- List])].


max(A, B) when A > B -> A;
max(_A, B) -> B.


clamp(V, _Min, Max) when V > Max -> Max;
clamp(V, Min, _Max) when V < Min -> Min;
clamp(V, _Min, _Max) -> V.


nowsec() ->
    {MegaSec, Sec, _MicroSec} = erlang:now(),
    MegaSec * 1000000 + Sec.


private_net({192, 168, _, _}) -> true; % 192.168.0.0/16
private_net({10, _, _, _}) -> true; % 10.0.0.0/8
private_net({172, B, _, _}) when B >= 16, B =< 31 -> true; % 172.16.0.0/12
% These are private but probably not needed to check for
%private_net({169, 254, _, _}) -> true; % 169.254.0.0/16 
%private_net({192, 0, 2, _}) -> true; % 192.0.2.0/24 
private_net(_) -> false.


peer_ip_to_string(<<A, B, C, D>>) ->
    io_lib:format("~w.~w.~w.~w", [A, B, C, D]).

peer_ip_to_tuple(<<A, B, C, D>>) ->
    {A, B, C, D}.

peer_port_to_string(<<Port:16>>) ->
    io_lib:format("~w", [Port]).

peer_port_to_integer(<<Port:16>>) ->
    Port.

% TODO: add more from http://wiki.theory.org/BitTorrentSpecification
% move to bittorrent.erl?
peer_id_to_client(<<$M, V1, $-, V2, $-, V3, $-, $-, _Rest/binary>>) ->
    io_lib:format("BitTorrent ~c.~c.~c", [V1, V2, V3]);
peer_id_to_client(<<$-, C:2/binary, V1, V2, V3, V4, $-, _Rest/binary>>) ->
    io_lib:format("~s ~c.~c.~c.~c",
        [
        case binary_to_list(C) of
            "AZ" -> "Azureus";
            "UT" -> "uTorrent";
            _ -> "Unknown"
        end,
        V1, V2, V3, V4
        ]);
peer_id_to_client(_PeerId) ->
    "Unknown".


hex_char(C) when C =< 9 ->
    $0 + C;
hex_char(C) ->
    $a + (C - 10).

hex_string(Bin) when is_binary(Bin) ->
    hex_string(binary_to_list(Bin));
hex_string(List) when is_list(List) ->
    [[hex_char(N bsr 4), hex_char(N band 2#1111)] || N <- List].


random_list(N) ->
    [random:uniform(256) - 1 || _ <- lists:seq(1, N)].

list_join(List, _Sep) when length(List) < 2 ->
    List;
list_join([X, Y], Sep) ->
    [X, Sep, Y];
list_join([X|H], Sep) ->
    [X, Sep|list_join(H, Sep)].


fuzzy_ago(0) ->
    "Now";
fuzzy_ago(Sec) ->
    list_join(fuzzy_ago(Sec, 2), " ").
fuzzy_ago(Sec, N) when N == 0; Sec < 1 ->
    [""];
fuzzy_ago(Sec, N) when N > 0, Sec < 60 ->
    [io_lib:format("~p Sec", [Sec])];
fuzzy_ago(Sec, N) when N > 0, Sec < 60*60 ->
    [io_lib:format("~p Min", [Sec div 60])] ++
    fuzzy_ago(Sec rem 60, N - 1);
fuzzy_ago(Sec, N) when N > 0, Sec < 60*60*24 ->
    [io_lib:format("~p Hours", [Sec div (60*60)])] ++
    fuzzy_ago(Sec rem (60*60), N - 1);
fuzzy_ago(Sec, N) when N > 0 ->
    [io_lib:format("~p Days", [Sec div (60*60*24)])] ++
    fuzzy_ago(Sec rem (60*60*24), N - 1).


fuzzy_size(Size) when Size < 1000 ->
    io_lib:format("~p Byte", [Size]);
fuzzy_size(Size) when Size < 1000*1000 ->
    io_lib:format("~.1f KByte", [Size / 1000]);
fuzzy_size(Size) when Size < 1000*1000*1000 ->
    io_lib:format("~.1f MByte", [Size / (1000*1000)]);
fuzzy_size(Size) when Size < 1000*1000*1000*1000 ->
    io_lib:format("~.1f GByte", [Size / (1000*1000*1000)]);
fuzzy_size(Size) ->
    io_lib:format("~.1f TByte", [Size / (1000*1000*1000*1000)]).

