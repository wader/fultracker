% not used yet, part of request generalization rewrite

% NOTE:
% keep dicts sorted
% TODO:
% reverse? then bencode noreverse?
response(AnnRsp, AnnReq) ->
    {dict, [
     {"complete", AnnRsp#announce_rsp.complete},
     {"incomplete", AnnRsp#announce_rsp.total - AnnRsp#announce_rsp.complete},
     {"interval", ?PEER_INTERVAL},
     {"min interval", ?PEER_MIN_INTERVAL},
     {"peers",
        case AnnReq#announce_req.compact of
        true ->
            [[Ip, Port] ||
            #peer{ip=Ip, port=Port} <- AnnRsp#announce_rsp.peers];
        false ->
            {list, [
                {dict, [
                 {"ip", utils:peer_ip_to_string(Ip)},
                 {"peer id", Id},
                 {"port", utils:peer_port_to_integer(Port)}
                ]} ||
                #peer{ip=Ip, port=Port, id=Id} <- AnnRsp#announce_rsp.peers]
            }
        end
     }]
    }.

request(L) ->
    request(L, #announce_req{}).
request([{"info_hash" = Key, Value}|T], A) ->
    case Value of
    V when length(V) == ?INFO_HASH_LENGTH ->
        request(T, A#announce_req{info_hash=list_to_binary(Value)});
    _ ->
        {error, Key}
    end;
request([{"peer_id" = Key, Value}|T], A) ->
    case Value of
    V when length(V) == ?PEER_ID_LENGTH ->
        request(T, A#announce_req{peer_id=list_to_binary(Value)});
    _ ->
        {error, Key}
    end;
request([{"port" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0, N =< 65535 ->
        request(T, A#announce_req{port=(<<N:16>>)});
    _ ->
        {error, Key}
    end;
request([{"uploaded" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        request(T, A#announce_req{uploaded=N});
    _ ->
        {error, Key}
    end;
request([{"downloaded" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        request(T, A#announce_req{downloaded=N});
    _ ->
        {error, Key}
    end;
request([{"left" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        request(T, A#announce_req{left=N});
    _ ->
        {error, Key}
    end;
request([{"compact", Value}|T], A) ->
    VA =
        case Value of
        "0" -> false;
        _ -> true
        end,
    request(T, A#announce_req{compact=VA});
request([{"event", Value}|T], A) ->
    VA = 
        case Value of
        "started" -> started;
        "stopped" -> stopped;
        "completed" -> completed;
        _ -> undefined
        end,
    request(T, A#announce_req{event=VA});
request([{"ip", Value}|T], A) ->
    case inet:ip(Value) of
    {ok, IpList} ->
        case utils:private_net(IpList) of
        false ->
            IpBin = list_to_binary(tuple_to_list(IpList)),
            request(A#announce_req{ip=IpBin});
        true ->
            request(T, A)
        end;
    _ ->
        request(T, A)
    end;
request([{"numwant" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        request(T, A#announce_req{
            numwant=utils:clamp(N, 0, ?NUMWANT_MAX)});
    _ ->
        {error, Key}
    end;
request([{"key", Value}|T], A) -> % TODO: limit?
    request(T, A#announce_req{key=list_to_binary(Value)});
request([{"trackerid", Value}|T], A) -> % TODO: limit?
    request(T, A#announce_req{trackerid=list_to_binary(Value)});
request([{_Key, _Value}|T], A) ->
    request(T, A); % skip unknown
request([], A) ->
    {ok, A}.

validate(#announce_req{info_hash=undefined}) ->
    {error, "info_hash"};
validate(#announce_req{peer_id=undefined}) ->
    {error, "peer_id"};
validate(#announce_req{port=undefined}) ->
    {error, "port"};
validate(#announce_req{uploaded=undefined}) ->
    {error, "uploaded"};
validate(#announce_req{downloaded=undefined}) ->
    {error, "downloaded"};
validate(#announce_req{compact=undefined}) ->
    {error, "compact"};
validate(_A) ->
    ok.

% no ip parameter, use peer address
scrub(#announce_req{ip=undefined} = R, PeerIp) ->
    scrub(R#announce_req{ip=PeerIp}, PeerIp);
% no numwant, assume default
scrub(#announce_req{numwant=undefined} = R, PeerIp) ->
    scrub(R#announce_req{numwant=?NUMWANT_DEFAULT}, PeerIp);
% no left parameter, assume 0
scrub(#announce_req{left=undefined} = R, PeerIp) ->
    scrub(R#announce_req{left=0}, PeerIp);
scrub(R, _PeerIp) ->
    R.

get_handler(#announce_req{info_hash=InfoHash}) ->
    torrent_manager:get_handler(InfoHash, true).

do((AnnReq) ->
    torrent_handler:announce(AnnReq)

