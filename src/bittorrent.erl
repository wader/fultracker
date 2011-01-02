% tracker protocol related stuff

-module(bittorrent).

-export([announce_request/2, announce_response/2, scrape_request/1]).

-include("bittorrent.hrl").
-include("peers.hrl").

-export([scrape_response/1]).



failure(Reason) ->
    {dict, [{"failure reason", Reason}]}.

% NOTE:
% keep dicts sorted
% TODO:
% reverse? then bencode noreverse?
announce_response(AnnRsp, AnnReq) ->
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

make_announce_req(L) ->
    make_announce_req(L, #announce_req{}).
make_announce_req([{"info_hash" = Key, Value}|T], A) ->
    case Value of
    V when length(V) == ?INFO_HASH_LENGTH ->
        make_announce_req(T, A#announce_req{info_hash=list_to_binary(Value)});
    _ ->
        {error, Key}
    end;
make_announce_req([{"peer_id" = Key, Value}|T], A) ->
    case Value of
    V when length(V) == ?PEER_ID_LENGTH ->
        make_announce_req(T, A#announce_req{peer_id=list_to_binary(Value)});
    _ ->
        {error, Key}
    end;
make_announce_req([{"port" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0, N =< 65535 ->
        make_announce_req(T, A#announce_req{port=(<<N:16>>)});
    _ ->
        {error, Key}
    end;
make_announce_req([{"uploaded" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        make_announce_req(T, A#announce_req{uploaded=N});
    _ ->
        {error, Key}
    end;
make_announce_req([{"downloaded" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        make_announce_req(T, A#announce_req{downloaded=N});
    _ ->
        {error, Key}
    end;
make_announce_req([{"left" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        make_announce_req(T, A#announce_req{left=N});
    _ ->
        {error, Key}
    end;
make_announce_req([{"compact", Value}|T], A) ->
    VA =
        case Value of
        "0" -> false;
        _ -> true
        end,
    make_announce_req(T, A#announce_req{compact=VA});
make_announce_req([{"event", Value}|T], A) ->
    VA = 
        case Value of
        "started" -> started;
        "stopped" -> stopped;
        "completed" -> completed;
        _ -> undefined
        end,
    make_announce_req(T, A#announce_req{event=VA});
make_announce_req([{"ip", Value}|T], A) ->
    case inet:ip(Value) of
    {ok, IpList} ->
        case utils:private_net(IpList) of
        false ->
            IpBin = list_to_binary(tuple_to_list(IpList)),
            make_announce_req(A#announce_req{ip=IpBin});
        true ->
            make_announce_req(T, A)
        end;
    _ ->
        make_announce_req(T, A)
    end;
make_announce_req([{"numwant" = Key, Value}|T], A) ->
    case string:to_integer(Value) of
    {N, []} when N >= 0 ->
        make_announce_req(T, A#announce_req{
            numwant=utils:clamp(N, 0, ?NUMWANT_MAX)});
    _ ->
        {error, Key}
    end;
make_announce_req([{"key", Value}|T], A) -> % TODO: limit?
    make_announce_req(T, A#announce_req{key=list_to_binary(Value)});
make_announce_req([{"trackerid", Value}|T], A) -> % TODO: limit?
    make_announce_req(T, A#announce_req{trackerid=list_to_binary(Value)});
make_announce_req([{_Key, _Value}|T], A) ->
    make_announce_req(T, A); % skip unknown
make_announce_req([], A) ->
    {ok, A}.


valid_announce_req(#announce_req{info_hash=undefined}) ->
    {error, "info_hash"};
valid_announce_req(#announce_req{peer_id=undefined}) ->
    {error, "peer_id"};
valid_announce_req(#announce_req{port=undefined}) ->
    {error, "port"};
valid_announce_req(#announce_req{uploaded=undefined}) ->
    {error, "uploaded"};
valid_announce_req(#announce_req{downloaded=undefined}) ->
    {error, "downloaded"};
valid_announce_req(#announce_req{compact=undefined}) ->
    {error, "compact"};
valid_announce_req(_A) ->
    ok.


% no ip parameter, use peer address
fix_announce_req(#announce_req{ip=undefined} = R, PeerIp) ->
    fix_announce_req(R#announce_req{ip=PeerIp}, PeerIp);
fix_announce_req(#announce_req{numwant=undefined} = R, PeerIp) ->
    fix_announce_req(R#announce_req{numwant=?NUMWANT_DEFAULT}, PeerIp);
% no left parameter, assume left=0
fix_announce_req(#announce_req{left=undefined} = R, PeerIp) ->
    fix_announce_req(R#announce_req{left=0}, PeerIp);
fix_announce_req(R, _PeerIp) ->
    R.

% TODO: noproc, call timoeus.. catch..
announce_request(QueryParamList, PeerIp) ->
    Bencode =
        case
            case
                case make_announce_req(QueryParamList) of
                {error, InvalidKey} ->
                    {error, "invalid " ++ InvalidKey ++ " parameter"};
                {ok, AnnReq1} ->
                    case valid_announce_req(AnnReq1) of
                    {error, MissingKey} ->
                        {error, "missing " ++ MissingKey ++ " parameter"};
                    ok ->
                        {ok, fix_announce_req(AnnReq1, PeerIp)}
                    end
                end
            of
            {ok, AnnReq} ->
                case torrent_manager:get_handler(
                        AnnReq#announce_req.info_hash, true) of
                {ok, Pid} ->
                    % TODO: handler is gone (when handler termination
                    % is implemented)
                    % catch... {'EXIT'...
                    % retry?
                    case torrent_handler:announce(Pid, AnnReq) of
                    {ok, AnnRsp} ->
                        {ok, announce_response(AnnRsp, AnnReq)};
                    {error, _} = R ->
                        R
                    end;
                {error, _} = R ->
                    R
                end;
            {error, _} = R ->
                R
            end
        of
        {ok, B} ->
            B;
        {error, Reason} ->
            failure(Reason)
        end,
    {ok, V} = bencode:encode(Bencode),
    V.


scrape_response(ScrapeRspList) ->
    {dict, [{
        "files",
        {dict, [{
            InfoHash,
            {dict, [
                {"complete", SR#scrape_rsp.complete},
                {"downloaded", SR#scrape_rsp.downloaded},
                {"incomplete", SR#scrape_rsp.incomplete}
            ]}
            } ||
            {InfoHash, SR} <- ScrapeRspList] 
        }
    }]}.

make_scrape_req(L) ->
    make_scrape_req(L, #scrape_req{}).
make_scrape_req([{"info_hash" = Key, Value}|T], SR) ->
    case Value of
    V when length(V) == ?INFO_HASH_LENGTH ->
        make_scrape_req(T, SR#scrape_req{info_hash=list_to_binary(Value)});
    _ ->
        {error, Key}
    end;
make_scrape_req([{_Key, _Value}|T], A) ->
    make_scrape_req(T, A); % skip unknown
make_scrape_req([], A) ->
    {ok, A}.

valid_scrape_req(#scrape_req{info_hash=undefined}) ->
    {error, "info_hash"};
valid_scrape_req(_SR) ->
    ok.


scrape_request(QueryParamList) ->
    Bencode =
        case
            case
                case make_scrape_req(QueryParamList) of
                {error, InvalidKey} ->
                    {error, "invalid " ++ InvalidKey ++ " parameter"};
                {ok, ScrapeReq1} ->
                    case valid_scrape_req(ScrapeReq1) of
                    {error, MissingKey} ->
                        {error, "missing " ++ MissingKey ++ " parameter"};
                    ok ->
                        {ok, ScrapeReq1}
                    end
                end
            of
            {ok, ScrapeReq} ->
                case torrent_manager:get_handler(
                        ScrapeReq#scrape_req.info_hash, false) of
                {ok, Pid} ->
                    % TODO: maybe race, handler has stopped..
                    % catch... {'EXIT'...
                    case torrent_handler:scrape(Pid) of
                    {ok, ScrapeRsp} ->
                        {ok, scrape_response(
                            [{ScrapeReq#scrape_req.info_hash, ScrapeRsp}])};
                    {error, _} = R ->
                        R
                    end;
                {error, _} = R ->
                    R
                end;
            {error, _} = R ->
                R
            end
        of
        {ok, B} ->
            B;
        {error, Reason} ->
            failure(Reason)
        end,
    {ok, V} = bencode:encode(Bencode),
    V.





% TODO: start of request generalization attempt
%scrape_request(Mod, ReqName, Create, QueryParamList, PeerIp) ->
%    Bencode =
%        case
%            case
%                case Mod:request(QueryParamList) of
%                {error, InvalidKey} ->
%                    {error, "invalid " ++ InvalidKey ++ " parameter"};
%                {ok, Req1} ->
%                    case Mod:validate(Req1) of
%                    {error, MissingKey} ->
%                        {error, "missing " ++ MissingKey ++ " parameter"};
%                    ok ->
%                        {ok, Mod:scrub(Req1)}
%                    end
%                end
%            of
%            {ok, Req} ->
%                case torrent_manager:get_handler(Req) of
%                {ok, Pid} ->
%                    case torrent_handler:ReqName(Req) of
%                    {ok, Rsp} -> {ok, Mod:response(Rsp)};
%                    {noproc
%                    end
%                {error, _} = R -> r;
%                _ -> {error, "internal error"}
%                    
%    
%
%                case torrent_manager:get_handler(
%                        ScrapeReq#scrape_req.info_hash, CreateIfMissing) of
%                {ok, Pid} ->
%                    % TODO: maybe race, handler has stopped..
%                    % catch... {'EXIT'...
%                    case torrent_handler:scrape(Pid) of
%                    {ok, ScrapeRsp} ->
%                        {ok, scrape_response(
%                            [{ScrapeReq#scrape_req.info_hash, ScrapeRsp}])};
%                    {error, _} = R ->
%                        R
%                    end;
%                {error, _} = R ->
%                    R
%                end;
%            {error, _} = R ->
%                R
%            end
%        of
%        {ok, B} ->
%            B;
%        {error, Reason} ->
%            failure(Reason)
%        end,
%    {ok, V} = bencode:encode(Bencode),
%    V.


