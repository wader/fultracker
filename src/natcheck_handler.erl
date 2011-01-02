% TODO:
%
% natcheck_mananger.... {ip, port} cache?
%


-module(natcheck_handler).

% api
-export([start/2, check/2]).

-include("bittorrent.hrl").

-define(NATCHECK_TIMEOUT, 10000).
-define(NATCHECK_PROTOCOL, "BitTorrent protocol").
-define(NATCHECK_PROTOCOL_LENGTH, 19).
-define(NATCHECK_PRPTOCOL_RESERVED_LENGTH, 8).

start(TorrentHandler, AnnReq) ->
    proc_lib:spawn(?MODULE, check, [TorrentHandler, AnnReq]).

check(TorrentHandler, AnnReq) ->
    case catch handshake(AnnReq) of
    ok ->
        io:format("natcheck ok~n", []),
        ok;
    {'EXIT', Reason} ->
        io:format("natcheck failed ~p ~n", [Reason])
    %    torrent_handler:natcheck_failed(
    %        TorrentHandler,
    %        AnnReq#announce_req.peer_id)
    end.

% nodelay?
% {packet, 1} does not work... <plength><protocol><0*8><info_hash>
handshake(AnnReq) ->
    {ok, Socket} =
        gen_tcp:connect(
            utils:peer_ip_to_tuple(AnnReq#announce_req.ip),
            utils:peer_port_to_integer(AnnReq#announce_req.port),
            [{active, false}, {packet, raw}, binary],
            ?NATCHECK_TIMEOUT),
   
    ok =
        gen_tcp:send(
            Socket,
            <<?NATCHECK_PROTOCOL_LENGTH,                    % prot string length
              ?NATCHECK_PROTOCOL,                           % prot string
              0:(8*8),                                      % 8 reserved bytes
              (AnnReq#announce_req.info_hash)/binary>>),% info_hash
    
    {ok,
        <<?NATCHECK_PROTOCOL_LENGTH,
          ?NATCHECK_PROTOCOL,
          _Reserved:8/binary,
          InfoHash:20/binary,
          PeerId:20/binary>>} =
        gen_tcp:recv(Socket, 1 + ?NATCHECK_PROTOCOL_LENGTH + 8 + 20 + 20),

    gen_tcp:close(Socket),

    InfoHash = AnnReq#announce_req.info_hash,
    PeerId = AnnReq#announce_req.peer_id,

    ok.

