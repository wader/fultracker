
% TODO:
% handle errors better
% periodtic n-get peers? not needed i think, be request driven insted
% self terminate on 0 peers..
% terminate if no message in <peer-timeout> time


-module(torrent_handler).
-behavior(gen_server).

% api
-export([start/0, announce/2, scrape/1, get_peers/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("bittorrent.hrl").
-include("peers.hrl").


-record(state, {
        peers,
        downloaded}).


start() ->
    gen_server:start(?MODULE, [], []).

announce(Pid, AnnReq) ->
    gen_server:call(Pid, {announce, AnnReq}).

natcheck_failed(Pid, PeerId) ->
    gen_server:cast(Pid, {natcheck_failed, PeerId}).

get_peers(Pid) ->
    gen_server:call(Pid, {get_peers}).

scrape(Pid) ->
    gen_server:call(Pid, {scrape}).

init(_Args) ->
    {ok, #state{peers=peers:new([], peers_gb_trees), downloaded=0}}.

handle_call({announce, #announce_req{event=Event, peer_id=PeerId} = AnnReq},
            _From,
            #state{peers=Peers} = State) ->
    case
        case {Event, peers:lookup(Peers, PeerId)} of
        {started, false} ->
            {ok, #peer{}, false};
        {_Event, {value, #peer{key=Key} = Peer}}
            when
                AnnReq#announce_req.key == Key ->
            {ok, Peer, true};
        {_Event, {value, Peer}} when is_record(Peer, peer) ->
            {error, "key mismatch"};
        {_Event, false} ->
            {error, "unknown id"}
        end
    of
    {ok, OldPeer, Found} ->
        NowSec = utils:nowsec(),

        NewPeer = 
            #peer{
                id=AnnReq#announce_req.peer_id,
                key=AnnReq#announce_req.key,
                ip=AnnReq#announce_req.ip,
                port=AnnReq#announce_req.port,
                left=AnnReq#announce_req.left,
                downloaded=AnnReq#announce_req.downloaded,
                downloaded_prev=OldPeer#peer.downloaded,
                uploaded=AnnReq#announce_req.uploaded,
                uploaded_prev=OldPeer#peer.uploaded,
                last_seen=NowSec,
                last_seen_prev=OldPeer#peer.last_seen,
                natcheck=undefined},
        
        {PeerList, NewPeers} =
            peers:get(
                State#state.peers,
                AnnReq,
                NowSec - ?PEER_TIMEOUT_INTERVAL),

        NewPeers2 =  
            case {Event, Found} of
            {started, false} ->
                %natcheck_manager:start_handler(self(), AnnReq),
                peers:add(NewPeers, NewPeer, OldPeer);
            {stopped, true} -> peers:delete(NewPeers, OldPeer);
            {_Event2, true} -> peers:enter(NewPeers, NewPeer, OldPeer)
            end,

        AnnRsp =
            #announce_rsp{
                peers=PeerList,
                complete=peers:seeds(NewPeers2),
                total=peers:size(NewPeers2)},

        {reply, {ok, AnnRsp},
            State#state{
                peers=NewPeers2,
                downloaded=State#state.downloaded +
                    if Event == completed -> 1; true -> 0 end}};
    {error, Error} ->
        {reply, {error, Error}, State}
    end;
handle_call({get_peers}, _From, #state{peers=Peers} = State) ->
    {reply, {ok, peers:peer_list(Peers)}, State};
% TODO: rename records etc... fix torrent_list.yaws too
handle_call({scrape}, _From, #state{peers=Peers} = State) ->
    Seeds=peers:seeds(Peers),
    Size=peers:size(Peers),
    {reply, 
        {ok,
            #scrape_rsp{
                complete=Seeds,
                downloaded=State#state.downloaded,
                incomplete=Size - Seeds
            }
        },
        State
    };

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({natcheck_failed, PeerId}, #state{peers=Peers} = State) ->
    case peers:lookup(Peers, PeerId) of
    {value, Peer} ->
        {reply, State#state{
            peers=peers:enter(Peers, Peer#peer{natcheck=false}, Peer)}
        };
    false ->
        {reply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

% TODO:
% if we get PEER_INTERVAL_TIMEOUT timeout it means no peer
% has been active... remove all peers?
%handle_info(timeout, State) ->
%    {_PeerList, NewPeers} =
%        peers:get(
%            State#state.peers,
%            #announce_req{numwant=peers:size(State#state.peers},
%            utils:nowsec() - ?PEER_INTERVAL_TIMEOUT),
%    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
