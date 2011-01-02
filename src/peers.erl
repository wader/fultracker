% Datatype to keep track of peers
% 
% id_queue is lazily updated, peers get removed (not inserted again)
% when calling peers:get and the peer id is not found.
% This way we can have a simple fifo data structure and not have traverse
% it on peer remove.
% 
% Peers are stored by peers_list or peers_gb_tree which abstracts away
% how the peers are stored (list or a tree).

% test performance...
% regress
% peers_*:list -> not key,value list? (need chagen in change_mod)
% limit where trees gets faster then list..

-module(peers).

-export([new/2, change_mod/2, get/3, enter/3, add/3,
         delete/2, lookup/2, peer_list/1, size/1, seeds/1]).

-export([gen_peer/0, gen_peers/1, test1/4]).

-include("peers.hrl").
-include("bittorrent.hrl").

-record(peers, {
        id_queue,
        seeds,
        module,
        module_state
        }).


new(PeerList, Mod) ->
    IdList = [Id || #peer{id=Id} <- PeerList],
    enter(
        #peers{
            id_queue=utils:queue_in_list(IdList, queue:new()),
            seeds=0,
            module=Mod,
            module_state=Mod:empty()},
        PeerList).

change_mod(#peers{module=Mod, module_state=ModState} = Peers, NewMod) ->
    PeerList = [Peer || {_Key, Peer} <- Mod:list(ModState)],
    enter(
        Peers#peers{
            seeds=0,
            module=NewMod,
            module_state=NewMod:empty()},
        PeerList).

get(Peers, AnnReq, LastSeenLimit) ->
    get(Peers, AnnReq, LastSeenLimit, [], peers:size(Peers)).
get(Peers, AnnReq, _LastSeenLimit, PeerList, PeersLeft)
    when
        PeersLeft == 0;
        length(PeerList) >= AnnReq#announce_req.numwant ->
    {PeerList, Peers};
get(#peers{
        id_queue=IdQueue,
        module=Mod,
        module_state=ModState,
        seeds=Seeds} = Peers,
        AnnReq, LastSeenLimit, PeerListAcc, PeersLeft) ->
    

    NumFetch =
        utils:clamp(
            0,
            PeersLeft,
            utils:max(
                % FIXME: other min value? here to prevent doing many
                % recursions
                AnnReq#announce_req.numwant div 2,
                AnnReq#announce_req.numwant - length(PeerListAcc))),
    
    {IdList, RestIdQueue} = utils:queue_out_list(NumFetch, IdQueue),

    {GoodIdList, GoodPeerList, NewModState, SeedsRemoved} =
        lists:foldr(
            fun
                % requesting peer, keep
                ({value, #peer{id=Id}},
                 {GoodIdListAcc, GoodPeerListAcc, ModStateAcc, SeedsRemovedAcc})
                    when Id == AnnReq#announce_req.peer_id ->
                    {[Id|GoodIdListAcc],
                     GoodPeerListAcc,
                     ModStateAcc,
                     SeedsRemovedAcc};
                % good peer, keep and return to requester
                ({value, #peer{id=Id, last_seen=LastSeen} = Peer},
                 {GoodIdListAcc, GoodPeerListAcc, ModStateAcc, SeedsRemovedAcc})
                    when LastSeen > LastSeenLimit ->
                    {[Id|GoodIdListAcc],
                     [Peer|GoodPeerListAcc],
                     ModStateAcc,
                     SeedsRemovedAcc};
                % timeouted, bad peer, put on delete list
                ({value, #peer{id=Id, left=Left}},
                 {GoodIdListAcc, GoodPeerListAcc, ModStateAcc, SeedsRemovedAcc}) ->
                    {GoodIdListAcc,
                     GoodPeerListAcc,
                     Mod:delete(ModStateAcc, Id),
                     SeedsRemovedAcc +
                        if Left == 0 -> 1; true -> 0 end};
                % peer not found, peer has requested to be removed
                (false, Acc) ->
                    Acc
            end,
            {[], PeerListAcc, ModState, 0},
            [Mod:lookup(ModState, Id) || Id <- IdList]),
    
    NewPeers =
        Peers#peers{
            % TODO: could shuffle here, dont thinks its needed
            id_queue=utils:queue_in_list(GoodIdList, RestIdQueue),
            module_state=NewModState,
            seeds=Seeds - SeedsRemoved},

    get(NewPeers, AnnReq, LastSeenLimit, GoodPeerList, PeersLeft - NumFetch).

enter(#peers{module=Mod, module_state=ModState, seeds=Seeds} = Peers,
      #peer{id=Id, left=NewLeft} = Peer,
      #peer{left=OldLeft}) ->
    Peers#peers{
        module_state=Mod:enter(ModState, Id, Peer),
        seeds=Seeds +
            if
                NewLeft == 0, OldLeft /= 0 -> 1;
                OldLeft == 0, NewLeft /= 0 -> -1;
                true -> 0
            end
        }.

enter(Peers, PeerList) ->
    lists:foldr(
        fun(Peer, PeersAcc) ->
            enter(PeersAcc, Peer, #peer{})
        end,
        Peers,
        PeerList).

add(#peers{id_queue=IdQueue} = Peers, #peer{id=Id} = Peer, OldPeer) ->
    enter(Peers#peers{id_queue=queue:in(Id, IdQueue)}, Peer, OldPeer).

delete(#peers{module=Mod, module_state=ModState, seeds=Seeds} = Peers,
       #peer{id=Id, left=Left}) ->
    Peers#peers{
        module_state=Mod:delete(ModState, Id),
        seeds=Seeds - if Left == 0 -> 1; true -> 0 end
        }.

lookup(#peers{module=Mod, module_state=ModState}, Id) ->
    Mod:lookup(ModState, Id).

peer_list(#peers{module=Mod, module_state=ModState}) ->
    [Peer || {_Key, Peer} <- Mod:list(ModState)].

size(#peers{module=Mod, module_state=ModState}) ->
    Mod:size(ModState).

seeds(#peers{seeds=Seeds}) ->
    Seeds.



% tests, regression etc.

gen_peer() ->
    #peer{
        id=list_to_binary(utils:random_list(2)),
        key=list_to_binary(utils:random_list(2)),
        ip=list_to_binary(utils:random_list(4)),
        port=list_to_binary(utils:random_list(2)),
        left=random:uniform(2) - 1,
        last_seen=random:uniform(1000)
    }.

gen_peers(Num) ->
    [gen_peer() || _ <- lists:seq(1, Num)].


test1(NumPeers, NumWant, PeerId, LastSeenLimit) ->
    PL = gen_peers(NumPeers),
    P = new(PL, peers_gb_trees),
    get(P, #announce_req{numwant=NumWant, peer_id=PeerId}, LastSeenLimit).

