
-module(natcheck_manager).
-behaviour(gen_server).

% api
-export([start_link/0, get_handler/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
        handlers
        }).


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_handler(TorrentHandler, AnnReq) ->
    gen_server:cast(?MODULE, {start_handler, TorrentHandler, AnnReq}).

stop_handler(AnnReq) ->
init(_Args) ->
    {ok, #state{handlers=gb_trees:empty()}}.

handle_call({start_handler, Handler, AnnReq#announce_req{ip=Ip, port=Port}},
            _From, #state{handlers=Handlers} = State) ->
    {Pid, NewHandlers} =
        case {gb_trees:lookup({Ip, Port}, Handlers) of
        {value, Pid} ->
            {Pid, Handlers};
        true ->
            % TODO: error check?
            {ok, Pid} = natcheck_handler:start(AnnReq),
            {Pid, gb_trees:insert(InfoHash, Pid, Handlers)}
        end,
    natcheck_handler:add_peer(Ip, Host),
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
