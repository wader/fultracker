
-module(torrent_manager).
-behaviour(gen_server).

% api
-export([start_link/0, get_handlers/0, get_handler/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
        handlers
        }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_handlers() ->
    gen_server:call(?MODULE, {get_handlers}).
get_handler(InfoHash, CreateIfMissing) ->
    gen_server:call(?MODULE, {get_handler, InfoHash, CreateIfMissing}).

    
init(_Args) ->
    {ok, #state{handlers=gb_trees:empty()}}.

handle_call({get_handler, InfoHash, CreateIfMissing}, _From,
            #state{handlers=Handlers} = State) ->
    case {gb_trees:lookup(InfoHash, Handlers), CreateIfMissing} of
    {{value, Pid}, _CreateIfMissing} ->
        {reply, {ok, Pid}, State};
    {none, true} ->
        {ok, Pid} = torrent_handler:start(),
        NewHandlers = gb_trees:insert(InfoHash, Pid, Handlers),
        {reply, {ok, Pid}, State#state{handlers = NewHandlers}};
    {none, false} ->
        {reply, {error, "unknown torrent"}, State}
    end;
handle_call({get_handlers}, _From, #state{handlers=Handlers} = State) ->
    {reply, {ok, gb_trees:to_list(Handlers)}, State};

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
 
