
-module(tracker_sup).
-behavior(supervisor).

% api
-export([start_link/0]).

% supervisor
-export([init/1]).

-define(SERVER_NAME, tracker_sup).


start_link() ->
    supervisor:start_link({local, ?SERVER_NAME}, ?MODULE, []).

init(_Args) ->
    {ok, {
     {one_for_one, 10, 10},
     [
      {"torrent_manager", {torrent_manager, start_link, []},
       permanent, brutal_kill, worker, [torrent_manager]}
     ]
    }}.

