-module(scrape).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    {content, "text/plain",
        bittorrent:scrape_request(yaws_api:parse_query(Arg))}.

