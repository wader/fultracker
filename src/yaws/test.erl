-module(test).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    {content, "text/html",
        io_lib:format("asdasd<pre>Arg=~p~nparse_query=~p</pre>",
            [Arg, yaws_api:parse_query(Arg)])}.

%out(Arg) ->
%    {content, "text/html", io_lib:format("~p", [Arg])}.

