-module(announce).
-export([out/1]).

-include("yaws_api.hrl").

out(Arg) ->
    {ok, {{A, B, C, D}, _Port}} = inet:peername(Arg#arg.clisock),
    IpBin = <<A, B, C, D>>,
    {content, "text/plain",
        bittorrent:announce_request(
            yaws_api:parse_query(Arg), IpBin)}.

