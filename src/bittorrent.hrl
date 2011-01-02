
-define(NUMWANT_DEFAULT, 50).
-define(NUMWANT_MAX, 100).
-define(PEER_INTERVAL, 600).
-define(PEER_MIN_INTERVAL, 660).
-define(PEER_TIMEOUT_INTERVAL, 720).
-define(INFO_HASH_LENGTH, 20). % 160 bit sha1 hash
-define(PEER_ID_LENGTH, 20).

% fields has same name as the ones used in the spec

-record(announce_req, {
        info_hash,
        peer_id,
        port,
        uploaded,
        downloaded,
        left,
        compact,
        event,
        ip,
        numwant,
        key,
        trackerid
        }).

-record(announce_rsp, {
        peers,
        complete,
        total
        }).

-record(scrape_req, {
        info_hash
        }).

-record(scrape_rsp, {
        complete,
        downloaded,
        incomplete
        }).
