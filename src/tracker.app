{application, tracker,
  [{description,  "bittorrent tracker"},
   {id,           ""},
   {vsn,          0.1},
   {modules,      [torrent_manager, torrent_handler, mod_bittorrent, tracker, tracker_sup, bencode, utils]},
   {maxP,         infinity},
   {maxT,         infinity},
   {registered,   [tracker, torrent_manager]},
   {included_applications, []},
   {applications, []},
   {env,          []},
   {mod,          {tracker, []}},
   {start_phases, undefined}]}.
    
