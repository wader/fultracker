$(EBIN_DIR)/bittorrent.beam: bittorrent.hrl
$(EBIN_DIR)/bittorrent.beam: peers.hrl
$(EBIN_DIR)/peers.beam: peers.hrl
$(EBIN_DIR)/peers.beam: bittorrent.hrl
$(EBIN_DIR)/torrent_handler.beam: bittorrent.hrl
$(EBIN_DIR)/torrent_handler.beam: peers.hrl
$(EBIN_DIR)/natcheck_handler.beam: bittorrent.hrl
