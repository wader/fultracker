TOP = ..
SUBDIRS = yaws
EBIN = \
	bencode.beam \
	utils.beam \
	bittorrent.beam \
	peers.beam \
	peers_list.beam \
	peers_gb_trees.beam \
	peers_ets.beam \
	torrent_manager.beam \
	torrent_handler.beam \
	natcheck_handler.beam \
	tracker_sup.beam \
	tracker.beam

include $(TOP)/include.mk

$(EBIN_DIR)/bittorrent.beam: bittorrent.hrl
$(EBIN_DIR)/bittorrent.beam: peers.hrl
$(EBIN_DIR)/peers.beam: peers.hrl
$(EBIN_DIR)/peers.beam: bittorrent.hrl
$(EBIN_DIR)/torrent_handler.beam: bittorrent.hrl
$(EBIN_DIR)/torrent_handler.beam: peers.hrl
$(EBIN_DIR)/natcheck_handler.beam: bittorrent.hrl
