#include "bridge.h"

void ts_document_root_node_p(TSDocument *document, TSNode *outNode) {
	*outNode = ts_document_root_node(document);
}

const char *ts_node_p_name(const TSNode *node, const TSDocument *document) {
	return ts_node_name(*node, document);
}

size_t ts_node_p_named_child_count(const TSNode *node) {
	return ts_node_named_child_count(*node);
}

void ts_node_p_named_child(const TSNode *node, size_t index, TSNode *outNode) {
	*outNode = ts_node_named_child(*node, index);
}
