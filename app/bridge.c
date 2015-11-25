#include "bridge.h"

void ts_document_root_node_p(TSDocument *document, TSNode *outNode) {
	*outNode = ts_document_root_node(document);
}

const char *ts_node_p_name(const TSNode *node, const TSDocument *document) {
	return ts_node_name(*node, document);
}
