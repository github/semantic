#include "bridge.h"

void ts_document_root_node_p(TSDocument *document, TSNode *outNode) {
	*outNode = ts_document_root_node(document);
}
