#include "tree_sitter/runtime.h"

void ts_document_root_node_p(TSDocument *document, TSNode *outNode);

const char *ts_node_p_name(const TSNode *node, const TSDocument *document);

size_t ts_node_p_named_child_count(const TSNode *node);
