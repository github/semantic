#include "tree_sitter/runtime.h"

extern TSLanguage *ts_language_c();
extern TSLanguage *ts_language_javascript();

void ts_document_root_node_p(TSDocument *document, TSNode *outNode);

const char *ts_node_p_name(const TSNode *node, const TSDocument *document);

size_t ts_node_p_named_child_count(const TSNode *node);
void ts_node_p_named_child(const TSNode *node, size_t index, TSNode *outNode);

size_t ts_node_p_pos_chars(const TSNode *node);
size_t ts_node_p_size_chars(const TSNode *node);

size_t ts_node_p_start_point(const TSNode *node);
size_t ts_node_p_end_point(const TSNode *node);
