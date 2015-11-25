#include "bridge.h"
#include <assert.h>
#include <stdio.h>

void ts_document_root_node_p(TSDocument *document, TSNode *outNode) {
	assert(document != NULL);
	assert(outNode != NULL);
	*outNode = ts_document_root_node(document);
}


const char *ts_node_p_name(const TSNode *node, const TSDocument *document) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(document != NULL);
	return ts_node_name(*node, document);
}


size_t ts_node_p_named_child_count(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_named_child_count(*node);
}

void ts_node_p_named_child(const TSNode *node, size_t index, TSNode *outNode) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(outNode != NULL);
	TSNode temp = ts_node_named_child(*node, index);
	if (temp.data == NULL) {
		printf("got broken child for index %ld\n", index);
	}
	assert(temp.data != NULL);
	*outNode = temp;
}


size_t ts_node_p_pos_chars(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_pos(*node).chars;
}

size_t ts_node_p_size_chars(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_size(*node).chars;
}
