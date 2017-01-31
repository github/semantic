#include "tree_sitter/runtime.h"
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
	return ts_node_type(*node, document);
}

size_t ts_node_p_child_count(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_child_count(*node);
}

size_t ts_node_p_named_child_count(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_named_child_count(*node);
}

void ts_node_p_child(const TSNode *node, size_t index, TSNode *outNode) {
	assert(node != NULL);
	assert(node->data != NULL);
	assert(outNode != NULL);
	TSNode temp = ts_node_child(*node, index);
	if (temp.data == NULL) {
		printf("got broken child for index %ld\n", index);
	}
	assert(temp.data != NULL);
	*outNode = temp;
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


size_t ts_node_p_start_char(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_char(*node);
}

size_t ts_node_p_end_char(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_char(*node);
}


size_t ts_node_p_start_point_row(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_point(*node).row;
}

size_t ts_node_p_start_point_column(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_start_point(*node).column;
}

size_t ts_node_p_end_point_row(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_point(*node).row;
}

size_t ts_node_p_end_point_column(const TSNode *node) {
	assert(node != NULL);
	assert(node->data != NULL);
	return ts_node_end_point(*node).column;
}
