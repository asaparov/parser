/**
 * parser_data.h
 *
 *  Created on: May 3, 2017
 *      Author: asaparov
 */

#ifndef PARSER_DATA_H_
#define PARSER_DATA_H_

#include "datalog.h"
#include "inmind.h"

#include <grammar/grammar.h>

inline void cleanup(array<datalog_expression_root*>& data) {
	for (datalog_expression_root* e : data) {
		free(*e); if (e->root.reference_count == 0) free(e);
	}
}

inline void cleanup(array<datalog_expression_root*>& data,
		sequence* sentences, datalog_expression_root** logical_forms = NULL,
		unsigned int sentence_count = 0, syntax_node<datalog_expression_root>** syntax = NULL)
{
	cleanup(data);
	if (sentences != NULL) {
		for (unsigned int i = 0; i < sentence_count; i++)
			free(sentences[i]);
		free(sentences);
	}
	if (logical_forms != NULL) {
		for (unsigned int i = 0; i < sentence_count; i++) {
			free(*logical_forms[i]);
			if (logical_forms[i]->root.reference_count == 0)
				free(logical_forms[i]);
		}
		free(logical_forms);
	}
	if (syntax != NULL) {
		for (unsigned int i = 0; i < sentence_count; i++) {
			if (syntax[i] == NULL) continue;
			free(*syntax[i]);
			if (syntax[i]->reference_count == 0)
				free(syntax[i]);
		}
		free(syntax);
	}
}

inline void cleanup(array<datalog_expression_root*>& geoquery_data, sequence* sentences,
		datalog_expression_root** logical_forms = NULL, unsigned int* nonterminals = NULL,
		unsigned int sentence_count = 0, syntax_node<datalog_expression_root>** syntax = NULL)
{
	if (nonterminals != NULL) free(nonterminals);
	cleanup(geoquery_data, sentences, logical_forms, sentence_count, syntax);
}

inline bool to_sequence(
		const datalog_expression* src, sequence& dst,
		hash_map<string, unsigned int>& token_map)
{
	if (src->type != DATALOG_LIST || !init(dst, src->list.elements.length)) return false;
	for (unsigned int i = 0; i < src->list.elements.length; i++) {
		if (src->list.elements[i]->type == DATALOG_CONSTANT) {
			dst.tokens[i] = src->list.elements[i]->constant.label;
		} else if (src->list.elements[i]->type == DATALOG_INTEGER) {
			int length = snprintf(NULL, 0, "%d", src->list.elements[i]->integer);
			string buffer = string(length + 1);
			buffer.length = length;
			if (snprintf(buffer.data, length + 1, "%d", src->list.elements[i]->integer) != length
			 || !get_token(buffer, dst.tokens[i], token_map))
				return false;
		} else {
			fprintf(stderr, "to_sequence ERROR: Unexpected token type.\n");
			free(dst); return false;
		}
	}
	return true;
}

bool read_data(array<datalog_expression_root*>& data,
		hash_map<string, unsigned int>& names, const char* data_filepath)
{
	/* first lex the data */
	array<datalog_token> tokens = array<datalog_token>(1024);
	FILE* input = fopen(data_filepath, "r");
	if (input == NULL) {
		fprintf(stderr, "ERROR: Unable to open '%s' for reading.\n", data_filepath);
		return false;
	}
	datalog_lex<true>(tokens, input);

	/* interpret the lexed data */
	if (!datalog_interpret(tokens, data, names)) {
		free_tokens(tokens);
		cleanup(data);
		fprintf(stderr, "ERROR: Unable to parse '%s'.\n", data_filepath);
		return false;
	}
	free_tokens(tokens);
	return true;
}

bool read_data(
		array<datalog_expression_root*>& data,
		hash_map<string, unsigned int>& names, sequence*& sentences,
		datalog_expression_root**& logical_forms, const char* data_filepath)
{
	if (!read_data(data, names, data_filepath)) return false;

	/* get the list of sentences and logical forms from the data */
	sentences = NULL; logical_forms = NULL;
	sentences = (sequence*) malloc(sizeof(sequence) * data.length);
	logical_forms = (datalog_expression_root**) malloc(sizeof(datalog_expression_root*) * data.length);
	if (sentences == NULL || logical_forms == NULL) {
		if (sentences != NULL) free(sentences);
		if (logical_forms != NULL) free(logical_forms);
		cleanup(data);
		return false;
	}

	for (unsigned int i = 0; i < data.length; i++) {
		if (data[i]->root.type != DATALOG_PREDICATE
		 || data[i]->root.pred.args[0] == NULL || data[i]->root.pred.args[1] == NULL
		 || !to_sequence(data[i]->root.pred.args[0], sentences[i], names)
		 || !init(logical_forms[i], data[i]->root.pred.args[1])) {
			cleanup(data, sentences, logical_forms, i); return false;
		}

		if (!valid_variable_scope(logical_forms[i]->root)) {
			fprintf(stderr, "read_data ERROR: The logical form for sentence %u in '%s' has invalid scope.\n", i, data_filepath);
			cleanup(data, sentences, logical_forms, i + 1);
			return false;
		}
	}
	return true;
}

bool read_inmind_data(array<datalog_expression_root*>& data,
		hash_map<string, unsigned int>& names, const char* data_filepath)
{
	/* first lex the data */
	array<inmind_token> tokens = array<inmind_token>(1024);
	FILE* input = fopen(data_filepath, "r");
	if (input == NULL) {
		fprintf(stderr, "ERROR: Unable to open '%s' for reading.\n", data_filepath);
		return false;
	}
	inmind_lex(tokens, input);

	/* interpret the lexed data */
	if (!inmind_interpret(tokens, data, names)) {
		free_tokens(tokens);
		cleanup(data);
		fprintf(stderr, "ERROR: Unable to parse '%s'.\n", data_filepath);
		return false;
	}
	free_tokens(tokens);
	return true;
}

bool read_inmind_data(
		array<datalog_expression_root*>& data,
		hash_map<string, unsigned int>& names, sequence*& sentences,
		datalog_expression_root**& logical_forms, const char* data_filepath)
{
	if (!read_inmind_data(data, names, data_filepath)) return false;

	/* get the list of sentences and logical forms from the data */
	sentences = NULL; logical_forms = NULL;
	sentences = (sequence*) malloc(sizeof(sequence) * data.length);
	logical_forms = (datalog_expression_root**) malloc(sizeof(datalog_expression_root*) * data.length);
	if (sentences == NULL || logical_forms == NULL) {
		if (sentences != NULL) free(sentences);
		if (logical_forms != NULL) free(logical_forms);
		cleanup(data);
		return false;
	}

	for (unsigned int i = 0; i < data.length; i++) {
		if (data[i]->root.type != DATALOG_PREDICATE
		 || data[i]->root.pred.args[0] == NULL || data[i]->root.pred.args[1] == NULL)
			return false;

		const datalog_expression& utterance = *data[i]->root.pred.args[0];
		const datalog_expression& logical_form = *data[i]->root.pred.args[1];
		if (utterance.type != DATALOG_PREDICATE || logical_form.type != DATALOG_PREDICATE
		 || utterance.pred.args[0] == NULL || logical_form.pred.args[0] == NULL
		 || utterance.pred.args[0]->type != DATALOG_STRING)
			return false;

		array<unsigned int>& tokens = *((array<unsigned int>*) alloca(sizeof(array<unsigned int>)));
		if (!array_init(tokens, 8)) {
			cleanup(data, sentences, logical_forms, i);
			return false;
		} else if (!tokenize(utterance.pred.args[0]->str.data, utterance.pred.args[0]->str.length, tokens, names)) {
			cleanup(data, sentences, logical_forms, i);
			free(tokens); return false;
		}
		sentences[i].tokens = tokens.data;
		sentences[i].length = tokens.length;

		if (!init(logical_forms[i], logical_form.pred.args[0])) {
			cleanup(data, sentences, logical_forms, i); return false;
		}

		if (!valid_variable_scope(logical_forms[i]->root)) {
			fprintf(stderr, "read_data ERROR: The logical form for sentence %u in '%s' has invalid scope.\n", i, data_filepath);
			cleanup(data, sentences, logical_forms, i + 1);
			return false;
		}
	}
	return true;
}

bool read_lexicon(
		array<datalog_expression_root*>& data, hash_map<string, unsigned int>& names,
		sequence*& sentences, datalog_expression_root**& logical_forms,
		const char* data_filepath, unsigned int*& nonterminals)
{
	if (!read_data(data, names, data_filepath)) return false;

	/* get the list of sentences and logical forms from the data */
	sentences = NULL; logical_forms = NULL;
	sentences = (sequence*) malloc(sizeof(sequence) * data.length);
	logical_forms = (datalog_expression_root**) malloc(sizeof(datalog_expression_root*) * data.length);
	nonterminals = (unsigned int*) malloc(sizeof(unsigned int) * data.length);
	if (sentences == NULL || logical_forms == NULL || nonterminals == NULL) {
		if (sentences != NULL) free(sentences);
		if (logical_forms != NULL) free(logical_forms);
		if (nonterminals != NULL) free(nonterminals);
		cleanup(data);
		return false;
	}

	for (unsigned int i = 0; i < data.length; i++) {
		if (data[i]->root.type != DATALOG_PREDICATE
		 || data[i]->root.pred.args[0] == NULL || data[i]->root.pred.args[1] == NULL) {
			cleanup(data, sentences, logical_forms, nonterminals, i); return false;
		}

		if (data[i]->root.pred.args[2] == NULL
		 || !to_sequence(data[i]->root.pred.args[1], sentences[i], names)) {
			fprintf(stderr, "read_data ERROR: Invalid number of arguments for entry %u in '%s'.\n", i, data_filepath);
			cleanup(data, sentences, logical_forms, nonterminals, i); return false;
		}
		nonterminals[i] = data[i]->root.pred.args[0]->constant.label;

		if (!init(logical_forms[i], data[i]->root.pred.args[2])) {
			cleanup(data, sentences, logical_forms, nonterminals, i); return false;
		}

		if (!valid_variable_scope(logical_forms[i]->root)) {
			fprintf(stderr, "read_lexicon ERROR: The logical form for sentence %u in '%s' has invalid scope.\n", i, data_filepath);
			cleanup(data, sentences, logical_forms, nonterminals, i + 1);
			return false;
		}
	}
	return true;
}

bool read_ontology(
		datalog_ontology& ontology,
		hash_map<string, unsigned int>& names,
		const char* ontology_filepath)
{
	/* first lex the ontology */
	array<ontology_token> tokens = array<ontology_token>(512);
	FILE* input = fopen(ontology_filepath, "r");
	if (input == NULL) {
		fprintf(stderr, "ERROR: Unable to open '%s' for reading.\n", ontology_filepath);
		return false;
	}
	ontology_lex(tokens, input);

	/* interpret the lexed data */
	if (!ontology_interpret(tokens, ontology, names)) {
		free_tokens(tokens);
		fprintf(stderr, "ERROR: Unable to parse '%s'.\n", ontology_filepath);
		return false;
	}
	free_tokens(tokens);
	return true;
}

bool read_beliefs(
		array<datalog_expression_root*>& data, hash_map<string, unsigned int>& names,
		datalog_expression_root**& logical_forms, const char* data_filepath)
{
	if (!read_data(data, names, data_filepath)) return false;

	/* get the list of sentences and logical forms from the data */
	logical_forms = NULL;
	logical_forms = (datalog_expression_root**) malloc(sizeof(datalog_expression_root*) * data.length);
	if (logical_forms == NULL) {
		if (logical_forms != NULL) free(logical_forms);
		cleanup(data);
		return false;
	}

	for (unsigned int i = 0; i < data.length; i++) {
		if (data[i]->root.type != DATALOG_PREDICATE || data[i]->root.pred.args[0] == NULL
		 || !init(logical_forms[i], data[i]->root.pred.args[0])) {
			cleanup(data, NULL, logical_forms, i); return false;
		}

		if (!valid_variable_scope(logical_forms[i]->root)) {
			fprintf(stderr, "read_data ERROR: The logical form for sentence %u in '%s' has invalid scope.\n", i, data_filepath);
			cleanup(data, NULL, logical_forms, i + 1);
			return false;
		}
	}
	return true;
}

#endif /* PARSER_DATA_H_ */
