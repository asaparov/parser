/**
 * datalog_to_lambda.cpp
 *
 *  Created on: Jun 24, 2016
 *      Author: asaparov
 */

#include "datalog_to_lambda.h"

template<typename Stream>
inline bool print_special_string(unsigned int item, Stream& out) {
	if (item == DATALOG_LABEL_EMPTY)
		return print("[null]", out);
	else if (item == DATALOG_LABEL_WILDCARD)
		return print('*', out);
	else return print("<new token>", out);
}

void print_sentence(const array<unsigned int>& sentence, const string* const* names, FILE* out) {
	if (sentence.length == 0) return;
	print(*names[sentence[0]], out);
	for (unsigned int i = 1; i < sentence.length; i++) {
		print(" ", out);
		print(*names[sentence[i]], out);
	}
}

int main(int argc, const char** argv) {
	if (argc < 3) {
		fprintf(stderr, "usage:  datalog_to_lambda <Datalog source filepath> <output filepath>\n");
		return EXIT_FAILURE;
	}

	array<datalog_token> tokens = array<datalog_token>(1024);
	FILE* input = fopen(argv[1], "r");
	if (input == NULL) {
		fprintf(stderr, "ERROR: Unable to open '%s' for reading.\n", argv[1]);
		return EXIT_FAILURE;
	}
	datalog_lex<true>(tokens, input);

	array<datalog_expression_root*> statements(1024);
	hash_map<string, unsigned int> names(1024);
	if (!populate_name_map(names)
	 || !datalog_interpret(tokens, statements, names)) {
		free_tokens(tokens);
		for (datalog_expression_root* e : statements) {
			free(*e); if (e->root.reference_count == 0) free(e);
		}
		fprintf(stderr, "ERROR: Unable to parse Datalog input.\n");
		return EXIT_FAILURE;
	}
	free_tokens(tokens);

	array<example> data = array<example>((unsigned int) statements.length);
	for (unsigned int i = 0; i < statements.length; i++) {
		if (!to_lambda_example(statements[i]->root, data[i], names)) {
			fprintf(stderr, "ERROR: Unable to convert example %u to lambda expression", i);
			if (data[i].sentence.length > 0) {
				fprintf(stderr, ":\n\t\"");
				const string** inverse_names = invert(names);
				print_sentence(data[i].sentence, inverse_names, stderr);
				fprintf(stderr, "\"\n"); free(inverse_names);
			} else {
				fprintf(stderr, ".\n");
			}
			break;
		}
		data.length++;
		free(*statements[i]);
		if (statements[i]->root.reference_count == 0)
			free(statements[i]);

		/* compute variable scope in this lambda expression */
		context ctx;
		if (!compute_scope(data[i].logical_form, ctx)
		 || !declare_variables(data[i].logical_form, ctx)) {
			fprintf(stderr, "ERROR: Unable to compute scope and add existential quantifiers.\n");
			break;
		}
	}

	FILE* out = fopen(argv[2], "w");
	const string** inverse_names = invert(names);
	if (out == NULL)
		fprintf(stderr, "ERROR: Unable to open '%s' for writing.\n", argv[2]);
	for (unsigned int i = 0; out != NULL && i < data.length; i++) {
		print_sentence(data[i].sentence, inverse_names, out);
		print('\t', out);
		lambda_printer printer = lambda_printer(inverse_names);
		print(data[i].logical_form, out, printer);
		print('\n', out);
	}
	if (out != NULL) {
		fflush(out); fclose(out);
	}

	for (auto item : data)
		free(item);
	for (auto entry : names)
		free(entry.key);
	free(inverse_names);
	return EXIT_SUCCESS;
}
