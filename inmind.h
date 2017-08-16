/**
 * \file inmind.h
 *
 * <!-- Created on: Jul 24, 2017
 *          Author: asaparov -->
 */

#ifndef INMIND_H_
#define INMIND_H_

#include <core/lex.h>
#include <core/utility.h>
#include "datalog.h"

using namespace core;


/**
 * A simple lexer for S-expression-like InMind data.
 */

enum class inmind_token_type {
	LPAREN,
	RPAREN,
	IDENTIFIER,
	STRING,
	KEYWORD_CALL
};

typedef lexical_token<inmind_token_type> inmind_token;

template<typename Stream>
inline bool print(inmind_token_type type, Stream& stream) {
	switch (type) {
	case inmind_token_type::LPAREN:
		return print('(', stream);
	case inmind_token_type::RPAREN:
		return print(')', stream);
	case inmind_token_type::IDENTIFIER:
		return fprintf(stream, "IDENTIFIER") > 0;
	case inmind_token_type::STRING:
		return fprintf(stream, "STRING") > 0;
	case inmind_token_type::KEYWORD_CALL:
		return fprintf(stream, "call KEYWORD") > 0;
	}
	fprintf(stderr, "print ERROR: Unknown inmind_token_type.\n");
	return false;
}

enum class inmind_lexer_state {
	START,
	IDENTIFIER,
	STRING
};

bool inmind_emit_symbol(array<inmind_token>& tokens, const position& start, char symbol) {
	switch (symbol) {
	case '(':
		return emit_token(tokens, start, start + 1, inmind_token_type::LPAREN);
	case ')':
		return emit_token(tokens, start, start + 1, inmind_token_type::RPAREN);
	default:
		fprintf(stderr, "inmind_emit_symbol ERROR: Unexpected symbol.\n");
		return false;
	}
}

inline bool inmind_emit_identifier(
		array<inmind_token>& tokens, array<char>& token,
		const position& start, const position& current)
{
	if (compare_strings(token, "call"))
		return emit_token(tokens, start, current, inmind_token_type::KEYWORD_CALL);
	return emit_token(tokens, token, start, current, inmind_token_type::IDENTIFIER);
}

bool inmind_lex(array<inmind_token>& tokens, FILE* input) {
	position start = position(1, 1);
	position current = position(1, 1);
	inmind_lexer_state state = inmind_lexer_state::START;
	array<char> token = array<char>(1024);

	int next = fgetc(input);
	bool new_line = false;
	while (next != -1) {
		switch (state) {
		case inmind_lexer_state::STRING:
			if (next == '\\') {
				/* escape character */
				next = fgetc(input); current.column++;
				if (next == -1) {
					read_error("Unexpected end of stream", current);
					return false;
				}
				if (!token.add(next)) return false;
			} else if (next == '"') {
				if (!emit_token(tokens, token, start, current, inmind_token_type::STRING)) return false;
				state = inmind_lexer_state::START;
				token.clear();
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case inmind_lexer_state::IDENTIFIER:
			if (next == '(' || next == ')') {
				if (!inmind_emit_identifier(tokens, token, start, current)
				 || !inmind_emit_symbol(tokens, current, next))
					return false;
				state = inmind_lexer_state::START;
				token.clear();
			} else if (next == '\'') {
				read_error("Unexpected quote after identifier", current);
				return false;
			} else if (next == '\\') {
				read_error("Unexpected backslash after identifier", current);
				return false;
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				if (!inmind_emit_identifier(tokens, token, start, current))
					return false;
				state = inmind_lexer_state::START;
				token.clear();
				new_line = (next == '\n');
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case inmind_lexer_state::START:
			if (next == '(' || next == ')') {
				if (!inmind_emit_symbol(tokens, current, next))
					return false;
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				new_line = (next == '\n');
			} else if (next == '"') {
				state = inmind_lexer_state::STRING;
			} else {
				if (!token.add(next)) return false;
				state = inmind_lexer_state::IDENTIFIER;
				start = current;
			}
			break;
		}

		if (new_line) {
			current.line++;
			current.column = 1;
			new_line = false;
		} else current.column++;
		next = fgetc(input);
	}

	if (state == inmind_lexer_state::STRING) {
		read_error("Expected closing quote", current);
		return false;
	} else if (state == inmind_lexer_state::IDENTIFIER) {
		return inmind_emit_identifier(tokens, token, start, current);
	}
	return true;
}


/**
 * A recursive descent parser for S-expression-like InMind data.
 */

inline bool tokenize(
		const string& sentence, sequence& output,
		hash_map<string, unsigned int>& names)
{
	bool whitespace = true;
	unsigned int token_start = 0;
	array<unsigned int> tokens = array<unsigned int>(16);
	for (unsigned int i = 0; i < sentence.length; i++) {
		if (whitespace) {
			if (!isspace(sentence[i])) {
				token_start = i;
				whitespace = false;
			}
		} else {
			if (sentence[i] == '\'' || sentence[i] == '?' || isspace(sentence[i])) {
				unsigned int id;
				if (!get_token(string(sentence.data + token_start, i - token_start), id, names)
				 || !tokens.add(id))
					return false;
				if (isspace(sentence[i])) {
					whitespace = true;
				} else {
					if (!get_token(string(sentence.data + i, 1), id, names)
					 || !tokens.add(id))
						return false;
					whitespace = true;
				}
			}
		}
	}

	if (!whitespace) {
		unsigned int id;
		if (!get_token(string(sentence.data + token_start, sentence.length - token_start), id, names)
		 || !tokens.add(id))
			return false;
	}

	if (!init(output, 1))
		return false;
	swap(output.tokens, tokens.data);
	output.length = tokens.length;
	return true;
}

bool inmind_interpret_expression(
	const array<inmind_token>& tokens,
	unsigned int& index,
	datalog_expression& exp,
	hash_map<string, unsigned int>& names)
{
	if (index >= tokens.length) {
		read_error("Expected a string or open parenthesis", tokens.last().end);
		return false;
	} else if (tokens[index].type == inmind_token_type::STRING) {
		if (!tokenize(tokens[index].text, exp.str, names)) return false;
		exp.type = DATALOG_STRING;
		exp.reference_count = 1;
		index++; return true;
	} else if (tokens[index].type != inmind_token_type::LPAREN) {
		read_error("Expected a string or open parenthesis", tokens[index].start);
		return false;
	}
	index++;

	if (index >= tokens.length) {
		read_error("Expected an identifier after an open parenthesis", tokens.last().end);
		return false;
	} else if (tokens[index].type == inmind_token_type::KEYWORD_CALL) {
		index++;
	}

	if (!expect_token(tokens, index, inmind_token_type::IDENTIFIER, "identifier")
	 || !get_token(tokens[index].text, exp.pred.function, names))
		return false;
	index++;

	for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
		exp.pred.args[i] = NULL;
	exp.pred.excluded_count = 0;
	exp.type = DATALOG_PREDICATE;
	exp.reference_count = 1;

	/* parse the arguments */
	unsigned int arg_index = 0;
	while (true) {
		if (index >= tokens.length) {
			read_error("Unexpected end of input", tokens.last().end);
			return false;
		} else if (tokens[index].type == inmind_token_type::RPAREN) {
			index++;
			return true;
		} else if (arg_index == array_length(exp.pred.args)) {
			read_error("Too many arguments", tokens[index].start);
			return false;
		} else if (!new_expression(exp.pred.args[arg_index])) {
			return false;
		} else if (!inmind_interpret_expression(tokens, index, *exp.pred.args[arg_index], names)) {
			free(exp.pred.args[arg_index]);
			exp.pred.args[arg_index] = NULL;
		}
		arg_index++;
	}
}

bool inmind_interpret(
	const array<inmind_token>& tokens,
	array<datalog_expression_root*>& expressions,
	hash_map<string, unsigned int>& names)
{
	unsigned int index = 0;
	while (index < tokens.length) {
		if (!expressions.ensure_capacity(expressions.length + 1))
			return false;

		datalog_expression_root*& next_expression = expressions[(unsigned int) expressions.length];
		next_expression = (datalog_expression_root*) malloc(sizeof(datalog_expression_root));
		if (next_expression == NULL) {
			fprintf(stderr, "inmind_interpret ERROR: Out of memory.\n");
			return false;
		}
		next_expression->index = NUMBER_ALL;
		next_expression->concord = NUMBER_NONE;
		next_expression->inf = INFLECTION_NONE;
		if (!inmind_interpret_expression(tokens, index, next_expression->root, names))
			return false;
		expressions.length++;
	}
	return true;
}

#endif /* INMIND_H_ */
