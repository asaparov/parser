/**
 * datalog.h
 *
 *  Created on: Jul 1, 2016
 *	  Author: asaparov
 */

#ifndef DATALOG_H_
#define DATALOG_H_

#include <core/lex.h>
#include <grammar/morphology.h>
#include <limits.h>
#include <type_traits>
#include <atomic>
#include <cctype>

using namespace core;

constexpr unsigned int DATALOG_LABEL_WILDCARD = UINT_MAX;
constexpr unsigned int DATALOG_LABEL_EMPTY = UINT_MAX - 1;
constexpr unsigned int DATALOG_LABEL_NOT = UINT_MAX - 2;

constexpr unsigned int PREDICATE_ANSWER = 1;
constexpr unsigned int PREDICATE_COUNT = 2;
constexpr unsigned int PREDICATE_SUM = 3;
constexpr unsigned int PREDICATE_HIGHEST = 4;
constexpr unsigned int PREDICATE_LOWEST = 5;
constexpr unsigned int PREDICATE_HEIGHT = 6;
constexpr unsigned int PREDICATE_LONGEST = 7;
constexpr unsigned int PREDICATE_SHORTEST = 8;
constexpr unsigned int PREDICATE_LENGTH = 9;
constexpr unsigned int PREDICATE_LARGEST = 10;
constexpr unsigned int PREDICATE_SMALLEST = 11;
constexpr unsigned int PREDICATE_MOST = 12;
constexpr unsigned int PREDICATE_FEWEST = 13;
constexpr unsigned int PREDICATE_NOT = 14;
constexpr unsigned int NUM_PREDICATES = 14;

constexpr unsigned int PREDICATE_PARSE = 15;

constexpr unsigned int DIRECTION_FORWARD = 16;
constexpr unsigned int DIRECTION_BACKWARD = 17;
constexpr unsigned int DIRECTION_BOTH = 18;
constexpr unsigned int DIRECTION_SELF = 19;
constexpr unsigned int DIRECTION_NONE = 20;
constexpr unsigned int DIRECTION_PREDICATE = 21;
constexpr unsigned int DIRECTION_CONSTANT = 22;

constexpr unsigned int ARITY_ZERO = 23;
constexpr unsigned int ARITY_ONE = 24;
constexpr unsigned int ARITY_TWO = 25;
constexpr unsigned int ARITY_THREE = 26;
constexpr unsigned int ARITY_NULL = 27;

constexpr unsigned int ARG_OTHER = 28;

constexpr unsigned int DATALOG_TRUE = 29;

constexpr unsigned int PREDICATE_LOC = 30;
constexpr unsigned int PREDICATE_CONST = 31;
constexpr unsigned int PREDICATE_ATTR = 32;
constexpr unsigned int PREDICATE_OBJECT = 33;

constexpr unsigned int NUMBER_OFFSET = 34;
constexpr unsigned int INFLECTION_OFFSET = 34 + grammatical_number_count - 1;

enum datalog_expression_type {
	DATALOG_PREDICATE	= INFLECTION_OFFSET + 1,
	DATALOG_FUNCTION	= INFLECTION_OFFSET + 2,
	DATALOG_TUPLE		= INFLECTION_OFFSET + 3,
	DATALOG_LIST		= INFLECTION_OFFSET + 4,
	DATALOG_VARIABLE	= INFLECTION_OFFSET + 5,
	DATALOG_CONSTANT	= INFLECTION_OFFSET + 6,
	DATALOG_INTEGER		= INFLECTION_OFFSET + 7,
	DATALOG_STRING		= INFLECTION_OFFSET + 8,
	DATALOG_EMPTY		= INFLECTION_OFFSET + 9,
	DATALOG_ANY			= INFLECTION_OFFSET + 10,
	DATALOG_NON_EMPTY	= INFLECTION_OFFSET + 11
};

struct datalog_token_scribe { };

template<typename Stream>
inline bool print(const string& str, Stream& out, const datalog_token_scribe& printer) {
	for (unsigned int i = 0; i < str.length; i++) {
		if (std::isspace(str[i]) || str[i] == '.') {
			return print('\'', out) && print(str, out) && print('\'', out);
		}
	}
	return print(str, out);
}

/**
 * Code for tokenizing/lexing Datalog data.
 */

bool populate_name_map(hash_map<string, unsigned int>& names) {
	bool success = true;
	success &= names.put("parse", PREDICATE_PARSE);
	success &= names.put("answer", PREDICATE_ANSWER);
	success &= names.put("count", PREDICATE_COUNT);
	success &= names.put("sum", PREDICATE_SUM);
	success &= names.put("highest", PREDICATE_HIGHEST);
	success &= names.put("lowest", PREDICATE_LOWEST);
	success &= names.put("height", PREDICATE_HEIGHT);
	success &= names.put("longest", PREDICATE_LONGEST);
	success &= names.put("shortest", PREDICATE_SHORTEST);
	success &= names.put("length", PREDICATE_LENGTH);
	success &= names.put("largest", PREDICATE_LARGEST);
	success &= names.put("smallest", PREDICATE_SMALLEST);
	success &= names.put("most", PREDICATE_MOST);
	success &= names.put("fewest", PREDICATE_FEWEST);
	success &= names.put("not", PREDICATE_NOT);
	success &= names.put("direction_forward", DIRECTION_FORWARD);
	success &= names.put("direction_backward", DIRECTION_BACKWARD);
	success &= names.put("direction_both", DIRECTION_BOTH);
	success &= names.put("direction_self", DIRECTION_SELF);
	success &= names.put("direction_none", DIRECTION_NONE);
	success &= names.put("direction_predicate", DIRECTION_PREDICATE);
	success &= names.put("direction_constant", DIRECTION_CONSTANT);
	success &= names.put("arity_zero", ARITY_ZERO);
	success &= names.put("arity_unary", ARITY_ONE);
	success &= names.put("arity_binary", ARITY_TWO);
	success &= names.put("arity_three", ARITY_THREE);
	success &= names.put("arity_null", ARITY_NULL);
	success &= names.put("arg_other", ARG_OTHER);
	success &= names.put("true", DATALOG_TRUE);
	success &= names.put("loc", PREDICATE_LOC);
	success &= names.put("const", PREDICATE_CONST);
	success &= names.put("attr", PREDICATE_ATTR);
	success &= names.put("object", PREDICATE_OBJECT);
	success &= names.put("sg",				NUMBER_OFFSET + NUMBER_SINGULAR - 1);
	success &= names.put("pl", 				NUMBER_OFFSET + NUMBER_PLURAL - 1);
	success &= names.put("uncountable",		NUMBER_OFFSET + NUMBER_UNCOUNTABLE - 1);
	success &= names.put("non_sg",			NUMBER_OFFSET + NUMBER_NON_SINGULAR - 1);
	success &= names.put("non_pl",			NUMBER_OFFSET + NUMBER_NON_PLURAL - 1);
	success &= names.put("all_numbers",		NUMBER_OFFSET + NUMBER_ALL - 1);
	success &= names.put("no_number",		NUMBER_OFFSET + NUMBER_NONE - 1);
	success &= names.put("past_participle", INFLECTION_OFFSET + INFLECTION_PAST_PARTICIPLE - 1);
	success &= names.put("present_participle", INFLECTION_OFFSET + INFLECTION_PRESENT_PARTICIPLE - 1);
	success &= names.put("infinitive",		INFLECTION_OFFSET + INFLECTION_INFINITIVE - 1);
	success &= names.put("default_tense",	INFLECTION_OFFSET + INFLECTION_OTHER_VERB - 1);
	success &= names.put("noun",			INFLECTION_OFFSET + INFLECTION_NOUN - 1);
	success &= names.put("adjective",		INFLECTION_OFFSET + INFLECTION_ADJECTIVE - 1);
	success &= names.put("comparative",		INFLECTION_OFFSET + INFLECTION_COMPARATIVE - 1);
	success &= names.put("superlative",		INFLECTION_OFFSET + INFLECTION_SUPERLATIVE - 1);
	success &= names.put("no_inflection",	INFLECTION_OFFSET + INFLECTION_NONE - 1);
	success &= names.put("type_predicate",	DATALOG_PREDICATE);
	success &= names.put("type_function",	DATALOG_FUNCTION);
	success &= names.put("type_tuple",		DATALOG_TUPLE);
	success &= names.put("type_list",		DATALOG_LIST);
	success &= names.put("type_variable",	DATALOG_VARIABLE);
	success &= names.put("type_constant",	DATALOG_CONSTANT);
	success &= names.put("type_integer",	DATALOG_INTEGER);
	success &= names.put("type_string", 	DATALOG_STRING);
	success &= names.put("type_empty",		DATALOG_EMPTY);
	success &= names.put("type_any",		DATALOG_ANY);
	success &= names.put("type_non_empty",	DATALOG_NON_EMPTY);
	return success;
}

enum datalog_token_type {
	DATALOG_TOKEN_LBRACKET,
	DATALOG_TOKEN_RBRACKET,
	DATALOG_TOKEN_LPAREN,
	DATALOG_TOKEN_RPAREN,
	DATALOG_TOKEN_COMMA,
	DATALOG_TOKEN_PERIOD,
	DATALOG_TOKEN_IDENTIFIER,
	DATALOG_TOKEN_STRING,
	DATALOG_TOKEN_SLASH_PLUS,
	DATALOG_TOKEN_TAB
};

typedef lexical_token<datalog_token_type> datalog_token;

template<typename Stream>
inline bool print(datalog_token_type type, Stream& stream) {
	switch (type) {
	case DATALOG_TOKEN_LBRACKET:
		return print('[', stream);
	case DATALOG_TOKEN_RBRACKET:
		return print(']', stream);
	case DATALOG_TOKEN_LPAREN:
		return print('(', stream);
	case DATALOG_TOKEN_RPAREN:
		return print(')', stream);
	case DATALOG_TOKEN_COMMA:
		return fprintf(stream, "COMMA") > 0;
	case DATALOG_TOKEN_PERIOD:
		return print('.', stream);
	case DATALOG_TOKEN_IDENTIFIER:
		return fprintf(stream, "IDENTIFIER") > 0;
	case DATALOG_TOKEN_STRING:
		return fprintf(stream, "STRING") > 0;
	case DATALOG_TOKEN_SLASH_PLUS:
		return fprintf(stream, "\\+") > 0;
	case DATALOG_TOKEN_TAB:
		return print('\t', stream);
	}
	fprintf(stderr, "print ERROR: Unknown datalog_token_type.\n");
	return false;
}

enum datalog_lexer_state {
	DATALOG_LEXER_START,
	DATALOG_LEXER_IDENTIFIER,
	DATALOG_LEXER_QUOTE,
	DATALOG_LEXER_COMMENT
};

bool datalog_emit_symbol(array<datalog_token>& tokens, const position& start, char symbol) {
	switch (symbol) {
	case ',':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_COMMA);
	case '.':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_PERIOD);
	case '(':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_LPAREN);
	case ')':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_RPAREN);
	case '[':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_LBRACKET);
	case ']':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_RBRACKET);
	case '\t':
		return emit_token(tokens, start, start + 1, DATALOG_TOKEN_TAB);
	default:
		fprintf(stderr, "datalog_emit_symbol ERROR: Unexpected symbol.\n");
		return false;
	}
}

template<bool TokenizeTabs = true>
bool datalog_lex(array<datalog_token>& tokens, FILE* input) {
	position start = position(1, 1);
	position current = position(1, 1);
	datalog_lexer_state state = DATALOG_LEXER_START;
	array<char> token = array<char>(1024);

	int prev = 0, next = fgetc(input);
	bool new_line = false;
	bool found_tab = TokenizeTabs;
	while (next != -1) {
		switch (state) {
		case DATALOG_LEXER_QUOTE:
			if (next == '\\') {
				/* escape character */
				next = fgetc(input); current.column++;
				if (next == -1) {
					read_error("Unexpected end of stream", current);
					return false;
				}
				if (!token.add(next)) return false;
			} else if (next == '\'') {
				if (!emit_token(tokens, token, start, current, DATALOG_TOKEN_STRING)) return false;
				state = DATALOG_LEXER_START;
				token.clear();
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case DATALOG_LEXER_IDENTIFIER:
			if (next == ',' || next == '.' || next == '(' || next == ')'
			 || next == '[' || next == ']' || (TokenizeTabs && next == '\t'))
			{
				if (TokenizeTabs && next == '\t')
					found_tab = true;
				if (!emit_token(tokens, token, start, current, DATALOG_TOKEN_IDENTIFIER)
				 || !datalog_emit_symbol(tokens, current, next))
					return false;
				state = DATALOG_LEXER_START;
				token.clear();
			} else if (next == '\'') {
				read_error("Unexpected quote after identifier", current);
				return false;
			} else if (next == '\\') {
				read_error("Unexpected backslash after identifier", current);
				return false;
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				if (!emit_token(tokens, token, start, current, DATALOG_TOKEN_IDENTIFIER))
					return false;
				state = DATALOG_LEXER_START;
				token.clear();
				new_line = (next == '\n');
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case DATALOG_LEXER_START:
			if (next == '\'' && found_tab) {
				state = DATALOG_LEXER_QUOTE;
				start = current;
			} else if (next == ',' || next == '.' || next == '(' || next == ')'
					|| next == '[' || next == ']' || (TokenizeTabs && next == '\t'))
			{
				if (!datalog_emit_symbol(tokens, current, next))
					return false;
			} else if (next == '\\') {
				next = fgetc(input);
				if (next != '+') {
					read_error("Expected '+'", current);
					return false;
				} if (!emit_token(tokens, current, current + 1, DATALOG_TOKEN_SLASH_PLUS))
					return false;
				current.column++;
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				new_line = (next == '\n');
			} else if (next == '/') {
				next = fgetc(input);
				if (next != '*') {
					read_error("Expected '*'", current);
					return false;
				}
				state = DATALOG_LEXER_COMMENT;
			} else {
				if (!token.add(next)) return false;
				state = DATALOG_LEXER_IDENTIFIER;
				start = current;
			}
			break;

		case DATALOG_LEXER_COMMENT:
			if (prev == '*' && next == '/') {
				state = DATALOG_LEXER_START;
			} else if (next == '\n') {
				new_line = true;
			}
			break;
		}

		if (new_line) {
			current.line++;
			current.column = 1;
			found_tab = TokenizeTabs;
			new_line = false;
		} else current.column++;
		prev = next;
		next = fgetc(input);
	}

	if (state == DATALOG_LEXER_QUOTE) {
		read_error("Expected closing quote", current);
		return false;
	} else if (state == DATALOG_LEXER_IDENTIFIER) {
		return emit_token(tokens, token, start, current, DATALOG_TOKEN_IDENTIFIER);
	}
	return true;
}

/**
 * Recursive-descent parser for tokenized Datalog data.
 */

struct datalog_expression;
bool datalog_interpret_expression(
	const array<datalog_token>& tokens,
	unsigned int& index,
	datalog_expression& exp,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables);

inline bool exclude(unsigned int*& excluded, unsigned int& excluded_count,
		const unsigned int* to_exclude, unsigned int to_exclude_count)
{
	unsigned int* excluded_union = (unsigned int*) malloc(
		sizeof(unsigned int) * (excluded_count + to_exclude_count));
	if (excluded_union == NULL) {
		fprintf(stderr, "exclude ERROR: Out of memory.\n");
		return false;
	}

	unsigned int excluded_union_count = 0;
	set_union(excluded_union, excluded_union_count, excluded, excluded_count, to_exclude, to_exclude_count);
	if (excluded_union_count == 0) {
		core::free(excluded_union);
		return true;
	} else if (excluded_count > 0)
		core::free(excluded);
	excluded = excluded_union;
	excluded_count = excluded_union_count;
	return true;
}

struct datalog_predicate {
	static constexpr unsigned int ARG_COUNT = 3;

	unsigned int function;
	unsigned int* excluded;
	unsigned int excluded_count;
	datalog_expression* args[ARG_COUNT];

	inline bool is_excluded(unsigned int value) const {
		return index_of(value, excluded, excluded_count) < excluded_count;
	}

	inline bool ensure_excluded_capacity(unsigned int capacity) {
		if (excluded_count == 0) excluded = NULL;
		if (!resize(excluded, capacity)) {
			fprintf(stderr, "datalog_predicate.ensure_excluded_capacity ERROR: Unable to expand excluded array.\n");
			return false;
		}
		return true;
	}

	bool exclude(const unsigned int* items, unsigned int count) {
		return ::exclude(excluded, excluded_count, items, count);
	}

	static inline void free(datalog_predicate& pred);
};

struct datalog_function {
	unsigned int function;
	unsigned int* excluded;
	unsigned int excluded_count;
	unsigned int vars[2];
	datalog_expression* arg;

	inline bool is_excluded(unsigned int value) const {
		return index_of(value, excluded, excluded_count) < excluded_count;
	}

	inline bool ensure_excluded_capacity(unsigned int capacity) {
		excluded = NULL;
		if (!resize(excluded, capacity)) {
			fprintf(stderr, "datalog_function.ensure_excluded_capacity ERROR: Unable to expand excluded array.\n");
			return false;
		}
		return true;
	}

	bool exclude(const unsigned int* items, unsigned int count) {
		return ::exclude(excluded, excluded_count, items, count);
	}

	static inline void free(datalog_function& func);
};

enum tuple_position {
	POSITION_LEFT,
	POSITION_RIGHT,
	POSITION_EXACT
};

struct datalog_tuple {
	array<datalog_expression*> elements;
	tuple_position position;

	static inline void free(datalog_tuple& tuple);
};

struct datalog_list {
	array<datalog_expression*> elements;

	static inline void free(datalog_list& list);
};

struct datalog_literal {
	unsigned int label;
	unsigned int* excluded;
	unsigned int excluded_count;

	inline bool is_excluded(unsigned int value) const {
		return index_of(value, excluded, excluded_count) < excluded_count;
	}

	inline bool ensure_excluded_capacity(unsigned int capacity) {
		excluded = NULL;
		if (!resize(excluded, capacity)) {
			fprintf(stderr, "datalog_function.ensure_excluded_capacity ERROR: Unable to expand excluded array.\n");
			return false;
		}
		return true;
	}

	bool exclude(const unsigned int* items, unsigned int count) {
		return ::exclude(excluded, excluded_count, items, count);
	}

	static inline void move(const datalog_literal& src, datalog_literal& dst) {
		dst.label = src.label;
		dst.excluded = src.excluded;
		dst.excluded_count = src.excluded_count;
	}

	static inline unsigned int hash(const datalog_literal& key) {
		unsigned int hash_value = default_hash(key.label);
		if (key.excluded_count > 0)
			hash_value ^= default_hash(key.excluded, key.excluded_count);
		return hash_value;
	}

	static inline bool is_empty(const datalog_literal& key) {
		return key.label == 0;
	}

	static inline void free(datalog_literal& literal);
};

inline bool operator < (const datalog_literal& first, const datalog_literal& second) {
	if (first.label < second.label) return true;
	else if (first.label > second.label) return false;
	else if (first.excluded_count < second.excluded_count) return true;
	else if (first.excluded_count > second.excluded_count) return false;

	for (unsigned int i = 0; i < first.excluded_count; i++) {
		if (first.excluded[i] < second.excluded[i]) return true;
		else if (first.excluded[i] > second.excluded[i]) return false;
	}

	/* the literals are identical */
	return false;
}

struct datalog_expression {
	datalog_expression_type type;
	unsigned int reference_count;
	union {
		datalog_predicate pred;
		datalog_function func;
		datalog_tuple tuple;
		datalog_list list;
		datalog_literal constant;
		unsigned int variable;
		int integer;
		sequence str;
	};

	datalog_expression() : type(DATALOG_ANY), reference_count(1) { }

	datalog_expression(datalog_expression_type type) : type(type), reference_count(1) { }

	datalog_expression(const datalog_expression& src) : reference_count(1) {
		if (!initialize(src))
			exit(EXIT_FAILURE);
	}

	~datalog_expression() { free(); }

	inline void operator = (const datalog_expression& src) {
		reference_count = 1;
		if (!initialize(src))
			exit(EXIT_FAILURE);
	}

	inline void recompute_hash() const { }

	static inline void move(const datalog_expression& src, datalog_expression& dst) {
		dst.type = src.type;
		dst.reference_count = src.reference_count;
		switch (src.type) {
		case DATALOG_PREDICATE:
			dst.pred.function = src.pred.function;
			dst.pred.excluded = src.pred.excluded;
			dst.pred.excluded_count = src.pred.excluded_count;
			for (unsigned int i = 0; i < array_length(src.pred.args); i++)
				dst.pred.args[i] = src.pred.args[i];
			return;
		case DATALOG_FUNCTION:
			dst.func.function = src.func.function;
			dst.func.excluded = src.func.excluded;
			dst.func.excluded_count = src.func.excluded_count;
			dst.func.arg = src.func.arg;
			for (unsigned int i = 0; i < array_length(src.func.vars); i++)
				dst.func.vars[i] = src.func.vars[i];
			return;
		case DATALOG_TUPLE:
			core::move(src.tuple.elements, dst.tuple.elements);
			dst.tuple.position = src.tuple.position;
			return;
		case DATALOG_LIST:
			core::move(src.list.elements, dst.list.elements); return;
		case DATALOG_VARIABLE:
			dst.variable = src.variable; return;
		case DATALOG_CONSTANT:
			dst.constant.label = src.constant.label;
			dst.constant.excluded = src.constant.excluded;
			dst.constant.excluded_count = src.constant.excluded_count;
			return;
		case DATALOG_INTEGER:
			dst.integer = src.integer;
			return;
		case DATALOG_STRING:
			sequence::move(src.str, dst.str);
			return;
		case DATALOG_EMPTY:
		case DATALOG_ANY:
		case DATALOG_NON_EMPTY:
			return;
		}
		fprintf(stderr, "datalog_expression.move ERROR: Unrecognized expression type.\n");
		exit(EXIT_FAILURE);
	}

	static inline void swap(datalog_expression& first, datalog_expression& second) {
		char* first_data = (char*) &first;
		char* second_data = (char*) &second;
		for (unsigned int i = 0; i < sizeof(datalog_expression); i++)
			core::swap(first_data[i], second_data[i]);
	}

	static inline bool is_empty(const datalog_expression& key) {
		return key.reference_count == 0;
	}

	static inline unsigned int hash(const datalog_expression& key) {
		unsigned int hash_value;
		switch (key.type) {
		case DATALOG_PREDICATE:
			hash_value = default_hash(key.pred.function);
			if (key.pred.excluded_count > 0)
				hash_value ^= default_hash(key.pred.excluded, key.pred.excluded_count);
			for (unsigned int i = 0; i < array_length(key.pred.args); i++) {
				if (key.pred.args[i] == NULL) continue;
				hash_value ^= hash(*key.pred.args[i]);
			}
			return 0 + 11 * hash_value;
		case DATALOG_FUNCTION:
			hash_value = default_hash(key.func.function);
			if (key.func.excluded_count > 0)
				hash_value ^= default_hash(key.func.excluded, key.func.excluded_count);
			if (key.func.arg != NULL)
				hash_value ^= hash(*key.func.arg);
			for (unsigned int i = 0; i < array_length(key.func.vars); i++) {
				if (key.func.vars[i] == 0) break;
				hash_value ^= default_hash(key.func.vars[i]);
			}
			return 1 + 11 * hash_value;
		case DATALOG_TUPLE:
			hash_value = default_hash(key.tuple.position);
			for (unsigned int i = 0; i < key.tuple.elements.length; i++)
				hash_value ^= hash(*key.tuple.elements[i]);
			return 2 + 11 * hash_value;
		case DATALOG_LIST:
			hash_value = 0;
			for (unsigned int i = 0; i < key.list.elements.length; i++)
				hash_value ^= hash(*key.list.elements[i]);
			return 3 + 11 * hash_value;
		case DATALOG_VARIABLE:
			return 4 + 11 * default_hash(key.variable);
		case DATALOG_CONSTANT:
			return 5 + 11 * datalog_literal::hash(key.constant);
		case DATALOG_INTEGER:
			return 6 + 11 * default_hash(key.integer);
		case DATALOG_STRING:
			return 7 + 11 * sequence::hash(key.str);
		case DATALOG_EMPTY:
			return 8;
		case DATALOG_ANY:
			return 9;
		case DATALOG_NON_EMPTY:
			return 10;
		}
		fprintf(stderr, "datalog_expression.hash ERROR: Unrecognized expression type.\n");
		exit(EXIT_FAILURE);
	}

	static inline void free(datalog_expression& exp) { exp.free(); }

private:
	template<typename... Args>
	inline bool initialize(const datalog_expression& src, Args&&... args);

	inline void free() {
		reference_count--;
		if (reference_count == 0) {
			switch (type) {
			case DATALOG_PREDICATE:
				core::free(pred);
				break;
			case DATALOG_FUNCTION:
				core::free(func);
				break;
			case DATALOG_TUPLE:
				core::free(tuple);
				break;
			case DATALOG_LIST:
				core::free(list);
				break;
			case DATALOG_VARIABLE:
				core::free(variable);
				break;
			case DATALOG_CONSTANT:
				core::free(constant);
				break;
			case DATALOG_INTEGER:
				core::free(integer);
				break;
			case DATALOG_STRING:
				core::free(str);
				break;
			case DATALOG_EMPTY:
			case DATALOG_ANY:
			case DATALOG_NON_EMPTY:
				break;
			}
		}
	}

	template<typename... Args>
	friend bool init(datalog_expression&, const datalog_expression&, Args&&...);
};

struct datalog_expression_root {
	/* semantic information */
	datalog_expression root;

	/* syntactic information */
	grammatical_number index;
	grammatical_number concord;
	inflection inf;

	datalog_expression_root() : index(NUMBER_ANY), concord(NUMBER_ANY), inf(INFLECTION_ANY) { }
	datalog_expression_root(const datalog_expression_root& src) :
		root(src.root), index(src.index), concord(src.concord), inf(src.inf) { }

	inline void operator = (const datalog_expression_root& src) {
		root = src.root;
		index = src.index;
		concord = src.concord;
		inf = src.inf;
	}

	inline void recompute_hash() const {
		root.recompute_hash();
	}

	static inline void move(const datalog_expression_root& src, datalog_expression_root& dst) {
		datalog_expression::move(src.root, dst.root);
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = src.inf;
	}

	static inline void swap(datalog_expression_root& first, datalog_expression_root& second) {
		datalog_expression::swap(first.root, second.root);
		core::swap(first.index, second.index);
		core::swap(first.concord, second.concord);
		core::swap(first.inf, second.inf);
	}

	static inline bool is_empty(const datalog_expression_root& key) {
		return datalog_expression::is_empty(key.root);
	}

	static inline unsigned int hash(const datalog_expression_root& key) {
		return datalog_expression::hash(key.root) ^ default_hash(key.index) ^ default_hash(key.concord) ^ default_hash(key.inf);
	}

	static inline void free(datalog_expression_root& exp) {
		datalog_expression::free(exp.root);
	}

	enum feature {
		FEATURE_NULL					=  0,
		FEATURE_FUNCTION				=  1,
		FEATURE_FUNCTION_ONLY			=  2,
		FEATURE_FUNCTION_ANSWER			=  3,
		FEATURE_HAS_FUNCTION			=  4,
		FEATURE_HAS_FUNCTION_NOT		=  5,
		FEATURE_HAS_FUNCTION_COUNT_NOT	=  6,
		FEATURE_HAS_FUNCTION_ANSWER		=  7,
		FEATURE_PREDICATE				=  8,
		FEATURE_PREDICATE_ONLY			=  9,
		FEATURE_FIRST_PREDICATE			= 10,
		FEATURE_SECOND_PREDICATE		= 11,
		FEATURE_THIRD_PREDICATE			= 12,
		FEATURE_LAST_PREDICATE			= 13,
		FEATURE_DIRECTION				= 14,
		FEATURE_DIRECTION_ROOT			= 15,
		FEATURE_CONSTANT				= 16,
		FEATURE_PREDICATE_ARITY			= 17,
		FEATURE_ARG1					= 18,
		FEATURE_ARG2					= 19,
		FEATURE_ARG3					= 20,
		FEATURE_ARG1_ONLY				= 21,
		FEATURE_ARG2_ONLY				= 22,
		FEATURE_ARG3_ONLY				= 23,
		FEATURE_ARG1_STRING				= 24,
		FEATURE_ARG2_ARITY				= 25,
		FEATURE_NUMBER					= 26,
		FEATURE_INFLECTION				= 27
	};

	enum function_type {
		FUNCTION_EMPTY										=   0,
		FUNCTION_IDENTITY									=   1,
		FUNCTION_IDENTITY_COORD								=   2,
		FUNCTION_NULL										=   3,
		FUNCTION_SELECT_LEFT								=   4,
		FUNCTION_SELECT_LEFT_COORD							=   5,
		FUNCTION_SELECT_LEFT_DELETE_FEATURES				=   6,
		FUNCTION_SELECT_LEFT_CONCORD_SINGULAR				=   7,
		FUNCTION_SELECT_LEFT_DISJOINT						=   8,
		FUNCTION_SELECT_LEFT_DISJOINT_COORD					=   9,
		FUNCTION_SELECT_LEFT2								=  10,
		FUNCTION_SELECT_LEFT2_DELETE_FEATURES				=  11,
		FUNCTION_SELECT_LEFT2_DISJOINT						=  12,
		FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR		=  13,
		FUNCTION_SELECT_LEFT2_DISJOINT_COORD				=  14,
		FUNCTION_SELECT_LEFT3_DISJOINT						=  15,
		FUNCTION_SELECT_LEFT4_DISJOINT						=  16,
		FUNCTION_SELECT_LEFT5_DISJOINT						=  17,
		FUNCTION_SELECT_LEFT6_DISJOINT						=  18,
		FUNCTION_SELECT_LEFT7_DISJOINT						=  19,
		FUNCTION_SELECT_LEFT_KEEP_FUNCTION					=  20,
		FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT			=  21,
		FUNCTION_SELECT_LEFT2_KEEP_FUNCTION					=  22,
		FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT		=  23,
		FUNCTION_SELECT_LEFT3_KEEP_FUNCTION					=  24,
		FUNCTION_SELECT_LEFT_DELETE_HEAD					=  25,
		FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR	=  26,
		FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT			=  27,
		FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD		=  28,
		FUNCTION_SELECT_LEFT_DELETE_FUNCTION				=  29,
		FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES		=  30,
		FUNCTION_SELECT_LEFT_DELETE_ANSWER					=  31,
		FUNCTION_SELECT_LEFT2_DELETE_ANSWER					=  32,
		FUNCTION_SELECT_LEFT3_DELETE_ANSWER					=  33,
		FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT		=  34,
		FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT		=  35,
		FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD				=  36,
		FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER			=  37,
		FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER			=  38,
		FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER			=  39,
		FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER			=  40,
		FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER		=  41,
		FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER		=  42,
		FUNCTION_SELECT_LEFT_DELETE_NOT						=  43,
		FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES			=  44,
		FUNCTION_SELECT_RIGHT								=  45,
		FUNCTION_SELECT_RIGHT2								=  46,
		FUNCTION_SELECT_RIGHT2_SINGULAR						=  47,
		FUNCTION_SELECT_RIGHT2_DISJOINT						=  48,
		FUNCTION_SELECT_RIGHT3_DISJOINT						=  49,
		FUNCTION_SELECT_RIGHT4_DISJOINT						=  50,
		FUNCTION_SELECT_RIGHT5_DISJOINT						=  51,
		FUNCTION_SELECT_RIGHT6_DISJOINT						=  52,
		FUNCTION_SELECT_RIGHT7_DISJOINT						=  53,
		FUNCTION_SELECT_RIGHT_DELETE_HEAD					=  54,
		FUNCTION_SELECT_RIGHT_DELETE_FUNCTION				=  55,
		FUNCTION_SELECT_RIGHT2_DELETE_ANSWER				=  56,
		FUNCTION_SELECT_FUNCTION							=  57,
		FUNCTION_SELECT_FUNCTION_DELETE_FEATURES			=  58,
		FUNCTION_SELECT_FUNCTION_DELETE_HEAD				=  59,
		FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES		=  60,

		FUNCTION_DELETE_LEFT								=  61,
		FUNCTION_DELETE_LEFT_COORD							=  62,
		FUNCTION_DELETE_LEFT_FEATURES						=  63,
		FUNCTION_DELETE_LEFT_DISJOINT						=  64,
		FUNCTION_DELETE_LEFT_DISJOINT_COORD					=  65,
		FUNCTION_DELETE_LEFT2								=  66,
		FUNCTION_DELETE_LEFT2_DISJOINT						=  67,
		FUNCTION_DELETE_LEFT2_DISJOINT_COORD				=  68,
		FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES						=  201,
		FUNCTION_DELETE_LEFT3_DISJOINT						=  69,
		FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES						=  202,
		FUNCTION_DELETE_LEFT4_DISJOINT						=  70,
		FUNCTION_DELETE_LEFT5_DISJOINT						=  71,
		FUNCTION_DELETE_LEFT6_DISJOINT						=  72,
		FUNCTION_DELETE_LEFT7_DISJOINT						=  73,
		FUNCTION_DELETE_LEFT_HEAD							=  74,
		FUNCTION_DELETE_LEFT_HEAD_FEATURES					=  75,
		FUNCTION_DELETE_LEFT2_HEAD							=  76,
		FUNCTION_DELETE_LEFT2_HEAD_FEATURES					=  77,
		FUNCTION_DELETE_LEFT_FUNCTION						=  78,
		FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT				=  79,
		FUNCTION_DELETE_LEFT2_FUNCTION						=  80,
		FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT				=  81,
		FUNCTION_DELETE_LEFT_FUNCTION_HEAD					=  82,
		FUNCTION_DELETE_LEFT3_FUNCTION_HEAD					=  83,
		FUNCTION_DELETE_LEFT_KEEP_ANSWER					=  84,
		FUNCTION_DELETE_LEFT_KEEP_FUNCTION					=  85,
		FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION			=  86,
		FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION				=  87,
		FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION	=  88,
		FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT					=  89,
		FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT			=  90,
		FUNCTION_DELETE_LEFT_ANSWER							=  91,
		FUNCTION_DELETE_LEFT2_ANSWER						=  92,
		FUNCTION_DELETE_LEFT3_ANSWER						=  93,
		FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT				=  94,
		FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT				=  95,
		FUNCTION_DELETE_LEFT_ANSWER_HEAD					=  96,
		FUNCTION_DELETE_LEFT_COUNT_ANSWER					=  97,
		FUNCTION_DELETE_LEFT2_COUNT_ANSWER					=  98,
		FUNCTION_DELETE_LEFT3_COUNT_ANSWER					=  99,
		FUNCTION_DELETE_LEFT_FUNCTION_ANSWER				= 100,
		FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER				= 101,
		FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER				= 102,
		FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION			= 103,
		FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION		= 104,
		FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION			= 105,
		FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION		= 106,
		FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION			= 107,
		FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION		= 108,
		FUNCTION_DELETE_RIGHT								= 109,
		FUNCTION_DELETE_RIGHT2								= 110,
		FUNCTION_DELETE_RIGHT2_DISJOINT						= 111,
		FUNCTION_DELETE_RIGHT3_DISJOINT						= 112,
		FUNCTION_DELETE_RIGHT4_DISJOINT						= 113,
		FUNCTION_DELETE_RIGHT5_DISJOINT						= 114,
		FUNCTION_DELETE_RIGHT6_DISJOINT						= 115,
		FUNCTION_DELETE_RIGHT7_DISJOINT						= 116,
		FUNCTION_DELETE_RIGHT_HEAD							= 117,
		FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION			= 118,
		FUNCTION_DELETE_RIGHT2_ANSWER						= 119,
		FUNCTION_DELETE_COUNT								= 120,
		FUNCTION_DELETE_COUNT_HEAD							= 121,
		FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL			= 122,
		FUNCTION_DELETE_COUNT_ANSWER						= 123,
		FUNCTION_DELETE_NOT									= 124,
		FUNCTION_DELETE_NOT_FEATURES						= 125,
		FUNCTION_DELETE_NOT_INFINITIVE						= 126,
		FUNCTION_DELETE_LEFT_NOT_HEAD						= 127,
		FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES				= 128,
		FUNCTION_DELETE_FUNCTION							= 129,
		FUNCTION_DELETE_FUNCTION_HEAD						= 130,
		FUNCTION_DELETE_FUNCTION_FEATURES					= 131,
		FUNCTION_DELETE_ANSWER								= 132,
		FUNCTION_DELETE_ANSWER_HAS_LOC						= 133,

		FUNCTION_SELECT_ARG1								= 134,
		FUNCTION_SELECT_ARG1_SINGULAR							= 235,
		FUNCTION_SELECT_ARG1_PLURAL								= 236,
		FUNCTION_SELECT_ARG1_ONLY								= 234,
		FUNCTION_SELECT_ARG1_DELETE_FEATURES					= 237,
		FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES				= 243,
		FUNCTION_SELECT_ARG2								= 136,
		FUNCTION_SELECT_ARG2_ONLY								= 244,
		FUNCTION_SELECT_ARG2_DELETE_FEATURES					= 238,
		FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES				= 245,
		FUNCTION_SELECT_ARG3								= 137,
		FUNCTION_SELECT_ARG3_ONLY								= 246,
		FUNCTION_SELECT_ARG3_DELETE_FEATURES				= 138,
		FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES				= 247,
		FUNCTION_DELETE_ARG1								= 139,
		FUNCTION_DELETE_ARG1_SINGULAR							= 239,
		FUNCTION_DELETE_ARG1_PLURAL								= 240,
		FUNCTION_DELETE_ARG1_FEATURES							= 241,
		FUNCTION_DELETE_ARG2								= 140,
		FUNCTION_DELETE_ARG2_FEATURES							= 242,
		FUNCTION_DELETE_ARG3								= 141,
		FUNCTION_DELETE_ARGS								= 142,
		FUNCTION_DELETE_ARGS_CONCORD_SINGULAR				= 143,
		FUNCTION_DELETE_ARGS_KEEP_PLURAL					= 144,
		FUNCTION_HEAD_ARG1_SELECT_ARG2						= 145,
		FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY					= 146,
		FUNCTION_EMPTY_TUPLE								= 147,
		FUNCTION_EMPTY_TUPLE_ONLY							= 148,
		FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR			= 149,
		FUNCTION_KEEP_NULL										= 249,
		FUNCTION_EMPTY_ARG2									= 150,
		FUNCTION_EMPTY_ARGS										= 248,
		FUNCTION_ARG2_ZERO_ARITY								= 250,
		FUNCTION_LOC										= 151,
		FUNCTION_TWO_PREDICATES								= 152,

		/* functions that relate to syntactic information */
		FUNCTION_SINGULAR									= 153,
		FUNCTION_PLURAL										= 154,
		FUNCTION_UNCOUNTABLE								= 155,
		FUNCTION_CONCORD_SINGULAR							= 156,
		FUNCTION_CONCORD_PLURAL								= 157,
		FUNCTION_CONCORD_UNCOUNTABLE						= 158,
		FUNCTION_CONCORD_NON_SINGULAR						= 159,
		FUNCTION_CONCORD_NON_PLURAL							= 160,
		FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR				= 252,
		FUNCTION_CONCORD_ALL								= 161,
		FUNCTION_KEEP_CONCORD_SINGULAR							= 251,
		FUNCTION_KEEP_CONCORD_PLURAL							= 253,
		FUNCTION_KEEP_CONCORD_UNCOUNTABLE						= 254,
		FUNCTION_DELETE_NOT_CONCORD_SINGULAR				= 162,
		FUNCTION_DELETE_NOT_CONCORD_PLURAL					= 163,
		FUNCTION_KEEP_SINGULAR								= 164,
		FUNCTION_KEEP_PLURAL								= 165,
		FUNCTION_KEEP_UNCOUNTABLE							= 166,
		FUNCTION_INFINITIVE									= 167,
		FUNCTION_PRESENT_PARTICIPLE							= 168,
		FUNCTION_PAST_PARTICIPLE							= 169,
		FUNCTION_KEEP_PRESENT_PARTICIPLE					= 170,
		FUNCTION_KEEP_PAST_PARTICIPLE						= 171,
		FUNCTION_FLIP_PREDICATE								= 172,
		FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE				= 173,
		FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE		= 174,
		FUNCTION_KEEP_FEATURES								= 175,
		FUNCTION_DELETE_FEATURES							= 176
	};

	struct function {
		function_type type;

		constexpr function(const function_type& type) : type(type) { }

		static inline unsigned int hash(const function& f) {
			return default_hash(f.type);
		}

		static inline bool is_empty(const function& f) {
			return f.type == FUNCTION_EMPTY;
		}

		static inline void set_empty(function& f) {
			f.type = FUNCTION_EMPTY;
		}
	};

	static constexpr function default_function() {
		return function(FUNCTION_NULL);
	}

	template<typename Stream>
	static inline bool read(feature& f, Stream& stream) {
		unsigned char c;
		if (!core::read(c, stream)) return false;
		f = static_cast<datalog_expression_root::feature>(c);
		return true;
	}

	template<typename Stream>
	static inline bool write(const feature& f, Stream& stream) {
		return core::write((unsigned char) f, stream);
	}

	static bool interpret(feature& f, const string& name) {
		if (name == "function") {
			f = datalog_expression_root::FEATURE_FUNCTION;
		} else if (name == "function_only") {
			f = datalog_expression_root::FEATURE_FUNCTION_ONLY;
		} else if (name == "function_answer") {
			f = datalog_expression_root::FEATURE_FUNCTION_ANSWER;
		} else if (name == "has_function") {
			f = datalog_expression_root::FEATURE_HAS_FUNCTION;
		} else if (name == "has_function_answer") {
			f = datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER;
		} else if (name == "has_function_not") {
			f = datalog_expression_root::FEATURE_HAS_FUNCTION_NOT;
		} else if (name == "has_function_count_not") {
			f = datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT;
		} else if (name == "predicate") {
			f = datalog_expression_root::FEATURE_PREDICATE;
		} else if (name == "predicate_only") {
			f = datalog_expression_root::FEATURE_PREDICATE_ONLY;
		} else if (name == "first_predicate") {
			f = datalog_expression_root::FEATURE_FIRST_PREDICATE;
		} else if (name == "second_predicate") {
			f = datalog_expression_root::FEATURE_SECOND_PREDICATE;
		} else if (name == "third_predicate") {
			f = datalog_expression_root::FEATURE_THIRD_PREDICATE;
		} else if (name == "last_predicate") {
			f = datalog_expression_root::FEATURE_LAST_PREDICATE;
		} else if (name == "direction") {
			f = datalog_expression_root::FEATURE_DIRECTION;
		} else if (name == "direction_root") {
			f = datalog_expression_root::FEATURE_DIRECTION_ROOT;
		} else if (name == "constant") {
			f = datalog_expression_root::FEATURE_CONSTANT;
		} else if (name == "arity") {
			f = datalog_expression_root::FEATURE_PREDICATE_ARITY;
		} else if (name == "arg1") {
			f = datalog_expression_root::FEATURE_ARG1;
		} else if (name == "arg2") {
			f = datalog_expression_root::FEATURE_ARG2;
		} else if (name == "arg3") {
			f = datalog_expression_root::FEATURE_ARG3;
		} else if (name == "arg1_only") {
			f = datalog_expression_root::FEATURE_ARG1_ONLY;
		} else if (name == "arg2_only") {
			f = datalog_expression_root::FEATURE_ARG2_ONLY;
		} else if (name == "arg3_only") {
			f = datalog_expression_root::FEATURE_ARG3_ONLY;
		} else if (name == "arg1_string") {
			f = datalog_expression_root::FEATURE_ARG1_STRING;
		} else if (name == "arg2_arity") {
			f = datalog_expression_root::FEATURE_ARG2_ARITY;
		} else if (name == "number") {
			f = datalog_expression_root::FEATURE_NUMBER;
		} else if (name == "inflection") {
			f = datalog_expression_root::FEATURE_INFLECTION;
		} else {
			core::print("parse ERROR: Unrecognized semantic feature name '", stderr);
			core::print(name, stderr); core::print("'.\n", stderr);
			return false;
		}
		return true;
	}

	template<typename Stream>
	static bool print(const feature& f, Stream& out) {
		switch (f) {
		case datalog_expression_root::FEATURE_NULL:
			return core::print("null", out);
		case datalog_expression_root::FEATURE_FUNCTION:
			return core::print("function", out);
		case datalog_expression_root::FEATURE_FUNCTION_ONLY:
			return core::print("function_only", out);
		case datalog_expression_root::FEATURE_FUNCTION_ANSWER:
			return core::print("function_answer", out);
		case datalog_expression_root::FEATURE_HAS_FUNCTION:
			return core::print("has_function", out);
		case datalog_expression_root::FEATURE_HAS_FUNCTION_NOT:
			return core::print("has_function_not", out);
		case datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT:
			return core::print("has_function_count_not", out);
		case datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER:
			return core::print("has_function_answer", out);
		case datalog_expression_root::FEATURE_PREDICATE:
			return core::print("predicate", out);
		case datalog_expression_root::FEATURE_PREDICATE_ONLY:
			return core::print("predicate_only", out);
		case datalog_expression_root::FEATURE_FIRST_PREDICATE:
			return core::print("first_predicate", out);
		case datalog_expression_root::FEATURE_SECOND_PREDICATE:
			return core::print("second_predicate", out);
		case datalog_expression_root::FEATURE_THIRD_PREDICATE:
			return core::print("third_predicate", out);
		case datalog_expression_root::FEATURE_LAST_PREDICATE:
			return core::print("last_predicate", out);
		case datalog_expression_root::FEATURE_DIRECTION:
			return core::print("direction", out);
		case datalog_expression_root::FEATURE_DIRECTION_ROOT:
			return core::print("direction_root", out);
		case datalog_expression_root::FEATURE_CONSTANT:
			return core::print("constant", out);
		case datalog_expression_root::FEATURE_PREDICATE_ARITY:
			return core::print("arity", out);
		case datalog_expression_root::FEATURE_ARG1:
			return core::print("arg1", out);
		case datalog_expression_root::FEATURE_ARG2:
			return core::print("arg2", out);
		case datalog_expression_root::FEATURE_ARG3:
			return core::print("arg3", out);
		case datalog_expression_root::FEATURE_ARG1_ONLY:
			return core::print("arg1_only", out);
		case datalog_expression_root::FEATURE_ARG2_ONLY:
			return core::print("arg2_only", out);
		case datalog_expression_root::FEATURE_ARG3_ONLY:
			return core::print("arg3_only", out);
		case datalog_expression_root::FEATURE_ARG1_STRING:
			return core::print("arg1_string", out);
		case datalog_expression_root::FEATURE_ARG2_ARITY:
			return core::print("arg2_arity", out);
		case datalog_expression_root::FEATURE_NUMBER:
			return core::print("number", out);
		case datalog_expression_root::FEATURE_INFLECTION:
			return core::print("inflection", out);
		}
		fprintf(stderr, "print ERROR: Unrecognized semantic feature.\n");
		return false;
	}

	template<typename Stream>
	static inline bool read(function& f, Stream& stream) {
		unsigned char c;
		if (!core::read(c, stream)) return false;
		f.type = static_cast<datalog_expression_root::function_type>(c);
		return true;
	}

	template<typename Stream>
	static inline bool write(const function& f, Stream& stream) {
		return core::write((unsigned char) f.type, stream);
	}

	template<typename T, typename Stream>
	static inline bool read(T* array, Stream& stream, unsigned int length) {
		for (unsigned int i = 0; i < length; i++)
			if (!read(array[i], stream)) return false;
		return true;
	}

	template<typename T, typename Stream>
	static inline bool write(const T* array, Stream& stream, unsigned int length) {
		for (unsigned int i = 0; i < length; i++)
			if (!write(array[i], stream)) return false;
		return true;
	}

	static bool interpret(function& f, const string& name) {
		if (name == "identity") {
			f.type = datalog_expression_root::FUNCTION_IDENTITY;
		} else if (name == "identity_coord") {
			f.type = datalog_expression_root::FUNCTION_IDENTITY_COORD;
		} else if (name == "null") {
			f.type = datalog_expression_root::FUNCTION_NULL;
		} else if (name == "select_left") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT;
		} else if (name == "select_left_coord") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_COORD;
		} else if (name == "select_left_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES;
		} else if (name == "select_left_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR;
		} else if (name == "select_left_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT;
		} else if (name == "select_left_disjoint_coord") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD;
		} else if (name == "select_left2") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2;
		} else if (name == "select_left2_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES;
		} else if (name == "select_left2_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT;
		} else if (name == "select_left2_disjoint_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR;
		} else if (name == "select_left2_disjoint_coord") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD;
		} else if (name == "select_left3_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_DISJOINT;
		} else if (name == "select_left4_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT4_DISJOINT;
		} else if (name == "select_left5_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT5_DISJOINT;
		} else if (name == "select_left6_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT6_DISJOINT;
		} else if (name == "select_left7_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT7_DISJOINT;
		} else if (name == "select_left_delete_head") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD;
		} else if (name == "select_left_delete_head_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR;
		} else if (name == "select_left_delete_head_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT;
		} else if (name == "select_left_delete_head_disjoint_coord") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD;
		} else if (name == "select_left_keep_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION;
		} else if (name == "select_left_keep_function_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT;
		} else if (name == "select_left2_keep_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION;
		} else if (name == "select_left2_keep_function_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT;
		} else if (name == "select_left3_keep_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_KEEP_FUNCTION;
		} else if (name == "select_left_delete_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION;
		} else if (name == "select_left_delete_function_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES;
		} else if (name == "select_left_delete_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER;
		} else if (name == "select_left2_delete_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_ANSWER;
		} else if (name == "select_left3_delete_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER;
		} else if (name == "select_left3_delete_answer_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT;
		} else if (name == "select_left5_delete_answer_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT;
		} else if (name == "select_left_delete_answer_head") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD;
		} else if (name == "select_left_delete_count_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER;
		} else if (name == "select_left2_delete_count_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER;
		} else if (name == "select_left3_delete_count_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER;
		} else if (name == "select_left_delete_function_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER;
		} else if (name == "select_left2_delete_function_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER;
		} else if (name == "select_left3_delete_function_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER;
		} else if (name == "select_left_delete_not") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT;
		} else if (name == "select_left_delete_not_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES;
		} else if (name == "select_right") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT;
		} else if (name == "select_right2") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT2;
		} else if (name == "select_right2_singular") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR;
		} else if (name == "select_right2_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT2_DISJOINT;
		} else if (name == "select_right3_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT3_DISJOINT;
		} else if (name == "select_right4_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT4_DISJOINT;
		} else if (name == "select_right5_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT5_DISJOINT;
		} else if (name == "select_right6_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT6_DISJOINT;
		} else if (name == "select_right7_disjoint") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT7_DISJOINT;
		} else if (name == "select_right_delete_head") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_HEAD;
		} else if (name == "select_right_delete_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_FUNCTION;
		} else if (name == "select_right2_delete_answer") {
			f.type = datalog_expression_root::FUNCTION_SELECT_RIGHT2_DELETE_ANSWER;
		} else if (name == "select_function") {
			f.type = datalog_expression_root::FUNCTION_SELECT_FUNCTION;
		} else if (name == "select_function_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES;
		} else if (name == "select_function_delete_head") {
			f.type = datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD;
		} else if (name == "select_function_delete_head_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES;
		} else if (name == "delete_left") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT;
		} else if (name == "delete_left_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES;
		} else if (name == "delete_left_coord") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_COORD;
		} else if (name == "delete_left_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT;
		} else if (name == "delete_left_disjoint_coord") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD;
		} else if (name == "delete_left2") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2;
		} else if (name == "delete_left2_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT;
		} else if (name == "delete_left2_disjoint_coord") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD;
		} else if (name == "delete_left2_disjoint_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES;
		} else if (name == "delete_left3_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT;
		} else if (name == "delete_left3_disjoint_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES;
		} else if (name == "delete_left4_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT4_DISJOINT;
		} else if (name == "delete_left5_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT5_DISJOINT;
		} else if (name == "delete_left6_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT6_DISJOINT;
		} else if (name == "delete_left7_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT7_DISJOINT;
		} else if (name == "delete_left_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD;
		} else if (name == "delete_left_head_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES;
		} else if (name == "delete_left2_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD;
		} else if (name == "delete_left2_head_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES;
		} else if (name == "delete_left_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION;
		} else if (name == "delete_left_function_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT;
		} else if (name == "delete_left2_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION;
		} else if (name == "delete_left2_function_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT;
		} else if (name == "delete_left_function_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_HEAD;
		} else if (name == "delete_left3_function_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_HEAD;
		} else if (name == "delete_left_keep_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_ANSWER;
		} else if (name == "delete_left_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_FUNCTION;
		} else if (name == "delete_left_features_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION;
		} else if (name == "delete_left_head_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION;
		} else if (name == "delete_left_head_features_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION;
		} else if (name == "delete_left_head_keep_not") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT;
		} else if (name == "delete_left_head_features_keep_not") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT;
		} else if (name == "delete_left_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER;
		} else if (name == "delete_left2_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER;
		} else if (name == "delete_left3_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER;
		} else if (name == "delete_left3_answer_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT;
		} else if (name == "delete_left5_answer_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT;
		} else if (name == "delete_left_answer_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD;
		} else if (name == "delete_left_count_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_COUNT_ANSWER;
		} else if (name == "delete_left2_count_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_COUNT_ANSWER;
		} else if (name == "delete_left3_count_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_COUNT_ANSWER;
		} else if (name == "delete_left_function_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_ANSWER;
		} else if (name == "delete_left2_function_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER;
		} else if (name == "delete_left3_function_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER;
		} else if (name == "delete_left_answer_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION;
		} else if (name == "delete_left_answer_head_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION;
		} else if (name == "delete_left2_answer_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION;
		} else if (name == "delete_left2_answer_head_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION;
		} else if (name == "delete_left3_answer_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION;
		} else if (name == "delete_left3_answer_head_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION;
		} else if (name == "delete_right") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT;
		} else if (name == "delete_right2") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT2;
		} else if (name == "delete_right2_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT2_DISJOINT;
		} else if (name == "delete_right3_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT3_DISJOINT;
		} else if (name == "delete_right4_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT4_DISJOINT;
		} else if (name == "delete_right5_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT5_DISJOINT;
		} else if (name == "delete_right6_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT6_DISJOINT;
		} else if (name == "delete_right7_disjoint") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT7_DISJOINT;
		} else if (name == "delete_right_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD;
		} else if (name == "delete_right_head_keep_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION;
		} else if (name == "delete_right2_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_RIGHT2_ANSWER;
		} else if (name == "delete_function") {
			f.type = datalog_expression_root::FUNCTION_DELETE_FUNCTION;
		} else if (name == "delete_function_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_FUNCTION_HEAD;
		} else if (name == "delete_function_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES;
		} else if (name == "delete_count") {
			f.type = datalog_expression_root::FUNCTION_DELETE_COUNT;
		} else if (name == "delete_count_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD;
		} else if (name == "delete_count_head_concord_plural") {
			f.type = datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL;
		} else if (name == "delete_count_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_COUNT_ANSWER;
		} else if (name == "delete_not") {
			f.type = datalog_expression_root::FUNCTION_DELETE_NOT;
		} else if (name == "delete_not_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES;
		} else if (name == "delete_not_infinitive") {
			f.type = datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE;
		} else if (name == "delete_left_not_head") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD;
		} else if (name == "delete_left_not_head_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES;
		} else if (name == "delete_answer") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ANSWER;
		} else if (name == "delete_answer_has_loc") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ANSWER_HAS_LOC;
		} else if (name == "select_arg1") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1;
		} else if (name == "select_arg1_singular") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR;
		} else if (name == "select_arg1_plural") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL;
		} else if (name == "select_arg1_only") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY;
		} else if (name == "select_arg1_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES;
		} else if (name == "select_arg1_only_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES;
		} else if (name == "select_arg2") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG2;
		} else if (name == "select_arg2_only") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY;
		} else if (name == "select_arg2_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES;
		} else if (name == "select_arg2_only_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES;
		} else if (name == "select_arg3") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG3;
		} else if (name == "select_arg3_only") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY;
		} else if (name == "select_arg3_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES;
		} else if (name == "select_arg3_only_delete_features") {
			f.type = datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES;
		} else if (name == "delete_arg1") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG1;
		} else if (name == "delete_arg1_singular") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR;
		} else if (name == "delete_arg1_plural") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL;
		} else if (name == "delete_arg1_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES;
		} else if (name == "delete_arg2") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG2;
		} else if (name == "delete_arg2_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES;
		} else if (name == "delete_arg3") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARG3;
		} else if (name == "delete_args") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARGS;
		} else if (name == "delete_args_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR;
		} else if (name == "delete_args_keep_plural") {
			f.type = datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL;
		} else if (name == "head_arg1_select_arg2") {
			f.type = datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2;
		} else if (name == "head_arg1_select_arg2_only") {
			f.type = datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY;
		} else if (name == "empty_tuple") {
			f.type = datalog_expression_root::FUNCTION_EMPTY_TUPLE;
		} else if (name == "empty_tuple_only") {
			f.type = datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY;
		} else if (name == "empty_tuple_only_keep_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR;
		} else if (name == "keep_null") {
			f.type = datalog_expression_root::FUNCTION_KEEP_NULL;
		} else if (name == "empty_arg2") {
			f.type = datalog_expression_root::FUNCTION_EMPTY_ARG2;
		} else if (name == "empty_args") {
			f.type = datalog_expression_root::FUNCTION_EMPTY_ARGS;
		} else if (name == "arg2_zero_arity") {
			f.type = datalog_expression_root::FUNCTION_ARG2_ZERO_ARITY;
		} else if (name == "loc") {
			f.type = datalog_expression_root::FUNCTION_LOC;
		} else if (name == "two_predicates") {
			f.type = datalog_expression_root::FUNCTION_TWO_PREDICATES;
		} else if (name == "singular") {
			f.type = datalog_expression_root::FUNCTION_SINGULAR;
		} else if (name == "plural") {
			f.type = datalog_expression_root::FUNCTION_PLURAL;
		} else if (name == "uncountable") {
			f.type = datalog_expression_root::FUNCTION_UNCOUNTABLE;
		} else if (name == "concord_singular") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_SINGULAR;
		} else if (name == "concord_plural") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_PLURAL;
		} else if (name == "concord_uncountable") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE;
		} else if (name == "concord_non_singular") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR;
		} else if (name == "concord_non_plural") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL;
		} else if (name == "concord_non_plural_keep_singular") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR;
		} else if (name == "concord_all") {
			f.type = datalog_expression_root::FUNCTION_CONCORD_ALL;
		} else if (name == "keep_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR;
		} else if (name == "keep_concord_plural") {
			f.type = datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL;
		} else if (name == "keep_concord_uncountable") {
			f.type = datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE;
		} else if (name == "delete_not_concord_singular") {
			f.type = datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR;
		} else if (name == "delete_not_concord_plural") {
			f.type = datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL;
		} else if (name == "keep_singular") {
			f.type = datalog_expression_root::FUNCTION_KEEP_SINGULAR;
		} else if (name == "keep_plural") {
			f.type = datalog_expression_root::FUNCTION_KEEP_PLURAL;
		} else if (name == "keep_uncountable") {
			f.type = datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE;
		} else if (name == "infinitive") {
			f.type = datalog_expression_root::FUNCTION_INFINITIVE;
		} else if (name == "present_participle") {
			f.type = datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE;
		} else if (name == "past_participle") {
			f.type = datalog_expression_root::FUNCTION_PAST_PARTICIPLE;
		} else if (name == "keep_present_participle") {
			f.type = datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE;
		} else if (name == "keep_past_participle") {
			f.type = datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE;
		} else if (name == "flip_predicate") {
			f.type = datalog_expression_root::FUNCTION_FLIP_PREDICATE;
		} else if (name == "flip_predicate_past_participle") {
			f.type = datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE;
		} else if (name == "flip_predicate_keep_past_participle") {
			f.type = datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE;
		} else if (name == "keep_features") {
			f.type = datalog_expression_root::FUNCTION_KEEP_FEATURES;
		} else if (name == "delete_features") {
			f.type = datalog_expression_root::FUNCTION_DELETE_FEATURES;
		} else {
			fprintf(stderr, "parse ERROR: Unrecognized semantic transformation function name.\n");
			return false;
		}
		return true;
	}

	template<typename Stream>
	static bool print(const function& f, Stream& out) {
		switch (f.type) {
		case datalog_expression_root::FUNCTION_EMPTY:
			return core::print("empty", out);
		case datalog_expression_root::FUNCTION_IDENTITY:
			return core::print("identity", out);
		case datalog_expression_root::FUNCTION_IDENTITY_COORD:
			return core::print("identity_coord", out);
		case datalog_expression_root::FUNCTION_NULL:
			return core::print("null", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT:
			return core::print("select_left", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_COORD:
			return core::print("select_left_coord", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES:
			return core::print("select_left_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR:
			return core::print("select_left_concord_singular", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT:
			return core::print("select_left_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD:
			return core::print("select_left_disjoint_coord", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2:
			return core::print("select_left2", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES:
			return core::print("select_left2_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT:
			return core::print("select_left2_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR:
			return core::print("select_left2_disjoint_concord_singular", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD:
			return core::print("select_left2_disjoint_coord", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_DISJOINT:
			return core::print("select_left3_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT4_DISJOINT:
			return core::print("select_left4_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT5_DISJOINT:
			return core::print("select_left5_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT6_DISJOINT:
			return core::print("select_left6_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT7_DISJOINT:
			return core::print("select_left7_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD:
			return core::print("select_left_delete_head", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR:
			return core::print("select_left_delete_head_concord_singular", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT:
			return core::print("select_left_delete_head_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD:
			return core::print("select_left_delete_head_disjoint_coord", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION:
			return core::print("select_left_keep_function", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT:
			return core::print("select_left_keep_function_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION:
			return core::print("select_left2_keep_function", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT:
			return core::print("select_left2_keep_function_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_KEEP_FUNCTION:
			return core::print("select_left3_keep_function", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION:
			return core::print("select_left_delete_function", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES:
			return core::print("select_left_delete_function_features", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER:
			return core::print("select_left_delete_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_ANSWER:
			return core::print("select_left2_delete_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER:
			return core::print("select_left3_delete_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT:
			return core::print("select_left3_delete_answer_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT:
			return core::print("select_left5_delete_answer_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD:
			return core::print("select_left_delete_answer_head", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER:
			return core::print("select_left_delete_count_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER:
			return core::print("select_left2_delete_count_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER:
			return core::print("select_left3_delete_count_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER:
			return core::print("select_left_delete_function_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER:
			return core::print("select_left2_delete_function_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER:
			return core::print("select_left3_delete_function_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT:
			return core::print("select_left_delete_not", out);
		case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES:
			return core::print("select_left_delete_not_features", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT:
			return core::print("select_right", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT2:
			return core::print("select_right2", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR:
			return core::print("select_right2_singular", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DISJOINT:
			return core::print("select_right2_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT3_DISJOINT:
			return core::print("select_right3_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT4_DISJOINT:
			return core::print("select_right4_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT5_DISJOINT:
			return core::print("select_right5_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT6_DISJOINT:
			return core::print("select_right6_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT7_DISJOINT:
			return core::print("select_right7_disjoint", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_HEAD:
			return core::print("select_right_delete_head", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_FUNCTION:
			return core::print("select_right_delete_function", out);
		case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DELETE_ANSWER:
			return core::print("select_right2_delete_answer", out);
		case datalog_expression_root::FUNCTION_SELECT_FUNCTION:
			return core::print("select_function", out);
		case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES:
			return core::print("select_function_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD:
			return core::print("select_function_delete_head", out);
		case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES:
			return core::print("select_function_delete_head_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT:
			return core::print("delete_left", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_COORD:
			return core::print("delete_left_coord", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES:
			return core::print("delete_left_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT:
			return core::print("delete_left_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD:
			return core::print("delete_left_disjoint_coord", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2:
			return core::print("delete_left2", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT:
			return core::print("delete_left2_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD:
			return core::print("delete_left2_disjoint_coord", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES:
			return core::print("delete_left2_disjoint_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT:
			return core::print("delete_left3_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES:
			return core::print("delete_left3_disjoint_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT4_DISJOINT:
			return core::print("delete_left4_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT5_DISJOINT:
			return core::print("delete_left5_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT6_DISJOINT:
			return core::print("delete_left6_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT7_DISJOINT:
			return core::print("delete_left7_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD:
			return core::print("delete_left_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES:
			return core::print("delete_left_head_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD:
			return core::print("delete_left2_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES:
			return core::print("delete_left2_head_features", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION:
			return core::print("delete_left_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT:
			return core::print("delete_left_function_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION:
			return core::print("delete_left2_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT:
			return core::print("delete_left2_function_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_HEAD:
			return core::print("delete_left_function_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_HEAD:
			return core::print("delete_left3_function_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_ANSWER:
			return core::print("delete_left_keep_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_FUNCTION:
			return core::print("delete_left_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION:
			return core::print("delete_left_features_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION:
			return core::print("delete_left_head_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION:
			return core::print("delete_left_head_features_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT:
			return core::print("delete_left_head_keep_not", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT:
			return core::print("delete_left_head_features_keep_not", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER:
			return core::print("delete_left_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER:
			return core::print("delete_left2_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER:
			return core::print("delete_left3_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT:
			return core::print("delete_left3_answer_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT:
			return core::print("delete_left5_answer_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD:
			return core::print("delete_left_answer_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_COUNT_ANSWER:
			return core::print("delete_left_count_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_COUNT_ANSWER:
			return core::print("delete_left2_count_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_COUNT_ANSWER:
			return core::print("delete_left3_count_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_ANSWER:
			return core::print("delete_left_function_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER:
			return core::print("delete_left2_function_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER:
			return core::print("delete_left3_function_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION:
			return core::print("delete_left_answer_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION:
			return core::print("delete_left_answer_head_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION:
			return core::print("delete_left2_answer_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION:
			return core::print("delete_left2_answer_head_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION:
			return core::print("delete_left3_answer_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION:
			return core::print("delete_left3_answer_head_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT:
			return core::print("delete_right", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT2:
			return core::print("delete_right2", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT2_DISJOINT:
			return core::print("delete_right2_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT3_DISJOINT:
			return core::print("delete_right3_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT4_DISJOINT:
			return core::print("delete_right4_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT5_DISJOINT:
			return core::print("delete_right5_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT6_DISJOINT:
			return core::print("delete_right6_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT7_DISJOINT:
			return core::print("delete_right7_disjoint", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD:
			return core::print("delete_right_head", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION:
			return core::print("delete_right_head_keep_function", out);
		case datalog_expression_root::FUNCTION_DELETE_RIGHT2_ANSWER:
			return core::print("delete_right2_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_COUNT:
			return core::print("delete_count", out);
		case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD:
			return core::print("delete_count_head", out);
		case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL:
			return core::print("delete_count_head_concord_plural", out);
		case datalog_expression_root::FUNCTION_DELETE_COUNT_ANSWER:
			return core::print("delete_count_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_NOT:
			return core::print("delete_not", out);
		case datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES:
			return core::print("delete_not_features", out);
		case datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE:
			return core::print("delete_not_infinitive", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD:
			return core::print("delete_left_not_head", out);
		case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES:
			return core::print("delete_left_not_head_features", out);
		case datalog_expression_root::FUNCTION_DELETE_FUNCTION:
			return core::print("delete_function", out);
		case datalog_expression_root::FUNCTION_DELETE_FUNCTION_HEAD:
			return core::print("delete_function_head", out);
		case datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES:
			return core::print("delete_function_features", out);
		case datalog_expression_root::FUNCTION_DELETE_ANSWER:
			return core::print("delete_answer", out);
		case datalog_expression_root::FUNCTION_DELETE_ANSWER_HAS_LOC:
			return core::print("delete_answer_has_loc", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1:
			return core::print("select_arg1", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR:
			return core::print("select_arg1_singular", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL:
			return core::print("select_arg1_plural", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY:
			return core::print("select_arg1_only", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES:
			return core::print("select_arg1_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES:
			return core::print("select_arg1_only_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG2:
			return core::print("select_arg2", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY:
			return core::print("select_arg2_only", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES:
			return core::print("select_arg2_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES:
			return core::print("select_arg2_only_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG3:
			return core::print("select_arg3", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY:
			return core::print("select_arg3_only", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES:
			return core::print("select_arg3_delete_features", out);
		case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES:
			return core::print("select_arg3_only_delete_features", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG1:
			return core::print("delete_arg1", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR:
			return core::print("delete_arg1_singular", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL:
			return core::print("delete_arg1_plural", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES:
			return core::print("delete_arg1_features", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG2:
			return core::print("delete_arg2", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES:
			return core::print("delete_arg2_features", out);
		case datalog_expression_root::FUNCTION_DELETE_ARG3:
			return core::print("delete_arg3", out);
		case datalog_expression_root::FUNCTION_DELETE_ARGS:
			return core::print("delete_args", out);
		case datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR:
			return core::print("delete_args_concord_singular", out);
		case datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL:
			return core::print("delete_args_keep_plural", out);
		case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2:
			return core::print("head_arg1_select_arg2", out);
		case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY:
			return core::print("head_arg1_select_arg2_only", out);
		case datalog_expression_root::FUNCTION_EMPTY_TUPLE:
			return core::print("empty_tuple", out);
		case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY:
			return core::print("empty_tuple_only", out);
		case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR:
			return core::print("empty_tuple_only_keep_concord_singular", out);
		case datalog_expression_root::FUNCTION_KEEP_NULL:
			return core::print("keep_null", out);
		case datalog_expression_root::FUNCTION_EMPTY_ARG2:
			return core::print("empty_arg2", out);
		case datalog_expression_root::FUNCTION_EMPTY_ARGS:
			return core::print("empty_args", out);
		case datalog_expression_root::FUNCTION_ARG2_ZERO_ARITY:
			return core::print("arg2_zero_arity", out);
		case datalog_expression_root::FUNCTION_LOC:
			return core::print("loc", out);
		case datalog_expression_root::FUNCTION_TWO_PREDICATES:
			return core::print("two_predicates", out);
		case datalog_expression_root::FUNCTION_SINGULAR:
			return core::print("singular", out);
		case datalog_expression_root::FUNCTION_PLURAL:
			return core::print("plural", out);
		case datalog_expression_root::FUNCTION_UNCOUNTABLE:
			return core::print("uncountable", out);
		case datalog_expression_root::FUNCTION_CONCORD_SINGULAR:
			return core::print("concord_singular", out);
		case datalog_expression_root::FUNCTION_CONCORD_PLURAL:
			return core::print("concord_plural", out);
		case datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE:
			return core::print("concord_uncountable", out);
		case datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR:
			return core::print("concord_non_singular", out);
		case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL:
			return core::print("concord_non_plural", out);
		case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR:
			return core::print("concord_non_plural_keep_singular", out);
		case datalog_expression_root::FUNCTION_CONCORD_ALL:
			return core::print("concord_all", out);
		case datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR:
			return core::print("keep_concord_singular", out);
		case datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL:
			return core::print("keep_concord_plural", out);
		case datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE:
			return core::print("keep_concord_uncountable", out);
		case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR:
			return core::print("delete_not_concord_singular", out);
		case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL:
			return core::print("delete_not_concord_plural", out);
		case datalog_expression_root::FUNCTION_KEEP_SINGULAR:
			return core::print("keep_singular", out);
		case datalog_expression_root::FUNCTION_KEEP_PLURAL:
			return core::print("keep_plural", out);
		case datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE:
			return core::print("keep_uncountable", out);
		case datalog_expression_root::FUNCTION_INFINITIVE:
			return core::print("infinitive", out);
		case datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE:
			return core::print("present_participle", out);
		case datalog_expression_root::FUNCTION_PAST_PARTICIPLE:
			return core::print("past_participle", out);
		case datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE:
			return core::print("keep_present_participle", out);
		case datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE:
			return core::print("keep_past_participle", out);
		case datalog_expression_root::FUNCTION_FLIP_PREDICATE:
			return core::print("flip_predicate", out);
		case datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE:
			return core::print("flip_predicate_past_participle", out);
		case datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE:
			return core::print("flip_predicate_keep_past_participle", out);
		case datalog_expression_root::FUNCTION_KEEP_FEATURES:
			return core::print("keep_features", out);
		case datalog_expression_root::FUNCTION_DELETE_FEATURES:
			return core::print("delete_features", out);
		}
		fprintf(stderr, "print ERROR: Unrecognized semantic transformation function.\n");
		return false;
	}

	static bool is_feature_pruneable(feature f) {
		switch (f) {
		case datalog_expression_root::FEATURE_SECOND_PREDICATE:
		case datalog_expression_root::FEATURE_THIRD_PREDICATE:
		case datalog_expression_root::FEATURE_LAST_PREDICATE:
		case datalog_expression_root::FEATURE_DIRECTION:
		case datalog_expression_root::FEATURE_DIRECTION_ROOT:
		case datalog_expression_root::FEATURE_ARG1:
		case datalog_expression_root::FEATURE_ARG1_ONLY:
		case datalog_expression_root::FEATURE_ARG1_STRING:
		case datalog_expression_root::FEATURE_ARG2:
		case datalog_expression_root::FEATURE_ARG2_ONLY:
		case datalog_expression_root::FEATURE_ARG3:
		case datalog_expression_root::FEATURE_ARG3_ONLY:
		case datalog_expression_root::FEATURE_NUMBER:
		case datalog_expression_root::FEATURE_INFLECTION:
			return false;
		case datalog_expression_root::FEATURE_FIRST_PREDICATE:
		case datalog_expression_root::FEATURE_FUNCTION:
		case datalog_expression_root::FEATURE_FUNCTION_ONLY:
		case datalog_expression_root::FEATURE_FUNCTION_ANSWER:
		case datalog_expression_root::FEATURE_HAS_FUNCTION:
		case datalog_expression_root::FEATURE_HAS_FUNCTION_NOT:
		case datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT:
		case datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER:
		case datalog_expression_root::FEATURE_PREDICATE_ARITY:
		case datalog_expression_root::FEATURE_ARG2_ARITY:
		case datalog_expression_root::FEATURE_PREDICATE:
		case datalog_expression_root::FEATURE_PREDICATE_ONLY:
		case datalog_expression_root::FEATURE_CONSTANT:
			return true;
		case datalog_expression_root::FEATURE_NULL:
			break;
		}
		fprintf(stderr, "datalog_expression_root.is_feature_pruneable ERROR: Unrecognized semantic feature.\n");
		exit(EXIT_FAILURE);
	}
};

/* forward declarations */

bool apply(datalog_expression_root::function, const datalog_expression&, datalog_expression&);
bool apply(datalog_expression_root::function, const datalog_expression_root&, datalog_expression_root&);
bool intersect(
		datalog_expression&, const datalog_expression*, const datalog_expression*,
		array<unsigned int>&, array<unsigned int>&, array<unsigned int>&);
bool operator == (const datalog_expression&, const datalog_expression&);
bool operator != (const datalog_expression&, const datalog_expression&);

thread_local datalog_expression DATALOG_ANY_TREE;
thread_local datalog_expression DATALOG_NON_EMPTY_TREE(DATALOG_NON_EMPTY);

inline bool new_expression(datalog_expression*& expression) {
	expression = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (expression == NULL) {
		fprintf(stderr, "new_expression ERROR: Out of memory.\n");
		return false;
	}
	return expression;
}

inline bool init(unsigned int& variable, unsigned int src) {
	variable = src;
	return true;
}

inline bool init(unsigned int& variable, unsigned int src, int offset) {
	variable = src + offset;
	return true;
}

inline bool initialize_any(datalog_expression& exp) {
	exp.type = DATALOG_ANY;
	exp.reference_count = 1;
	return true;
}

inline bool initialize_any(datalog_expression_root& exp) {
	if (!initialize_any(exp.root)) return false;
	exp.index = NUMBER_ANY;
	exp.concord = NUMBER_ANY;
	exp.inf = INFLECTION_ANY;
	return true;
}

inline bool ensure_variable_map_capacity(array<unsigned int>& variable_map, unsigned int variable) {
	unsigned int old_capacity = variable_map.capacity;
	if (!variable_map.ensure_capacity(variable + 1)) return false;
	memset(variable_map.data + old_capacity, 0, sizeof(unsigned int) * (variable_map.capacity - old_capacity));
	return true;
}

inline bool init(unsigned int& variable, unsigned int src,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map)
{
	if (!ensure_variable_map_capacity(src_variable_map, src))
		return false;
	else if (src_variable_map[src] != 0) {
		variable = src_variable_map[src];
		if (!ensure_variable_map_capacity(dst_variable_map, variable))
			return false;
		if (dst_variable_map[variable] == 0)
			dst_variable_map[variable] = 1;
		return true;
	}

	unsigned int i;
	for (i = src - 1; i > 0; i--)
		if (src_variable_map[i] != 0) break;
	unsigned int first = src_variable_map[i] + src - i;

	for (unsigned int j = src_variable_map[i] + 1; j < first; j++) {
		if (!ensure_variable_map_capacity(dst_variable_map, j + 1))
			return false;
		else if (dst_variable_map[j]) first++;
	}
	if (!ensure_variable_map_capacity(dst_variable_map, first))
		return false;
	while (dst_variable_map[first]) {
		first++;
		if (!ensure_variable_map_capacity(dst_variable_map, first))
			return false;
	}

	src_variable_map[src] = first;
	dst_variable_map[first] = 1;
	variable = first;
	return true;
}

inline bool init(unsigned int& variable, unsigned int src,
		const array_map<unsigned int, unsigned int>& variable_map)
{
	variable = variable_map.get(src);
	return true;
}

template<typename... Args>
inline bool init(datalog_expression*& expression, datalog_expression* src, Args&&... args)
{
	if (!new_expression(expression)) return false;
	if (!init(*expression, *src, std::forward<Args>(args)...)) {
		free(expression); return false;
	}
	return true;
}

template<>
inline bool init<>(datalog_expression*& expression, datalog_expression* src) {
	expression = src;
	src->reference_count++;
	return true;
}

inline bool init(datalog_expression_root*& expression, datalog_expression* src,
		grammatical_number index = NUMBER_ALL, grammatical_number concord = NUMBER_NONE, inflection inf = INFLECTION_NONE)
{
	expression = (datalog_expression_root*) malloc(sizeof(datalog_expression_root));
	if (expression == NULL) {
		fprintf(stderr, "init ERROR: Insufficient memory for datalog_expression_root.\n");
		return false;
	} else if (!init(expression->root, *src)) {
		free(expression);
		return false;
	}
	expression->index = index;
	expression->concord = concord;
	expression->inf = inf;
	return true;
}

template<typename... Args>
inline bool init(datalog_expression**& args,
		datalog_expression* const* const expressions,
		unsigned int count, Args&&... optional_args)
{
	for (unsigned int i = 0; i < count; i++) {
		if (!init(args[i], expressions[i], std::forward<Args>(optional_args)...)) {
			for (unsigned int j = 0; j < i; j++) {
				free(*args[i]);
				if (args[i]->reference_count == 0)
					free(args[i]);
			}
			return false;
		}
	}
	return true;
}

template<typename... Args>
inline bool init(array<datalog_expression*>& args,
		datalog_expression* const* const expressions,
		unsigned int count, Args&&... optional_args)
{
	if (!array_init(args, max(1u, count))) {
		return false;
	} else if (!init(args.data, expressions, count, std::forward<Args>(optional_args)...)) {
		free(args); return false;
	}
	args.length = count;
	return true;
}

inline bool init(datalog_function& func) {
	func.excluded_count = 0;
	func.arg = NULL;
	return true;
}

inline bool init_excluded(unsigned int*& excluded, const unsigned int* src, unsigned int src_count) {
	excluded = (unsigned int*) malloc(sizeof(unsigned int) * src_count);
	if (excluded == NULL) {
		fprintf(stderr, "init_excluded ERROR: Out of memory.\n");
		return false;
	}
	memcpy(excluded, src, sizeof(unsigned int) * src_count);
	return true;
}

template<typename... Args>
inline bool init(datalog_function& func, const datalog_function& src, Args&&... args)
{
	func.function = src.function;
	if (src.excluded_count > 0 && !init_excluded(func.excluded, src.excluded, src.excluded_count))
		return false;
	func.excluded_count = src.excluded_count;

	for (unsigned int i = 0; i < array_length(src.vars); i++) {
		if (src.vars[i] == 0) func.vars[i] = 0;
		else if (!init(func.vars[i], src.vars[i], std::forward<Args>(args)...))
			return false;
	}
	if (!init(func.arg, src.arg, std::forward<Args>(args)...)) {
		if (func.excluded_count > 0)
			free(func.excluded);
		return false;
	}
	return true;
}

inline bool init(datalog_predicate& pred) {
	pred.excluded_count = 0;
	for (unsigned int i = 0; i < array_length(pred.args); i++)
		pred.args[i] = NULL;
	return true;
}

template<typename... Args>
inline bool init(datalog_predicate& pred, const datalog_predicate& src, Args&&... args)
{
	pred.function = src.function;
	if (src.excluded_count > 0 && !init_excluded(pred.excluded, src.excluded, src.excluded_count))
		return false;
	pred.excluded_count = src.excluded_count;

	for (unsigned int i = 0; i < array_length(src.args); i++)
		pred.args[i] = NULL;
	for (unsigned int i = 0; i < array_length(src.args); i++) {
		if (src.args[i] != NULL
		 && !init(pred.args[i], src.args[i], std::forward<Args>(args)...)) {
			free(pred); return false;
		}
	}
	return true;
}

inline bool init(datalog_tuple& tuple, tuple_position position, unsigned int initial_capacity = 4) {
	tuple.position = position;
	return array_init(tuple.elements, initial_capacity);
}

template<typename... Args>
inline bool init(
		datalog_tuple& tuple, tuple_position position,
		datalog_expression* const* const expressions,
		unsigned int count, Args&&... args)
{
	tuple.position = position;
	return init(tuple.elements, expressions, count, std::forward<Args>(args)...);
}

template<typename... Args>
inline bool init(datalog_tuple& tuple, const datalog_tuple& src, Args&&... args) {
	tuple.position = src.position;
	return init(tuple.elements, src.elements.data, src.elements.length, std::forward<Args>(args)...);
}

template<unsigned int Capacity, tuple_position Position>
inline bool init_tuple(datalog_expression& exp) {
	if (!init(exp.tuple, Position, Capacity)) {
		fprintf(stderr, "init_tuple ERROR: Unable to initialize tuple.\n");
		return false;
	}
	exp.type = DATALOG_TUPLE;
	exp.reference_count = 1;
	return true;
}

template<unsigned int Capacity, tuple_position Position>
inline bool init_tuple(datalog_expression*& exp) {
	exp = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (exp == NULL) {
		fprintf(stderr, "init_tuple ERROR: Out of memory.\n");
		return false;
	} else if (!init_tuple<Capacity, Position>(*exp)) {
		free(exp);
		return false;
	}
	return true;
}

inline bool init_tuple(datalog_expression& exp, tuple_position position, unsigned int capacity) {
	if (!init(exp.tuple, position, max(1u, capacity))) {
		fprintf(stderr, "init_tuple ERROR: Unable to initialize tuple.\n");
		return false;
	}
	exp.type = DATALOG_TUPLE;
	exp.reference_count = 1;
	return true;
}

inline bool init_tuple(datalog_expression*& exp, tuple_position position, unsigned int capacity) {
	exp = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (exp == NULL) {
		fprintf(stderr, "init_tuple ERROR: Out of memory.\n");
		return false;
	} else if (!init_tuple(*exp, position, capacity)) {
		free(exp); return false;
	}
	return true;
}

inline bool init(datalog_list& list) {
	return array_init(list.elements, 8);
}

template<typename... Args>
inline bool init(datalog_list& list, const datalog_list& src, Args&&... args) {
	return init(list.elements, src.elements.data, src.elements.length, std::forward<Args>(args)...);
}

inline bool init(datalog_literal& literal, const datalog_literal& src) {
	literal.label = src.label;
	if (src.excluded_count > 0 && !init_excluded(literal.excluded, src.excluded, src.excluded_count))
		return false;
	literal.excluded_count = src.excluded_count;
	return true;
}

template<typename... Args>
inline bool datalog_expression::initialize(const datalog_expression& src, Args&&... args) {
	switch (src.type) {
	case DATALOG_PREDICATE:
		if (!init(pred, src.pred, std::forward<Args>(args)...)) return false;
		break;
	case DATALOG_FUNCTION:
		if (!init(func, src.func, std::forward<Args>(args)...)) return false;
		break;
	case DATALOG_TUPLE:
		if (!init(tuple, src.tuple, std::forward<Args>(args)...)) return false;
		break;
	case DATALOG_LIST:
		if (!init(list, src.list, std::forward<Args>(args)...)) return false;
		break;
	case DATALOG_VARIABLE:
		if (!init(variable, src.variable, std::forward<Args>(args)...)) return false;
		break;
	case DATALOG_CONSTANT:
		if (!init(constant, src.constant)) return false;
		break;
	case DATALOG_INTEGER:
		integer = src.integer;
		break;
	case DATALOG_STRING:
		if (!init(str, src.str)) return false;
		break;
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		break;
	}
	type = src.type;
	return true;
}

template<typename... Args>
bool init(datalog_expression& e, const datalog_expression& src, Args&&... args) {
	e.reference_count = 1;
	return e.initialize(src, std::forward<Args>(args)...);
}

template<typename... Args>
inline bool init(datalog_expression*& exp, const datalog_expression& src, Args&&... args)
{
	if (!new_expression(exp)) return false;
	else if (!init(*exp, src, std::forward<Args>(args)...)) {
		free(exp); return false;
	}
	return true;
}

inline void datalog_predicate::free(datalog_predicate& pred) {
	for (unsigned int i = 0; i < array_length(pred.args); i++) {
		if (pred.args[i] != NULL) {
			core::free(*pred.args[i]);
			if (pred.args[i]->reference_count == 0)
				core::free(pred.args[i]);
		}
	}
	if (pred.excluded_count > 0)
		core::free(pred.excluded);
}

inline void datalog_function::free(datalog_function& func) {
	if (func.arg != NULL) {
		core::free(*func.arg);
		if (func.arg->reference_count == 0)
			core::free(func.arg);
	}
	if (func.excluded_count > 0)
		core::free(func.excluded);
}

inline void datalog_tuple::free(datalog_tuple& tuple) {
	for (unsigned int i = 0; i < tuple.elements.length; i++) {
		core::free(*tuple.elements[i]);
		if (tuple.elements[i]->reference_count == 0)
			core::free(tuple.elements[i]);
	}
	core::free(tuple.elements);
}

inline void datalog_list::free(datalog_list& list)
{
	for (unsigned int i = 0; i < list.elements.length; i++) {
		core::free(*list.elements[i]);
		if (list.elements[i]->reference_count == 0)
			core::free(list.elements[i]);
	}
	core::free(list.elements);
}

inline void datalog_literal::free(datalog_literal& literal)
{
	if (literal.excluded_count > 0)
		core::free(literal.excluded);
}

template<int Offset = 0>
inline bool build_variable_map(unsigned int src,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map)
{
	if (!ensure_variable_map_capacity(src_variable_map, src)
	 || !ensure_variable_map_capacity(dst_variable_map, src + Offset))
		return false;
	else if (src_variable_map[src] != 0) return true;
	src_variable_map[src] = src + Offset;
	dst_variable_map[src + Offset] = 1;
	return true;
}

template<int Offset = 0>
inline bool build_variable_map(unsigned int variable,
		array_map<unsigned int, unsigned int>& variable_map)
{
	if (!variable_map.ensure_capacity(variable_map.size + 1)) return false;
	unsigned int index = variable_map.index_of(variable);
	if (index < variable_map.size) {
		return true;
	} else {
		variable_map.keys[index] = variable;
		variable_map.values[index] = variable_map.size + 1;
		variable_map.size++;
		return true;
	}
}

template<int Offset = 0, typename... Args>
bool build_variable_map(const datalog_expression& exp, Args&&... args)
{
	switch (exp.type) {
	case DATALOG_PREDICATE:
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) break;
			if (!build_variable_map<Offset>(*exp.pred.args[i], std::forward<Args>(args)...)) return false;
		}
		return true;
	case DATALOG_FUNCTION:
		for (unsigned int i = 0; i < array_length(exp.func.vars); i++) {
			if (exp.func.vars[i] == 0) break;
			if (!build_variable_map<Offset>(exp.func.vars[i], std::forward<Args>(args)...)) return false;
		}
		return build_variable_map<Offset>(*exp.func.arg, std::forward<Args>(args)...);
	case DATALOG_TUPLE:
		for (datalog_expression* arg : exp.tuple.elements)
			if (!build_variable_map<Offset>(*arg, std::forward<Args>(args)...)) return false;
		return true;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			if (!build_variable_map<Offset>(*arg, std::forward<Args>(args)...)) return false;
		return true;
	case DATALOG_VARIABLE:
		return build_variable_map<Offset>(exp.variable, std::forward<Args>(args)...);
	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "build_variable_map ERROR: Unrecognized expression type.\n");
	return false;
}

inline void apply_variable_map(unsigned int& variable, const array<unsigned int>& variable_map) {
	variable = variable_map[variable];
}

template<typename... Args>
void apply_variable_map(datalog_expression& exp, const Args&... variable_map)
{
	switch (exp.type) {
	case DATALOG_PREDICATE:
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) break;
			apply_variable_map(*exp.pred.args[i], variable_map...);
		}
		return;
	case DATALOG_FUNCTION:
		for (unsigned int i = 0; i < array_length(exp.func.vars); i++) {
			if (exp.func.vars[i] == 0) break;
			apply_variable_map(exp.func.vars[i], variable_map...);
		}
		apply_variable_map(*exp.func.arg, variable_map...);
		return;
	case DATALOG_TUPLE:
		for (datalog_expression* arg : exp.tuple.elements)
			apply_variable_map(*arg, variable_map...);
		return;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			apply_variable_map(*arg, variable_map...);
		return;
	case DATALOG_VARIABLE:
		apply_variable_map(exp.variable, variable_map...);
		return;
	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return;
	}
	fprintf(stderr, "apply_variable_map ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

inline void apply_variable_map(array<unsigned int>& src_variable_map, const array<unsigned int>& to_apply) {
	for (unsigned int i = 0; i < src_variable_map.capacity; i++)
		if (src_variable_map[i] != 0) apply_variable_map(src_variable_map[i], to_apply);
}

inline void compress_variable_map(array<unsigned int>& variable_map) {
	unsigned int sum = 0;
	for (unsigned int i = 1; i < variable_map.capacity; i++) {
		sum += variable_map[i];
		variable_map[i] = sum;
	}
}

template<unsigned int EmptyVariable, unsigned int AnyVariable, typename Function>
unsigned int map_variables(const datalog_expression& exp, Function func)
{
	unsigned int count = EmptyVariable;
	switch (exp.type) {
	case DATALOG_PREDICATE:
		if (exp.pred.args[0] != NULL)
			count = func(count, map_variables<EmptyVariable, AnyVariable>(*exp.pred.args[0], func));
		for (unsigned int i = 1; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) break;
			count = func(count, map_variables<EmptyVariable, AnyVariable>(*exp.pred.args[i], func));
		}
		return count;
	case DATALOG_FUNCTION:
		for (unsigned int i = 0; i < array_length(exp.func.vars); i++) {
			if (exp.func.vars[i] == 0) break;
			count = func(count, exp.func.vars[i]);
		}
		return func(count, map_variables<EmptyVariable, AnyVariable>(*exp.func.arg, func));
	case DATALOG_TUPLE:
		if (exp.tuple.position != POSITION_EXACT)
			count = func(count, AnyVariable);
		for (datalog_expression* arg : exp.tuple.elements)
			count = func(count, map_variables<EmptyVariable, AnyVariable>(*arg, func));
		return count;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			count = func(count, map_variables<EmptyVariable, AnyVariable>(*arg, func));
		return count;
	case DATALOG_VARIABLE:
		return func(count, exp.variable);
	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
		return func(count, EmptyVariable);
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return func(count, AnyVariable);
	}
	fprintf(stderr, "map_variables ERROR: Unrecognized expression type.\n");
	return false;
}

inline unsigned int choose_head(unsigned int first, unsigned int second) {
	if (first == 1 || second == 1) return 1;
	if (first == DATALOG_LABEL_WILDCARD || second == DATALOG_LABEL_WILDCARD)
		return DATALOG_LABEL_WILDCARD;
	return min(first, second);
}

inline unsigned int get_head(const datalog_expression& exp) {
	return map_variables<DATALOG_LABEL_EMPTY, DATALOG_LABEL_WILDCARD>(exp, choose_head);
}

inline unsigned int min_variable(const datalog_expression& exp) {
	static auto min_func = [] (unsigned int a, unsigned int b) {
		return min(a, b);
	};
	return map_variables<UINT_MAX, UINT_MAX>(exp, min_func);
}

inline unsigned int max_variable(const datalog_expression& exp) {
	static auto max_func = [] (unsigned int a, unsigned int b) {
		return max(a, b);
	};
	return map_variables<0, 0>(exp, max_func);
}

inline unsigned int variable_count(const datalog_expression& exp) {
	unsigned int max_var = max_variable(exp);
	if (max_var == 0) return 0;
	else return max_var - min_variable(exp) + 1;
}

bool is_ambiguous(const datalog_expression& exp)
{
	switch (exp.type) {
	case DATALOG_PREDICATE:
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) return true;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) break;
			if (is_ambiguous(*exp.pred.args[i])) return true;
		}
		return false;
	case DATALOG_FUNCTION:
		if (exp.func.function == DATALOG_LABEL_WILDCARD) return true;
		return is_ambiguous(*exp.func.arg);
	case DATALOG_TUPLE:
		if (exp.tuple.position != POSITION_EXACT) return true;
		for (datalog_expression* arg : exp.tuple.elements)
			if (is_ambiguous(*arg)) return true;
		return false;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			if (is_ambiguous(*arg)) return true;
		return false;
	case DATALOG_CONSTANT:
		return (exp.constant.label == DATALOG_LABEL_WILDCARD);
	case DATALOG_VARIABLE:
	case DATALOG_INTEGER:
	case DATALOG_EMPTY:
		return false;
	case DATALOG_STRING:
		return exp.str.length == 1 && exp.str[0] == DATALOG_LABEL_WILDCARD;
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "is_ambiguous ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

inline bool is_ambiguous(const datalog_expression_root& exp) {
	return is_ambiguous(exp.root);
}

bool valid_variable_scope(const datalog_expression& exp, array<unsigned int>& variables) {
	switch (exp.type) {
	case DATALOG_PREDICATE:
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) continue;
			if (!valid_variable_scope(*exp.pred.args[i], variables)) return false;
		}
		return true;
	case DATALOG_FUNCTION:
		if (exp.func.function == PREDICATE_COUNT || exp.func.function == PREDICATE_SUM) {
			if (!ensure_variable_map_capacity(variables, exp.func.vars[0])) exit(EXIT_FAILURE);
			if (variables[exp.func.vars[0]] == 1) {
				/* this variable appeared before the function */
				return false;
			}
		} else if (exp.func.function == PREDICATE_MOST || exp.func.function == PREDICATE_FEWEST) {
			if (!ensure_variable_map_capacity(variables, exp.func.vars[1])) exit(EXIT_FAILURE);
			if (variables[exp.func.vars[1]] == 1) {
				/* this variable appeared before the function */
				return false;
			}
		}

		if (!valid_variable_scope(*exp.func.arg, variables)) return false;

		if (exp.func.function == PREDICATE_COUNT || exp.func.function == PREDICATE_SUM)
			variables[exp.func.vars[0]] = 2;
		else if (exp.func.function == PREDICATE_MOST || exp.func.function == PREDICATE_FEWEST)
			variables[exp.func.vars[1]] = 2;
		return true;
	case DATALOG_TUPLE:
		for (datalog_expression* arg : exp.tuple.elements)
			if (!valid_variable_scope(*arg, variables)) return false;
		return true;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			if (!valid_variable_scope(*arg, variables)) return false;
		return true;
	case DATALOG_VARIABLE:
		if (!ensure_variable_map_capacity(variables, exp.variable)) exit(EXIT_FAILURE);
		if (variables[exp.variable] == 0) {
			/* this is a new variable */
			variables[exp.variable] = 1;
		} else if (variables[exp.variable] == 2) {
			/* this variable is out of scope */
			return false;
		}
		return true;
	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "valid_variable_scope ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

inline bool valid_variable_scope(const datalog_expression& exp) {
	array<unsigned int> variables = array<unsigned int>(8);
	memset(variables.data, 0, sizeof(unsigned int) * variables.capacity);
	return valid_variable_scope(exp, variables);
}

template<unsigned int Predicate>
bool has_predicate(const datalog_expression& exp) {
	switch (exp.type) {
	case DATALOG_PREDICATE:
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (!exp.pred.is_excluded(Predicate)) return true;
		} else if (exp.pred.function == Predicate)
			return true;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) continue;
			if (has_predicate<Predicate>(*exp.pred.args[i]))
				return true;
		}
		return false;
	case DATALOG_FUNCTION:
		return has_predicate<Predicate>(*exp.func.arg);
	case DATALOG_TUPLE:
		for (datalog_expression* arg : exp.tuple.elements)
			if (has_predicate<Predicate>(*arg)) return true;
		return false;
	case DATALOG_LIST:
		for (datalog_expression* arg : exp.list.elements)
			if (has_predicate<Predicate>(*arg)) return true;
		return false;
	case DATALOG_VARIABLE:
	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
		return false;
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "has_predicate ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

constexpr bool operator == (
		const datalog_expression_root::function& first,
		const datalog_expression_root::function& second)
{
	return first.type == second.type;
}

constexpr bool operator != (
		const datalog_expression_root::function& first,
		const datalog_expression_root::function& second)
{
	return first.type != second.type;
}

constexpr bool operator < (
		const datalog_expression_root::function& first,
		const datalog_expression_root::function& second)
{
	return first.type < second.type;
}

inline bool operator == (const datalog_literal& first, const datalog_literal& second) {
	if (first.label != second.label
	 || first.excluded_count != second.excluded_count)
		return false;
	for (unsigned int i = 0; i < first.excluded_count; i++)
		if (first.excluded[i] != second.excluded[i]) return false;
	return true;
}

bool operator == (const datalog_expression& first, const datalog_expression& second)
{
	if (first.type != second.type) return false;
	switch (first.type) {
	case DATALOG_PREDICATE:
		if (first.pred.function != second.pred.function
		 || first.pred.excluded_count != second.pred.excluded_count)
			return false;
		for (unsigned int i = 0; i < first.pred.excluded_count; i++)
			if (first.pred.excluded[i] != second.pred.excluded[i]) return false;
		for (unsigned int i = 0; i < array_length(first.pred.args); i++) {
			if (first.pred.args[i] == NULL) {
				if (second.pred.args[i] == NULL) continue;
				else return false;
			} else if (second.pred.args[i] == NULL) {
				return false;
			} else if (first.pred.args[i] != second.pred.args[i]
					&& *first.pred.args[i] != *second.pred.args[i])
				return false;
		}
		return true;
	case DATALOG_FUNCTION:
		if (first.func.function != second.func.function
		 || first.func.excluded_count != second.func.excluded_count)
			return false;
		for (unsigned int i = 0; i < first.func.excluded_count; i++)
			if (first.func.excluded[i] != second.func.excluded[i]) return false;
		if (first.func.arg == NULL) {
			if (second.func.arg == NULL) return true;
			else return false;
		} else if (second.func.arg == NULL) {
				return false;
		} else if (first.func.arg != second.func.arg
				&& *first.func.arg != *second.func.arg)
			return false;
		return true;
	case DATALOG_TUPLE:
		if (first.tuple.position != second.tuple.position
		 || first.tuple.elements.length != second.tuple.elements.length)
			return false;
		for (unsigned int i = 0; i < first.tuple.elements.length; i++)
			if (first.tuple.elements[i] != second.tuple.elements[i]
			 && *first.tuple.elements[i] != *second.tuple.elements[i])
				return false;
		return true;
	case DATALOG_LIST:
		if (first.list.elements.length != second.list.elements.length)
			return false;
		for (unsigned int i = 0; i < first.list.elements.length; i++)
			if (first.list.elements[i] != second.list.elements[i]
			 && *first.list.elements[i] != *second.list.elements[i])
				return false;
		return true;
	case DATALOG_VARIABLE:
		return first.variable == second.variable;
	case DATALOG_CONSTANT:
		return first.constant == second.constant;
	case DATALOG_INTEGER:
		return first.integer == second.integer;
	case DATALOG_STRING:
		return first.str == second.str;
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "datalog_expression operator == ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

inline bool operator != (const datalog_expression& first, const datalog_expression& second) {
	return !(first == second);
}

inline bool operator == (const datalog_expression_root& first, const datalog_expression_root& second) {
	return first.index == second.index && first.concord == second.concord
		&& first.inf == second.inf && first.root == second.root;
}

inline bool operator != (const datalog_expression_root& first, const datalog_expression_root& second) {
	return first.index != second.index || first.concord != second.concord
		|| first.inf != second.inf || first.root != second.root;
}

/* forward declarations */

bool canonicalize(const datalog_expression& src, datalog_expression& dst,
		const array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope);

struct canonicalizer { };

inline bool less_than(
		const datalog_expression& first,
		const datalog_expression& second,
		const canonicalizer& sorter)
{
	if (first.type < second.type) return true;
	else if (first.type > second.type) return false;
	switch (first.type) {
	case DATALOG_PREDICATE:
		if (first.pred.function < second.pred.function) return true;
		else if (first.pred.function > second.pred.function) return false;
		else if (first.pred.excluded_count < second.pred.excluded_count) return true;
		else if (first.pred.excluded_count > second.pred.excluded_count) return false;
		for (unsigned int i = 0; i < first.pred.excluded_count; i++) {
			if (first.pred.excluded[i] < second.pred.excluded[i]) return true;
			else if (first.pred.excluded[i] > second.pred.excluded[i]) return false;
		} for (unsigned int i = 0; i < array_length(first.pred.args); i++) {
			if (first.pred.args[i] == NULL) {
				if (second.pred.args[i] == NULL) {
					continue;
				} else {
					return true;
				}
			} else {
				if (second.pred.args[i] == NULL) {
					return false;
				} else if (less_than(*first.pred.args[i], *second.pred.args[i], sorter)) {
					return true;
				} else if (less_than(*second.pred.args[i], *first.pred.args[i], sorter)) {
					return false;
				}
			}
		}
		return false;
	case DATALOG_FUNCTION:
		if (first.func.function < second.func.function) return true;
		else if (first.func.function > second.func.function) return false;
		else if (first.func.excluded_count < second.func.excluded_count) return true;
		else if (first.func.excluded_count > second.func.excluded_count) return false;
		else if (first.func.arg == NULL) {
			if (second.func.arg == NULL) {
				return false;
			} else {
				return true;
			}
		} else {
			if (second.func.arg == NULL) {
				return true;
			} else {
				return less_than(*first.func.arg, *second.func.arg, sorter);
			}
		}
	case DATALOG_TUPLE:
		if (first.tuple.position < second.tuple.position) return true;
		else if (first.tuple.position > second.tuple.position) return false;
		else if (first.tuple.elements.length < second.tuple.elements.length) return true;
		else if (first.tuple.elements.length > second.tuple.elements.length) return false;
		for (unsigned int i = 0; i < first.tuple.elements.length; i++) {
			if (less_than(*first.tuple.elements[i], *second.tuple.elements[i], sorter)) {
				return true;
			} else if (less_than(*second.tuple.elements[i], *first.tuple.elements[i], sorter)) {
				return false;
			}
		}
		return false;
	case DATALOG_LIST:
		if (first.list.elements.length < second.list.elements.length) return true;
		else if (first.list.elements.length > second.list.elements.length) return false;
		for (unsigned int i = 0; i < first.list.elements.length; i++) {
			if (less_than(*first.list.elements[i], *second.list.elements[i], sorter)) {
				return true;
			} else if (less_than(*second.list.elements[i], *first.list.elements[i], sorter)) {
				return false;
			}
		}
		return false;
	case DATALOG_CONSTANT:
		return first.constant < second.constant;
	case DATALOG_INTEGER:
		return first.integer < second.integer;
	case DATALOG_STRING:
		return first.str < second.str;
	case DATALOG_VARIABLE:
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "less_than ERROR: Unrecognized expression type during canonicalization.\n");
	exit(EXIT_FAILURE);
}

inline bool less_than(
		const datalog_expression* first,
		const datalog_expression* second,
		const canonicalizer& sorter)
{
	return less_than(*first, *second, sorter);
}

struct datalog_scope {
	const datalog_expression* exp;
	array<unsigned int>* variables;
};

inline bool process_maximal_scope(const datalog_expression* child_expression,
		const array_map<unsigned int, datalog_scope>& available_scopes,
		array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope,
		array<const datalog_expression*>& root_scope, const array<unsigned int>& child_variables)
{
	datalog_scope* scope = NULL;
	for (unsigned int i = available_scopes.size; i > 0; i--) {
		if (child_variables.contains(available_scopes.keys[i - 1])) {
			scope = &available_scopes.values[i - 1];
			break;
		}
	}

	if (scope == NULL) {
		return root_scope.add(child_expression);
	} else {
		array<unsigned int>& variables = *scope->variables;
		if (!variables.ensure_capacity(variables.length + child_variables.length))
			return false;
		for (unsigned int child_variable : child_variables) {
			if (variables.contains(child_variable)) continue;
			variables[variables.length] = child_variable;
			variables.length++;
		}
		return maximal_scope.get(scope->exp).add(child_expression);
	}
}

bool compute_maximal_scope(const datalog_expression& src,
		array_map<unsigned int, datalog_scope>& available_scopes,
		array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope,
		array<const datalog_expression*>& root_scope, array<unsigned int>& variables)
{
	unsigned int old_maximal_scope_count;
	unsigned int old_available_scope_count = available_scopes.size;
	array<unsigned int> child_variables = array<unsigned int>(8);
	array_map<unsigned int, datalog_scope> child_scopes = array_map<unsigned int, datalog_scope>(8);
	array<const datalog_expression*> child_root_scope = array<const datalog_expression*>(8);

	switch (src.type) {
	case DATALOG_PREDICATE:
		for (unsigned int i = 0; i < array_length(src.pred.args); i++) {
			if (src.pred.args[i] == NULL) continue;

			/* arguments of predicates can't be moved outside */
			if (!maximal_scope.ensure_capacity(maximal_scope.size + 1))
				return false;
			old_maximal_scope_count = maximal_scope.size;
			maximal_scope.keys[old_maximal_scope_count] = src.pred.args[i];
			if (!array_init(maximal_scope.values[old_maximal_scope_count], 4))
				return false;
			maximal_scope.size++;

			/* recursively compute the variables of the child expression */
			if (!compute_maximal_scope(*src.pred.args[i], child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !process_maximal_scope(src.pred.args[i], child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !maximal_scope.values[old_maximal_scope_count].append(child_root_scope.data, child_root_scope.length)
			 || !variables.append(child_variables.data, child_variables.length))
				return false;
			child_scopes.clear(); child_root_scope.clear(); child_variables.clear();
		}
		return true;

	case DATALOG_FUNCTION:
		if (!maximal_scope.ensure_capacity(maximal_scope.size + 1)
		 || !available_scopes.ensure_capacity(available_scopes.size + array_length(src.func.vars)))
			return false;

		for (unsigned int i = 0; i < array_length(src.func.vars); i++) {
			if (src.func.vars[i] == 0) continue;
			else if (!variables.add(src.func.vars[i]))
				return false;
			available_scopes.keys[available_scopes.size] = src.func.vars[i];
			available_scopes.values[available_scopes.size] = {src.func.arg, &variables};
			available_scopes.size++;
		}

		old_maximal_scope_count = maximal_scope.size;
		maximal_scope.keys[old_maximal_scope_count] = src.func.arg;
		if (!array_init(maximal_scope.values[old_maximal_scope_count], 4))
			return false;
		maximal_scope.size++;

		if (src.func.function == PREDICATE_ANSWER || src.func.function == PREDICATE_NOT) {
			/* negation and lambda terms block movement */
			for (unsigned int i = 0; i < array_length(src.func.vars); i++) {
				if (src.func.vars[i] == 0) continue;
				child_scopes.keys[i] = src.func.vars[i];
				child_scopes.values[i] = {src.func.arg, &variables};
				child_scopes.size++;
			}

			/* recursively compute the variables of the child expression */
			if (!compute_maximal_scope(*src.func.arg, child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !process_maximal_scope(src.func.arg, child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !maximal_scope.values[old_maximal_scope_count].append(child_root_scope.data, child_root_scope.length))
				return false;

		} else {
			/* recursively compute the variables of the child expression */
			if (!compute_maximal_scope(*src.func.arg, available_scopes, maximal_scope, root_scope, child_variables)
			 || !process_maximal_scope(src.func.arg, available_scopes, maximal_scope, root_scope, child_variables))
				return false;
		}
		available_scopes.size = old_available_scope_count;
		return true;

	case DATALOG_TUPLE:
		for (unsigned int i = 0; i < src.tuple.elements.length; i++) {
			if (!compute_maximal_scope(*src.tuple.elements[i], available_scopes, maximal_scope, root_scope, child_variables)
			 || !process_maximal_scope(src.tuple.elements[i], available_scopes, maximal_scope, root_scope, child_variables))
				return false;
			child_variables.clear();
		}

		/* since all elements of a tuple are "moved" to a higher scope,
		   the tuple becomes vacuous, so don't mark it for movement */
		return true;

	case DATALOG_LIST:
		/* lists block all movement */
		for (unsigned int i = 0; i < src.tuple.elements.length; i++) {
			if (maximal_scope.ensure_capacity(maximal_scope.size + 1))
				return false;
			old_maximal_scope_count = maximal_scope.size;
			maximal_scope.keys[old_maximal_scope_count] = src.tuple.elements[i];
			if (!array_init(maximal_scope.values[old_maximal_scope_count], 4))
				return false;
			maximal_scope.size++;

			/* recursively compute the variables of the child expression */
			if (!compute_maximal_scope(*src.tuple.elements[i], child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !process_maximal_scope(src.tuple.elements[i], child_scopes, maximal_scope, child_root_scope, child_variables)
			 || !maximal_scope.values[old_maximal_scope_count].append(child_root_scope.data, child_root_scope.length))
				return false;
			child_scopes.clear(); child_root_scope.clear(); child_variables.clear();
		}
		return true;

	case DATALOG_VARIABLE:
		return variables.add(src.variable);

	case DATALOG_CONSTANT:
	case DATALOG_INTEGER:
	case DATALOG_STRING:
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return true;
	}
	fprintf(stderr, "compute_maximal_scope ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

inline bool compute_maximal_scope(const datalog_expression& src,
		array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope)
{
	array<unsigned int> variables = array<unsigned int>(8);
	array_map<unsigned int, datalog_scope> available_scopes = array_map<unsigned int, datalog_scope>(8);
	array<const datalog_expression*> root_scope = array<const datalog_expression*>(8);
	return compute_maximal_scope(src, available_scopes, maximal_scope, root_scope, variables);
}

inline bool canonicalize_scope(const datalog_expression* src_scope, datalog_expression& dst,
		const array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope)
{
	const array<const datalog_expression*>& moved_expressions = maximal_scope.get(src_scope);

	tuple_position position = (src_scope->type == DATALOG_TUPLE) ? src_scope->tuple.position : POSITION_EXACT;
	if (position == POSITION_RIGHT) position = POSITION_LEFT;

	if (moved_expressions.length == 1 && position == POSITION_EXACT) {
		return canonicalize(*moved_expressions[0], dst, maximal_scope);
	} else {
		if (!init_tuple(dst, position, moved_expressions.length))
			return false;
		for (unsigned int i = 0; i < moved_expressions.length; i++) {
			if (!new_expression(dst.tuple.elements[i])) {
				free(dst); return false;
			} else if (!canonicalize(*moved_expressions[i], *dst.tuple.elements[i], maximal_scope)) {
				free(dst.tuple.elements[i]);
				free(dst); return false;
			}
			dst.tuple.elements.length++;
		}

		/* remove any vacuous elements */
		for (unsigned int i = 0; i < dst.tuple.elements.length; i++) {
			if (dst.tuple.elements[i]->type == DATALOG_TUPLE && dst.tuple.elements[i]->tuple.elements.length == 0) {
				if (dst.tuple.elements[i]->tuple.position != POSITION_EXACT)
					dst.tuple.position = POSITION_LEFT;

				free(*dst.tuple.elements[i]);
				free(dst.tuple.elements[i]);
				dst.tuple.elements.remove(i); i--;
			}
		}

		if (dst.tuple.elements.length > 1) {
			/* sort the tuple elements */
			sort(dst.tuple.elements, canonicalizer());

			/* remove duplicate elements */
			unsigned int dst_index = 0;
			for (unsigned int i = 1; i < dst.tuple.elements.length; i++) {
				if (dst.tuple.elements[dst_index] != dst.tuple.elements[i]) {
					dst.tuple.elements[++dst_index] = dst.tuple.elements[i];
				} else {
					free(*dst.tuple.elements[i]);
					free(dst.tuple.elements[i]);
				}
			}
			dst.tuple.elements.length = dst_index + 1;
		}

		/* if only one element remains, simplify the tuple */
		if (dst.tuple.elements.length == 1 && dst.tuple.position == POSITION_EXACT) {
			datalog_expression* singleton = dst.tuple.elements[0];
			singleton->reference_count++;
			free(dst); dst = *singleton; free(*singleton);
			if (singleton->reference_count == 0)
				free(singleton);
		}
		return true;
	}
	return true;
}

bool canonicalize(const datalog_predicate& pred, datalog_expression& dst,
		const array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope)
{
	dst.pred.function = pred.function;
	if (pred.excluded_count > 0 && !init_excluded(dst.pred.excluded, pred.excluded, pred.excluded_count))
		return false;
	dst.pred.excluded_count = pred.excluded_count;
	for (unsigned int i = 0; i < array_length(pred.args); i++)
		dst.pred.args[i] = NULL;
	for (unsigned int i = 0; i < array_length(pred.args); i++) {
		if (pred.args[i] == NULL) {
			continue;
		} else if (!new_expression(dst.pred.args[i])) {
			free(dst.pred); return false;
		} else if (!canonicalize_scope(pred.args[i], *dst.pred.args[i], maximal_scope)) {
			free(dst.pred.args[i]);
			dst.pred.args[i] = NULL;
			free(dst.pred); return false;
		}
	}
	dst.type = DATALOG_PREDICATE;
	dst.reference_count = 1;
	return true;
}

bool canonicalize(const datalog_expression& src, datalog_expression& dst,
		const array_map<const datalog_expression*, array<const datalog_expression*>>& maximal_scope)
{
	switch (src.type) {
	case DATALOG_PREDICATE:
		return canonicalize(src.pred, dst, maximal_scope);
	case DATALOG_FUNCTION:
		if (!new_expression(dst.func.arg)) return false;
		dst.func.function = src.func.function;
		if (src.func.excluded_count > 0 && !init_excluded(dst.func.excluded, src.func.excluded, src.func.excluded_count)) {
			free(dst.func.arg); return false;
		}
		dst.func.excluded_count = src.func.excluded_count;
		dst.type = DATALOG_FUNCTION;
		dst.reference_count = 1;
		for (unsigned int i = 0; i < array_length(src.func.vars); i++)
			dst.func.vars[i] = src.func.vars[i];
		return canonicalize_scope(src.func.arg, *dst.func.arg, maximal_scope);
	case DATALOG_TUPLE:
		return init_tuple(dst, src.tuple.position, 1);
	case DATALOG_LIST:
		if (!array_init(dst.list.elements, src.list.elements.length)) return false;
		dst.type = DATALOG_LIST;
		dst.reference_count = 1;
		for (unsigned int i = 0; i < src.list.elements.length; i++) {
			if (!new_expression(dst.list.elements[i])) {
				free(dst); return false;
			} else if (!canonicalize_scope(src.list.elements[i], *dst.list.elements[i], maximal_scope)) {
				free(dst.list.elements[i]);
				free(dst); return false;
			}
			dst.list.elements.length++;
		}
		return true;
	case DATALOG_CONSTANT:
		if (!init(dst.constant, src.constant)) return false;
		dst.type = DATALOG_CONSTANT;
		dst.reference_count = 1;
		return true;
	case DATALOG_INTEGER:
		dst.integer = src.integer;
		dst.type = DATALOG_INTEGER;
		dst.reference_count = 1;
		return true;
	case DATALOG_STRING:
		dst.str = src.str;
		dst.type = DATALOG_STRING;
		dst.reference_count = 1;
		return true;
	case DATALOG_VARIABLE:
		dst.variable = src.variable;
		dst.type = DATALOG_VARIABLE;
		dst.reference_count = 1;
		return true;
	case DATALOG_EMPTY:
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		dst.type = src.type;
		dst.reference_count = 1;
		return true;
	}
	fprintf(stderr, "canonicalize ERROR: Unrecognized expression type.\n");
	exit(EXIT_FAILURE);
}

template<typename K, typename V>
void free_values(array_map<K, V>& map) {
	for (auto entry : map)
		free(entry.value);
}

bool equivalent(const datalog_expression& first, const datalog_expression& second)
{
	array_map<const datalog_expression*, array<const datalog_expression*>> first_maximal_scope(8), second_maximal_scope(8);
	if (!compute_maximal_scope(first, first_maximal_scope)
	 || !compute_maximal_scope(second, second_maximal_scope))
	{
		free_values(first_maximal_scope);
		free_values(second_maximal_scope);
		return false;
	}

	/* canonicalize the logical form structure (modulo variables) */
	datalog_expression first_canonical;
	datalog_expression second_canonical;
	if (!canonicalize(first, first_canonical, first_maximal_scope)
	 || !canonicalize(second, second_canonical, second_maximal_scope))
	{
		free_values(first_maximal_scope);
		free_values(second_maximal_scope);
		free(first_canonical); exit(EXIT_FAILURE);
	}
	free_values(first_maximal_scope);
	free_values(second_maximal_scope);

	/* relabel variables in prefix order */
	datalog_expression first_relabeled;
	datalog_expression second_relabeled;
	array_map<unsigned int, unsigned int> first_variable_map = array_map<unsigned int, unsigned int>(8);
	array_map<unsigned int, unsigned int> second_variable_map = array_map<unsigned int, unsigned int>(8);
	if (!build_variable_map(first_canonical, first_variable_map)
	 || !build_variable_map(second_canonical, second_variable_map)
	 || !init(first_relabeled, first_canonical, first_variable_map)
	 || !init(second_relabeled, second_canonical, second_variable_map)) {
		free(first_canonical); free(second_canonical);
		free(first_relabeled); exit(EXIT_FAILURE);
	}
	free(first_canonical); free(second_canonical);

	bool are_equivalent = (first_relabeled == second_relabeled);
	free(first_relabeled); free(second_relabeled);
	return are_equivalent;
}

inline bool equivalent(const datalog_expression_root& first, const datalog_expression_root& second)
{
	return first.index == second.index && first.concord == second.concord
		&& first.inf == second.inf && equivalent(first.root, second.root);
}

template<typename Stream>
inline bool print_variable(unsigned int variable, Stream& out) {
	if (variable == 0) return print('0', out);
	else return print((char) ('A' + variable - 1), out);
}

template<typename Stream, typename... Printer>
bool print(const datalog_function& func, Stream& out, Printer&&... printer) {
	if (func.function == DATALOG_LABEL_WILDCARD) {
		return print("*(", out) && print(*func.arg, out, std::forward<Printer>(printer)...) && print(')', out);
	}
	if (func.function != PREDICATE_NOT && (!print(func.function, out, std::forward<Printer>(printer)...) || !print('(', out)))
		return false;
	switch (func.function) {
	case PREDICATE_COUNT:
	case PREDICATE_SUM:
		if (!print_variable(func.vars[0], out)) return false;
		if (!print(',', out) || !print(*func.arg, out, std::forward<Printer>(printer)...) || !print(',', out))
			return false;
		return print_variable(func.vars[1], out) && print(')', out);

	case PREDICATE_ANSWER:
	case PREDICATE_HIGHEST:
	case PREDICATE_LOWEST:
	case PREDICATE_LONGEST:
	case PREDICATE_SHORTEST:
	case PREDICATE_LARGEST:
	case PREDICATE_SMALLEST:
		if (!print_variable(func.vars[0], out)) return false;
		return print(',', out) && print(*func.arg, out, std::forward<Printer>(printer)...) && print(')', out);

	case PREDICATE_MOST:
	case PREDICATE_FEWEST:
		if (!print_variable(func.vars[0], out) || !print(',', out) || !print_variable(func.vars[1], out))
			return false;
		return print(',', out) && print(*func.arg, out, std::forward<Printer>(printer)...) && print(')', out);

	case PREDICATE_NOT:
		return print("\\+", out) && print(*func.arg, out, std::forward<Printer>(printer)...);

	default:
		fprintf(stderr, "print ERROR: Unrecognized datalog_function predicate.\n");
		return false;
	}
}

template<typename Stream, typename... Printer>
bool print(const datalog_tuple& tuple, Stream& out, Printer&&... printer) {
	if (tuple.elements.length == 0) {
		if (tuple.position == POSITION_EXACT) return print("()", out);
		else return print("(...)", out);
	}
	if (!print('(', out)) return false;
	if (tuple.position == POSITION_RIGHT && !print("...,", out)) return false;
	if (!print(*tuple.elements[0], out, std::forward<Printer>(printer)...)) return false;
	for (unsigned int i = 1; i < tuple.elements.length; i++) {
		if (!print(',', out)
		 || !print(*tuple.elements[i], out, std::forward<Printer>(printer)...)) return false;
	}
	if (tuple.position == POSITION_LEFT && !print(",...", out)) return false;
	return print(')', out);
}

template<typename Stream, typename... Printer>
bool print(const datalog_expression& exp, Stream& out, Printer&&... printer) {
	switch (exp.type) {
	case DATALOG_PREDICATE:
		if (!print(exp.pred.function, out, std::forward<Printer>(printer)...) || !print('(', out))
			return false;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) break;
			if (i > 0 && !print(',', out)) return false;
			if (!print(*exp.pred.args[i], out, std::forward<Printer>(printer)...))
				return false;
		}
		return print(')', out);
	case DATALOG_FUNCTION:
		return print(exp.func, out, std::forward<Printer>(printer)...);
	case DATALOG_TUPLE:
		return print(exp.tuple, out, std::forward<Printer>(printer)...);
	case DATALOG_LIST:
		if (!print('[', out)) return false;
		for (unsigned int i = 0; i < exp.tuple.elements.length; i++) {
			if (i > 0 && !print(',', out)) return false;
			if (!print(*exp.tuple.elements[i], out, std::forward<Printer>(printer)...)) return false;
		}
		return print(']', out);
	case DATALOG_VARIABLE:
		return print_variable(exp.variable, out);
	case DATALOG_CONSTANT:
		return print(exp.constant.label, out, std::forward<Printer>(printer)...);
	case DATALOG_INTEGER:
		return print(exp.integer, out);
	case DATALOG_STRING:
		if (exp.str.length == 1 && exp.str[0] == DATALOG_LABEL_WILDCARD)
			return print("<any string>", out);
		return print('"', out) && print(exp.str, out, std::forward<Printer>(printer)...) && print('"', out);
	case DATALOG_EMPTY:
		return print("<empty>", out);
	case DATALOG_ANY:
	case DATALOG_NON_EMPTY:
		return print('*', out);
	}
	fprintf(stderr, "print ERROR: Unrecognized datalog_expression type.\n");
	exit(EXIT_FAILURE);
}

template<typename Stream, typename... Printer>
bool print(const datalog_expression_root& exp, Stream& out, Printer&&... printer) {
	if (!print(exp.root, out, std::forward<Printer>(printer)...)) return false;

	unsigned int index = 0;
	if (exp.index != NUMBER_ALL) {
		if (index == 0 && !print('[', out)) return false;
		if (!print("index:", out) || !print(exp.index, out)) return false;
		index++;
	} if (exp.concord != NUMBER_NONE) {
		if (index == 0 && !print('[', out)) return false;
		if (index != 0 && !print(',', out)) return false;
		if (!print("concord:", out) || !print(exp.concord, out)) return false;
		index++;
	} if (exp.inf != INFLECTION_NONE) {
		if (index == 0 && !print('[', out)) return false;
		if (index != 0 && !print(',', out)) return false;
		if (!print(exp.inf, out)) return false;
		index++;
	}
	return (index == 0 || print(']', out));
}

bool datalog_interpret_args(
	const array<datalog_token>& tokens,
	unsigned int& index,
	array<datalog_expression*>& args,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	while (true) {
		if (!args.ensure_capacity(args.length + 1)) return false;

		datalog_expression*& next_arg = args[(unsigned int) args.length];
		if (!new_expression(next_arg)
		 || !datalog_interpret_expression(tokens, index, *next_arg, names, variables)) {
			if (next_arg != NULL) free(next_arg);
			return false;
		}
		args.length++;

		if (next_arg->type == DATALOG_EMPTY) {
			free(next_arg);
			next_arg = NULL;
		}

		if (index >= tokens.length) {
			fprintf(stderr, "ERROR: Unexpected end of input.\n");
			return false;
		} else if (tokens[index].type != DATALOG_TOKEN_COMMA) {
			/* this is the end of the list, so return */
			return true;
		}
		index++;
	}
}

template<datalog_token_type ClosingSymbol, const char* SymbolDescription>
bool datalog_interpret_comma_list(
	const array<datalog_token>& tokens,
	unsigned int& index,
	array<datalog_expression*>& args,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	if (tokens[index].type == ClosingSymbol) {
		/* this function has no arguments */
		index++;
		return true;
	} else {
		if (!datalog_interpret_args(tokens, index, args, names, variables))
			return false;
		if (!expect_token(tokens, index, ClosingSymbol, SymbolDescription))
			return false;
		index++;
		return true;
	}
}

char bracket_description[] = "closing bracket in list";
char parenthesis_description[] = "closing parenthesis in function call";

inline bool datalog_interpret_list(
	const array<datalog_token>& tokens,
	unsigned int& index,
	array<datalog_expression*>& args,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	return datalog_interpret_comma_list
		<DATALOG_TOKEN_RBRACKET, bracket_description>(tokens, index, args, names, variables);
}

inline bool datalog_interpret_tuple(
	const array<datalog_token>& tokens,
	unsigned int& index,
	array<datalog_expression*>& args,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	return datalog_interpret_comma_list
		<DATALOG_TOKEN_RPAREN, parenthesis_description>(tokens, index, args, names, variables);
}

bool datalog_interpret_function(
		unsigned int predicate, datalog_expression& exp,
		array<datalog_expression*>& args, const position& pos)
{
	if (!init(exp.func)) return false;
	exp.func.function = predicate;
	exp.type = DATALOG_FUNCTION;
	unsigned int var_index = 0;
	for (datalog_expression* arg : args) {
		switch (arg->type) {
		case DATALOG_VARIABLE:
			if (var_index == array_length(exp.func.vars)) {
				read_error("Too many variable arguments to function", pos);
				free(exp.func); return false;
			}
			exp.func.vars[var_index] = arg->variable;
			var_index++;
			break;
		default:
			if (exp.func.arg != NULL) {
				read_error("Too many non-variable arguments to function", pos);
				free(exp.func); return false;
			}
			exp.func.arg = arg;
			arg->reference_count++;
			break;
		}
	}

	for (; var_index < array_length(exp.func.vars); var_index++)
		exp.func.vars[var_index] = 0;
	return true;
}

bool datalog_interpret_predicate(
		unsigned int predicate, datalog_expression& exp,
		array<datalog_expression*>& args, const position& pos)
{
	if (!init(exp.pred)) return false;
	exp.pred.function = predicate;
	exp.type = DATALOG_PREDICATE;
	unsigned int index = 0;
	for (datalog_expression* arg : args) {
		if (index == array_length(exp.pred.args)) {
			read_error("Too many arguments to predicate instance", pos);
			free(exp.pred); return false;
		}
		exp.pred.args[index] = arg;
		if (arg != NULL)
			arg->reference_count++;
		index++;
	}
	return true;
}

bool datalog_interpret_expression(
	const array<datalog_token>& tokens,
	unsigned int& index,
	datalog_expression& exp,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	if (index >= tokens.length) {
		fprintf(stderr, "ERROR: Unexpected end of input.\n");
		return false;
	} else if (tokens[index].type == DATALOG_TOKEN_IDENTIFIER || tokens[index].type == DATALOG_TOKEN_STRING) {
		const datalog_token& identifier = tokens[index];
		index++;
		if (index >= tokens.length) {
			fprintf(stderr, "ERROR: Unexpected end of input.\n");
			return false;
		} else if (tokens[index].type == DATALOG_TOKEN_LPAREN) {
			/* this is a higher-order function or predicate instance */
			index++;
			exp.reference_count = 1;

			unsigned int predicate;
			if (!get_token(identifier.text, predicate, names)) {
				free(exp); return false;
			}

			/* parse the function/predicate arguments */
			array<datalog_expression*> args = array<datalog_expression*>(4);
			if (!datalog_interpret_tuple(tokens, index, args, names, variables)) {
				for (datalog_expression* arg : args) {
					free(*arg); free(arg);
				}
				free(exp); return false;
			}

			bool failed = (predicate <= NUM_PREDICATES && !datalog_interpret_function(predicate, exp, args, identifier.end))
					   || (predicate > NUM_PREDICATES && !datalog_interpret_predicate(predicate, exp, args, identifier.end));
			for (datalog_expression* arg : args) {
				if (arg == NULL) continue;
				free(*arg);
				if (arg->reference_count == 0)
					free(arg);
			}
			return !failed;
		} else {
			exp.reference_count = 1;
			if (identifier.type == DATALOG_TOKEN_STRING) {
				/* this is a constant */
				exp.type = DATALOG_CONSTANT;
				if (!get_token(identifier.text, exp.constant.label, names))
					return false;
				exp.constant.excluded_count = 0;
			} else if ((identifier.text.length == 1 && identifier.text[0] >= 'A' && identifier.text[0] <= 'Z')
			 || (identifier.text.length == 2 && identifier.text[0] >= 'A' && identifier.text[0] <= 'Z' && isdigit(identifier.text[1]))) {
				/* this is a variable */
				exp.type = DATALOG_VARIABLE;
				if (!get_token(identifier.text, exp.variable, variables))
					return false;
			} else if (identifier.text.length > 1 && identifier.text[0] == '_') {
				/* this is also a variable */
				exp.type = DATALOG_VARIABLE;
				if (!get_token(identifier.text, exp.variable, variables))
					return false;
			} else if (identifier.text.length == 1 && identifier.text[0] == '_') {
				exp.type = DATALOG_EMPTY;
				return true;
			} else  {
				if (parse_int(identifier.text, exp.integer)) {
					/* this is an integer */
					exp.type = DATALOG_INTEGER;
				} else {
					/* this is a constant */
					exp.type = DATALOG_CONSTANT;
					if (!get_token(identifier.text, exp.constant.label, names))
						return false;
					exp.constant.excluded_count = 0;
				}
			}
		}
	} else if (tokens[index].type == DATALOG_TOKEN_SLASH_PLUS) {
		/* this is a negation */
		index++;
		if (!init(exp.func))
			return false;
		if (!new_expression(exp.func.arg)
		 || !datalog_interpret_expression(tokens, index, *exp.func.arg, names, variables)) {
			if (exp.func.arg != NULL) free(exp.func.arg);
			return false;
		}
		for (unsigned int i = 0; i < array_length(exp.func.vars); i++)
			exp.func.vars[i] = 0;
		exp.func.function = PREDICATE_NOT;
		exp.type = DATALOG_FUNCTION;
		exp.reference_count = 1;
	} else if (tokens[index].type == DATALOG_TOKEN_LBRACKET) {
		/* this is a list */
		index++;
		if (!init(exp.list))
			return false;
		exp.type = DATALOG_LIST;
		exp.reference_count = 1;
		if (!datalog_interpret_list(tokens, index, exp.list.elements, names, variables)) {
			free(exp.list);
			return false;
		}
	} else if (tokens[index].type == DATALOG_TOKEN_LPAREN) {
		/* this is a tuple */
		index++;
		if (!init(exp.tuple, POSITION_EXACT, 1)) {
			fprintf(stderr, "datalog_interpret_expression ERROR: Unable to initialize tuple.\n");
			return false;
		}
		exp.type = DATALOG_TUPLE;
		exp.reference_count = 1;
		if (!datalog_interpret_tuple(tokens, index, exp.tuple.elements, names, variables)) {
			free(exp.tuple);
			return false;
		}
	} else {
		read_error("Unexpected symbol at beginning of expression", tokens[index].start);
		return false;
	}
	return true;
}

bool datalog_interpret(
	const array<datalog_token>& tokens,
	array<datalog_expression_root*>& expressions,
	hash_map<string, unsigned int>& names)
{
	unsigned int index = 0;
	while (index < tokens.length) {
		if (!expressions.ensure_capacity(expressions.length + 1))
			return false;

		hash_map<string, unsigned int> variables = hash_map<string, unsigned int>(16);
		datalog_expression_root*& next_expression = expressions[(unsigned int) expressions.length];
		next_expression = (datalog_expression_root*) malloc(sizeof(datalog_expression_root));
		if (next_expression == NULL) {
			fprintf(stderr, "datalog_interpret ERROR: Out of memory.\n");
			return false;
		}
		next_expression->index = NUMBER_ALL;
		next_expression->concord = NUMBER_NONE;
		next_expression->inf = INFLECTION_NONE;
		if (!datalog_interpret_expression(tokens, index, next_expression->root, names, variables))
			return false;
		for (auto entry : variables) free(entry.key);
		expressions.length++;

		if (!expect_token(tokens, index, DATALOG_TOKEN_PERIOD, "period at end of statement"))
			return false;
		index++;
	}
	return true;
}


/**
 * Below is code for parsing a variable-free variant of Datalog.
 */

#include <grammar/tree_semantics.h>

/* forward declarations */
bool variable_free_interpret_expression(
	const array<datalog_token>& tokens,
	unsigned int& index, tree_semantics& expression,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables);

bool datalog_interpret_sentence(
	const array<datalog_token>& tokens, unsigned int& index,
	array<array<unsigned int>>& sentences,
	hash_map<string, unsigned int>& names)
{
	if (!sentences.ensure_capacity(sentences.length + 1)
	 || !array_init(sentences[(unsigned int) sentences.length], 16))
		return false;
	sentences.length++;

	array<unsigned int>& sentence = sentences.last();
	while (true) {
		if (!sentence.ensure_capacity(sentence.length + 1))
			return false;

		if (index >= tokens.length) {
			fprintf(stderr, "ERROR: Unexpected end of input.\n");
			return false;
		} else if (tokens[index].type == DATALOG_TOKEN_TAB) {
			index++;
			return true;
		} else if (tokens[index].type == DATALOG_TOKEN_PERIOD) {
			if (!get_token(".", sentence[(unsigned int) sentence.length], names))
				return false;
			sentence.length++; index++;
		} else if (tokens[index].type == DATALOG_TOKEN_IDENTIFIER || tokens[index].type == DATALOG_TOKEN_STRING) {
			if (!get_token(tokens[index].text, sentence[(unsigned int) sentence.length], names))
				return false;
			sentence.length++; index++;
		} else {
			read_error("Expected a sentence token or tab", tokens[index].start);
			return false;
		}
	}
}

bool variable_free_interpret_args(
	const array<datalog_token>& tokens, unsigned int& index,
	tree_semantics& expression,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	expression.left_child = (tree_semantics*) malloc(sizeof(tree_semantics));
	if (expression.left_child == NULL) {
		fprintf(stderr, "variable_free_interpret_args ERROR: Out of memory.\n");
		return false;
	}
	if (!variable_free_interpret_expression(tokens, index, *expression.left_child, names, variables))
		return false;

	if (index >= tokens.length || tokens[index].type != DATALOG_TOKEN_COMMA)
		return true;
	index++;

	expression.right_child = (tree_semantics*) malloc(sizeof(tree_semantics));
	if (expression.right_child == NULL) {
		fprintf(stderr, "variable_free_interpret_args ERROR: Out of memory.\n");
		return false;
	}
	return variable_free_interpret_expression(tokens, index, *expression.right_child, names, variables);
}

bool variable_free_interpret_expression(
	const array<datalog_token>& tokens, unsigned int& index,
	tree_semantics& expression,
	hash_map<string, unsigned int>& names,
	hash_map<string, unsigned int>& variables)
{
	expression.left_child = NULL;
	expression.right_child = NULL;
	expression.excluded_count = 0;
	expression.reference_count = 1;

	if (!expect_token(tokens, index, DATALOG_TOKEN_IDENTIFIER,
		"identifier at beginning of variable-free expression"))
		return false;
	if (!get_token(tokens[index].text, expression.label, names))
		return false;
	index++;

	if (index >= tokens.length) {
		fprintf(stderr, "ERROR: Unexpected end of input.\n");
		return false;
	} else if (tokens[index].type == DATALOG_TOKEN_LPAREN) {
		index++;
		if (!variable_free_interpret_args(tokens, index, expression, names, variables))
			return false;
		if (!expect_token(tokens, index, DATALOG_TOKEN_RPAREN, "Closing right parenthesis"))
			return false;
		index++;
	}
	return true;
}

bool variable_free_interpret(
	const array<datalog_token>& tokens,
	array<array<unsigned int>>& sentences,
	array<tree_semantics>& expressions,
	hash_map<string, unsigned int>& names)
{
	unsigned int index = 0;
	hash_map<string, unsigned int> dummy = hash_map<string, unsigned int>(1);
	while (index < tokens.length) {
		if (!expressions.ensure_capacity(expressions.length + 1))
			return false;
		tree_semantics& expression = expressions[(unsigned int) expressions.length];

		if (!datalog_interpret_sentence(tokens, index, sentences, names)
		 || !variable_free_interpret_expression(tokens, index, expression, names, dummy))
			return false;
		expressions.length++;
	}
	for (auto entry : dummy) free(entry.key);
	return true;
}


/**
 * Some routines for initializing derivation trees.
 */

template<typename Distribution>
bool sample_preterminal(
	const grammar<datalog_expression, Distribution>& G,
	const sequence& terminal, unsigned int& sample)
{
	array<unsigned int> preterminals = array<unsigned int>(G.nonterminals.length);
	for (unsigned int i = 0; i < G.nonterminals.length; i++) {
		if (!G.nonterminals[i].is_preterminal)
			continue;
		auto& terminal_prior = G.nonterminals[i].rule_distribution.h.pi.terminal_prior;
		if (terminal_prior.probability(terminal) > 0.0)
			preterminals.add(G.nonterminals[i].id);
	}
	if (preterminals.length == 0)
		return false;
	sample = sample_uniform(preterminals);
	return true;
}

template<typename Distribution>
bool sample_nonterminal(const grammar<datalog_expression, Distribution>& G, unsigned int& sample) {
	array<unsigned int> nonterminals = array<unsigned int>(G.nonterminals.length);
	for (unsigned int i = 0; i < G.nonterminals.length; i++) {
		if (!G.nonterminals[i].is_preterminal)
			nonterminals.add(G.nonterminals[i].id);
	}
	if (nonterminals.length == 0)
		return false;
	sample = sample_uniform(nonterminals);
	return true;
}

template<typename RulePrior>
inline bool sentence_starts_with(
		const RulePrior& rule_prior, const sequence& sentence,
		unsigned int earliest_end, unsigned int latest_end,
		unsigned int earliest_start, unsigned int& found)
{
	unsigned int start = earliest_start;
	for (unsigned int end = earliest_end; end < latest_end + 1; end++) {
		rule<datalog_expression_root> r = rule<datalog_expression_root>({sentence.tokens + start, end - start});
		if (rule_prior.probability(r) > 0.0) {
			found = end;
			return true;
		}
	}
	return false;
}

template<typename RulePrior>
inline bool sentence_ends_with(
		const RulePrior& rule_prior, const sequence& sentence,
		unsigned int end, unsigned int earliest_start, unsigned int& found)
{
	for (unsigned int start = earliest_start; start < end; start++) {
		rule<datalog_expression_root> r = rule<datalog_expression_root>({sentence.tokens + start, end - start});
		if (rule_prior.probability(r) > 0.0) {
			found = end;
			return true;
		}
	}
	return false;
}

template<typename RulePrior>
inline bool sentence_matches(
		const RulePrior& rule_prior, const sequence& sentence,
		unsigned int earliest_end, unsigned int latest_end,
		unsigned int earliest_start, unsigned int& found)
{
	for (unsigned int end = earliest_end; end < latest_end + 1; end++) {
		if (sentence_ends_with(rule_prior, sentence, end, earliest_start, found))
			return true;
	}
	return false;
}

template<typename Distribution>
bool rule_matches(const grammar<datalog_expression_root, Distribution>& G,
		const rule<datalog_expression_root>& r, const sequence& sentence)
{
	if (r.length > sentence.length)
		return false;
	unsigned int prev = 0;
	for (unsigned int k = 0; k < r.length; k++) {
		const auto& child_nonterminal = G.nonterminals[r.nonterminals[k] - 1];
		const auto& rule_prior = child_nonterminal.rule_distribution.h.pi;
		if (child_nonterminal.is_preterminal) {
			/* find the earliest end position for this preterminal */
			unsigned int earliest_start = prev;
			unsigned int earliest_end = prev + 1;
			unsigned int latest_end = sentence.length - r.length + k + 1;

			if (k == 0) {
				if (!sentence_starts_with(rule_prior, sentence, earliest_end, latest_end, earliest_start, prev))
					return false;
			} else if (k + 1 == r.length) {
				if (!sentence_ends_with(rule_prior, sentence, latest_end, earliest_start, prev))
					return false;
			} else {
				if (!sentence_matches(rule_prior, sentence, earliest_end, latest_end, earliest_start, prev))
					return false;
			}
		} else {
			prev++;
		}
	}
	return true;
}

/* checks if the given logical form can be associated with the given nonterminal
   (by checking whether the semantic feature functions can be evaluated) */
template<typename Distribution>
bool is_nonterminal_valid(const grammar<datalog_expression, Distribution>& G,
		const datalog_expression& logical_form, unsigned int nonterminal)
{
	const auto& N = G.nonterminals[nonterminal - 1];
	unsigned int* excluded; unsigned int value, excluded_count;
	for (unsigned int i = 0; i < N.feature_count; i++)
		if (!get_feature(N.feature_sequence[i], logical_form, value, excluded, excluded_count)) return false;
	return true;
}

template<typename Distribution>
bool initialize_tree(
	const grammar<datalog_expression_root, Distribution>& G,
	syntax_node<datalog_expression_root>*& tree,
	const sequence& sentence,
	const datalog_expression_root& logical_form,
	unsigned int nonterminal = 1)
{
	if (!is_nonterminal_valid(G, logical_form, nonterminal))
		return false;

	const auto& rule_prior = G.nonterminals[nonterminal - 1].rule_distribution.h.pi;
	tree = (syntax_node<datalog_expression_root>*) malloc(sizeof(syntax_node<datalog_expression_root>));
	if (tree == NULL) {
		exit(EXIT_FAILURE);
	} if (G.nonterminals[nonterminal - 1].is_preterminal) {
		/* this is a terminal */
		if (!init(*tree, sentence)) exit(EXIT_FAILURE);
		return true;
	}

	/* pick a random production rule */
	array<unsigned int> available_rules = array<unsigned int>(8);
	for (unsigned int i = 0; i < rule_prior.rules.capacity; i++) {
		const rule<datalog_expression_root>& r = rule_prior.rules.keys[i];
		if (is_empty(r)) continue;
		if (rule_matches(G, r, sentence)) {
			if (!available_rules.add(i))
				exit(EXIT_FAILURE);
		}
	}
	if (available_rules.length == 0) {
		free(tree); tree = NULL;
		return false;
	}

	const rule<datalog_expression_root>& selected_rule = rule_prior.rules.keys[sample_uniform(available_rules)];
	if (!init(*tree, selected_rule))
		exit(EXIT_FAILURE);

	unsigned int prev = 0;
	for (unsigned int k = 0; k < selected_rule.length; k++) {
		/* pick a random split point from {prev + 1, ..., n - K + k} */
		unsigned int start, next;
		if (k + 1 == selected_rule.length)
			start = sentence.length;
		else start = prev + 1;
		const auto& child_nonterminal = G.nonterminals[selected_rule.nonterminals[k] - 1];
		if (child_nonterminal.is_preterminal) {
			array<unsigned int> available_next = array<unsigned int>(8);
			for (unsigned int i = start; i < sentence.length - selected_rule.length + k + 2; i++) {
				const auto& rule_prior = child_nonterminal.rule_distribution.h.pi;
				rule<datalog_expression_root> r = rule<datalog_expression_root>({sentence.tokens + prev, i - prev});
				if (rule_prior.probability(r) > 0.0 && !available_next.add(i))
					exit(EXIT_FAILURE);
			}
			if (available_next.length == 0) {
				free(*tree); free(tree);
				tree = NULL; return false;
			}
			next = sample_uniform(available_next);
		} else {
			next = sample_uniform(sentence.length - selected_rule.length + k - start + 2) + start;
		}

		datalog_expression_root child;
		if (!apply(selected_rule.transformations[k], logical_form, child)) {
			free(*tree); free(tree);
			tree = NULL; return false;
		} if (!initialize_tree(G, tree->children[k], {sentence.tokens + prev, next - prev}, child, selected_rule.nonterminals[k])) {
			free(child);
			free(*tree); free(tree);
			tree = NULL; return false;
		}
		free(child);
		prev = next;
	}
	return true;
}

inline bool can_be_empty(const datalog_expression& exp) {
	if (exp.type == DATALOG_NON_EMPTY) {
		return false;
	} if (exp.type == DATALOG_ANY || exp.type == DATALOG_EMPTY) {
		return true;
	} else if (exp.type == DATALOG_CONSTANT) {
		if (exp.constant.label == DATALOG_LABEL_EMPTY) return true;
		else if (exp.constant.label != DATALOG_LABEL_WILDCARD) return false;
		return !exp.constant.is_excluded(DATALOG_LABEL_EMPTY);
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.function == DATALOG_LABEL_EMPTY) return true;
		else if (exp.pred.function != DATALOG_LABEL_WILDCARD) return false;
		return !exp.pred.is_excluded(DATALOG_LABEL_EMPTY);
	}
	return false;
}


/**
 * Semantic transformation functions and their inverses.
 */

template<unsigned int FieldCount>
inline bool select_any(datalog_expression& dst) {
	if (FieldCount == 1) {
		initialize_any(dst);
	} else {
		if (!init(dst.tuple, POSITION_EXACT, FieldCount)) return false;
		for (unsigned int i = 0; i < FieldCount; i++)
			dst.tuple.elements[i] = &DATALOG_ANY_TREE;
		dst.tuple.elements.length = FieldCount;
		DATALOG_ANY_TREE.reference_count += FieldCount;
		dst.type = DATALOG_TUPLE;
		dst.reference_count = 1;
	}
	return true;
}

template<unsigned int FieldCount>
inline bool delete_any(datalog_expression& dst) {
	initialize_any(dst);
	return true;
}

template<unsigned int FieldCount, bool KeepHead>
inline bool select_left(const datalog_expression& src, datalog_expression& dst,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY)
{
	if (src.type == DATALOG_PREDICATE && FieldCount == 1) {
		unsigned int head = get_head(src);
		parent_head = choose_head(parent_head, head);
		child_head = choose_head(child_head, head);
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;
		return init(dst, src, KeepHead ? 0 : -1);
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type != DATALOG_TUPLE || (src.tuple.position != POSITION_LEFT && src.tuple.elements.length < FieldCount)) {
		return false;
	} else {
		/* check that the head variable of the child is correct */
		for (unsigned int i = 0; i < min(FieldCount, (unsigned int) src.tuple.elements.length); i++) {
			unsigned int head = get_head(*src.tuple.elements[i]);
			child_head = choose_head(child_head, head);
			parent_head = choose_head(parent_head, head);
		}
		if (src.tuple.position != POSITION_EXACT && child_head != 1)
			child_head = DATALOG_LABEL_WILDCARD;
		for (unsigned int i = FieldCount; i < src.tuple.elements.length; i++)
			parent_head = choose_head(parent_head, get_head(*src.tuple.elements[i]));
		if (src.tuple.position != POSITION_EXACT && parent_head != 1)
			parent_head = DATALOG_LABEL_WILDCARD;
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		/* create the child logical form */
		if (FieldCount == 1) {
			if (src.tuple.elements.length == 0) {
				dst.type = DATALOG_ANY;
				dst.reference_count = 1;
			} else {
				if (!init(dst, *src.tuple.elements[0], src_variable_map, dst_variable_map))
					return false;
				compress_variable_map(dst_variable_map);
				apply_variable_map(dst, dst_variable_map);
				apply_variable_map(src_variable_map, dst_variable_map);
			}
		} else {
			if (!init(dst.tuple, POSITION_EXACT, FieldCount)) return false;
			else if (!init(dst.tuple.elements.data, src.tuple.elements.data,
					min(FieldCount, (unsigned int) src.tuple.elements.length),
					src_variable_map, dst_variable_map)) {
				free(dst.tuple); return false;
			}
			for (unsigned int i = src.tuple.elements.length; i < FieldCount; i++) {
				dst.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			dst.tuple.elements.length = FieldCount;
			dst.type = DATALOG_TUPLE;
			dst.reference_count = 1;
			compress_variable_map(dst_variable_map);
			for (unsigned int i = 0; i < min(FieldCount, (unsigned int) src.tuple.elements.length); i++)
				apply_variable_map(*dst.tuple.elements[i], dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
		}
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead>
inline bool select_left(const datalog_expression& src, datalog_expression& dst,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	array<unsigned int> src_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	src_variable_map[KeepHead ? 1 : 2] = 1;
	dst_variable_map[1] = 1;
	return select_left<FieldCount, KeepHead>(src, dst, src_variable_map, dst_variable_map, parent_head, child_head);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
inline bool delete_left(const datalog_expression& src, datalog_expression& dst,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	if (src.type == DATALOG_PREDICATE && FieldCount == 1) {
		unsigned int head = get_head(src);
		parent_head = choose_head(parent_head, head);
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		if (!init(dst.tuple, POSITION_EXACT, 1)) return false;
		dst.type = DATALOG_TUPLE;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_any<FieldCount>(dst);
	} else if (src.type != DATALOG_TUPLE || (src.tuple.position != POSITION_LEFT && src.tuple.elements.length < FieldCount)) {
		return false;
	} else {
		/* check that the head variable of the child is correct */
		for (unsigned int i = FieldCount; i < src.tuple.elements.length; i++) {
			unsigned int head = get_head(*src.tuple.elements[i]);
			child_head = choose_head(child_head, head);
			parent_head = choose_head(parent_head, head);
		}
		if (src.tuple.position != POSITION_EXACT && child_head != 1)
			child_head = DATALOG_LABEL_WILDCARD;
		for (unsigned int i = 0; i < min(FieldCount, (unsigned int) src.tuple.elements.length); i++)
			parent_head = choose_head(parent_head, get_head(*src.tuple.elements[i]));
		if (src.tuple.position != POSITION_EXACT && parent_head != 1)
			parent_head = DATALOG_LABEL_WILDCARD;
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		if (FieldCount >= src.tuple.elements.length) {
			if (src.tuple.position == POSITION_EXACT) {
				if (!init(dst.tuple, POSITION_EXACT, 1))
					return false;
				dst.type = DATALOG_TUPLE;
			} else {
				dst.type = DATALOG_ANY;
			}
		} else if (src.tuple.elements.length - FieldCount == 1 && src.tuple.position == POSITION_EXACT) {
			if (!init(dst, *src.tuple.elements[FieldCount], src_variable_map, dst_variable_map))
				return false;
			/* TODO: i think we only need to compress and apply the variable map if MergeVariables = true */
			compress_variable_map(dst_variable_map);
			apply_variable_map(dst, dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
			return true;
		} else {
			if (!init(dst.tuple, src.tuple.position,
					src.tuple.elements.data + FieldCount,
					src.tuple.elements.length - FieldCount,
					src_variable_map, dst_variable_map))
				return false;
			dst.type = DATALOG_TUPLE;
			/* TODO: i think we only need to compress and apply the variable map if MergeVariables = true */
			compress_variable_map(dst_variable_map);
			for (unsigned int i = 0; i < dst.tuple.elements.length; i++)
				apply_variable_map(*dst.tuple.elements[i], dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
		}
	}
	dst.reference_count = 1;
	return true;
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
inline bool delete_left(const datalog_expression& src, datalog_expression& dst,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	array<unsigned int> src_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	src_variable_map[KeepHead ? 1 : 2] = 1;
	dst_variable_map[1] = 1;
	return delete_left<FieldCount, KeepHead, MergeVariables>(src, dst, src_variable_map, dst_variable_map, parent_head, child_head);
}

template<unsigned int FieldCount, bool KeepHead>
inline bool select_right(const datalog_expression& src, datalog_expression& dst,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	if (src.type == DATALOG_PREDICATE && FieldCount == 1) {
		return init(dst, src);
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type != DATALOG_TUPLE || (src.tuple.position != POSITION_RIGHT && src.tuple.elements.length < FieldCount)) {
		return false;
	} else {
		/* check that the head variable of the child is correct */
		for (unsigned int i = max(FieldCount, (unsigned int) src.tuple.elements.length) - FieldCount; i < src.tuple.elements.length; i++) {
			unsigned int head = get_head(*src.tuple.elements[i]);
			child_head = choose_head(child_head, head);
			parent_head = choose_head(parent_head, head);
		}
		if (src.tuple.position != POSITION_EXACT && child_head != 1)
			child_head = DATALOG_LABEL_WILDCARD;
		for (unsigned int i = 0; i < src.tuple.elements.length - FieldCount; i++)
			parent_head = choose_head(parent_head, get_head(*src.tuple.elements[i]));
		if (src.tuple.position != POSITION_EXACT && parent_head != 1)
			parent_head = DATALOG_LABEL_WILDCARD;
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		if (FieldCount == 1) {
			if (src.tuple.elements.length == 0) {
				dst.type = DATALOG_ANY;
				dst.reference_count = 1;
			} else {
				if (!init(dst, *src.tuple.elements.last(), src_variable_map, dst_variable_map))
					return false;
				compress_variable_map(dst_variable_map);
				apply_variable_map(dst, dst_variable_map);
				apply_variable_map(src_variable_map, dst_variable_map);
			}
		} else {
			if (!init(dst.tuple, POSITION_EXACT, FieldCount)) return false;
			if (src.tuple.elements.length < FieldCount) {
				if (!init(dst.tuple.elements.data, src.tuple.elements.data,
						src.tuple.elements.length, src_variable_map, dst_variable_map)) {
					free(dst.tuple); return false;
				}
				for (unsigned int i = 0; i < FieldCount - src.tuple.elements.length; i++)
					dst.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count += FieldCount - src.tuple.elements.length;
			} else {
				if (!init(dst.tuple.elements.data,
						src.tuple.elements.data + src.tuple.elements.length - FieldCount,
						FieldCount, src_variable_map, dst_variable_map)) {
					free(dst.tuple); return false;
				}
			}
			dst.tuple.elements.length = FieldCount;
			dst.type = DATALOG_TUPLE;
			dst.reference_count = 1;
			compress_variable_map(dst_variable_map);
			for (unsigned int i = 0; i < dst.tuple.elements.length; i++)
				apply_variable_map(*dst.tuple.elements[i], dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
		}
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead>
inline bool select_right(const datalog_expression& src, datalog_expression& dst,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	array<unsigned int> src_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	src_variable_map[KeepHead ? 1 : 2] = 1;
	dst_variable_map[1] = 1;
	return select_right<FieldCount, KeepHead>(src, dst, src_variable_map, dst_variable_map, parent_head, child_head);
}

template<unsigned int FieldCount, bool KeepHead>
inline bool delete_right(const datalog_expression& src, datalog_expression& dst,
		array<unsigned int>& src_variable_map, array<unsigned int>& dst_variable_map,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	if (src.type == DATALOG_PREDICATE && FieldCount == 1) {
		unsigned int head = get_head(src);
		parent_head = choose_head(parent_head, head);
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		if (!init(dst.tuple, POSITION_EXACT, 1)) return false;
		dst.type = DATALOG_TUPLE;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_any<FieldCount>(dst);
	} else if (src.type != DATALOG_TUPLE || (src.tuple.position != POSITION_RIGHT && src.tuple.elements.length < FieldCount)) {
		return false;
	} else {
		/* check that the head variable of the child is correct */
		for (unsigned int i = 0; i < max(FieldCount, (unsigned int) src.tuple.elements.length) - FieldCount; i++) {
			unsigned int head = get_head(*src.tuple.elements[i]);
			child_head = choose_head(child_head, head);
			parent_head = choose_head(parent_head, head);
		}
		if (src.tuple.position != POSITION_EXACT && child_head != 1)
			child_head = DATALOG_LABEL_WILDCARD;
		for (unsigned int i = max(FieldCount, (unsigned int) src.tuple.elements.length) - FieldCount; i < src.tuple.elements.length; i++)
			parent_head = choose_head(parent_head, get_head(*src.tuple.elements[i]));
		if (src.tuple.position != POSITION_EXACT && parent_head != 1)
			parent_head = DATALOG_LABEL_WILDCARD;
		if (KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head != child_head) return false;
		if (!KeepHead && parent_head != DATALOG_LABEL_WILDCARD && child_head != DATALOG_LABEL_WILDCARD && parent_head == child_head) return false;

		if (FieldCount >= src.tuple.elements.length)  {
			if (src.tuple.position == POSITION_EXACT) {
				if (!init(dst.tuple, POSITION_EXACT, 1))
					return false;
				dst.type = DATALOG_TUPLE;
			} else {
				dst.type = DATALOG_ANY;
			}
		} else if (src.tuple.elements.length - FieldCount == 1 && src.tuple.position == POSITION_EXACT) {
			if (!init(dst, *src.tuple.elements[0], src_variable_map, dst_variable_map))
				return false;
			compress_variable_map(dst_variable_map);
			apply_variable_map(dst, dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
			return true;
		} else {
			if (!init(dst.tuple, src.tuple.position,
					src.tuple.elements.data,
					src.tuple.elements.length - FieldCount,
					src_variable_map, dst_variable_map))
				return false;
			dst.type = DATALOG_TUPLE;
			compress_variable_map(dst_variable_map);
			for (unsigned int i = 0; i < dst.tuple.elements.length; i++)
				apply_variable_map(*dst.tuple.elements[i], dst_variable_map);
			apply_variable_map(src_variable_map, dst_variable_map);
		}
	}
	dst.reference_count = 1;
	return true;
}

template<unsigned int FieldCount, bool KeepHead>
inline bool delete_right(const datalog_expression& src, datalog_expression& dst,
		unsigned int parent_head = DATALOG_LABEL_EMPTY, unsigned int child_head = DATALOG_LABEL_EMPTY) {
	array<unsigned int> src_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	src_variable_map[KeepHead ? 1 : 2] = 1;
	dst_variable_map[1] = 1;
	return delete_right<FieldCount, KeepHead>(src, dst, src_variable_map, dst_variable_map, parent_head, child_head);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool select_left_keep_function(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_FUNCTION) {
		if (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate)
			return false;
	} else if (src.type != DATALOG_ANY && src.type != DATALOG_NON_EMPTY) return false;
	if (!init(dst.func)) return false;
	dst.func.arg = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		if (dst.func.arg == NULL || !select_any<FieldCount>(*dst.func.arg)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++)
			dst.func.vars[i] = 0;
		dst.func.function = DATALOG_LABEL_WILDCARD;
	} else {
		array<unsigned int> src_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
		src_variable_map[KeepHead ? 1 : 2] = 1;
		dst_variable_map[1] = 1;

		unsigned int head = DATALOG_LABEL_EMPTY;
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++) {
			if (src.func.vars[i] == 0) {
				dst.func.vars[i] = 0;
			} else {
				if (!KeepHead && src.func.vars[i] == 1) {
					free(dst.func.arg);
					return false;
				}
				dst.func.vars[i] = src.func.vars[i] - (KeepHead ? 0 : 1);
				head = choose_head(head, src.func.vars[i]);
				src_variable_map[src.func.vars[i]] = dst.func.vars[i];
				dst_variable_map[dst.func.vars[i]] = 1;
			}
		}
		if (dst.func.arg == NULL || !select_left<FieldCount, KeepHead>(
				*src.func.arg, *dst.func.arg, src_variable_map, dst_variable_map, head, head)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}

		dst.func.function = src.func.function;
		if (src.func.excluded_count > 0 && !init_excluded(dst.func.excluded, src.func.excluded, src.func.excluded_count)) {
			free(*dst.func.arg); free(dst.func.arg);
			return false;
		}
		dst.func.excluded_count = src.func.excluded_count;

	}
	dst.type = DATALOG_FUNCTION;
	dst.reference_count = 1;
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool delete_left_keep_function(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_FUNCTION) {
		if (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate)
			return false;
	} else if (src.type != DATALOG_ANY && src.type != DATALOG_NON_EMPTY
			&& !(src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT))
		return false;
	if (!init(dst.func)) return false;
	dst.func.arg = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY
	 || (src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT)) {
		if (dst.func.arg == NULL || !delete_any<FieldCount>(*dst.func.arg)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++)
			dst.func.vars[i] = 0;
		dst.func.function = DATALOG_LABEL_WILDCARD;
	} else {
		array<unsigned int> src_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
		src_variable_map[KeepHead ? 1 : 2] = 1;
		dst_variable_map[1] = 1;

		unsigned int head = DATALOG_LABEL_EMPTY;
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++) {
			if (src.func.vars[i] == 0) {
				dst.func.vars[i] = 0;
			} else {
				if (!KeepHead && src.func.vars[i] == 1) {
					free(dst.func.arg);
					return false;
				}
				dst.func.vars[i] = src.func.vars[i] - (KeepHead ? 0 : 1);
				head = choose_head(head, src.func.vars[i]);
				src_variable_map[src.func.vars[i]] = dst.func.vars[i];
				dst_variable_map[dst.func.vars[i]] = 1;
			}
		}
		if (dst.func.arg == NULL || !delete_left<FieldCount, KeepHead>(
				*src.func.arg, *dst.func.arg, src_variable_map, dst_variable_map, head, head)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}

		dst.func.function = src.func.function;
		if (src.func.excluded_count > 0 && !init_excluded(dst.func.excluded, src.func.excluded, src.func.excluded_count)) {
			free(*dst.func.arg); free(dst.func.arg);
			return false;
		}
		dst.func.excluded_count = src.func.excluded_count;
	}
	dst.type = DATALOG_FUNCTION;
	dst.reference_count = 1;
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool delete_right_keep_function(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_FUNCTION) {
		if (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate)
			return false;
	} else if (src.type != DATALOG_ANY && src.type != DATALOG_NON_EMPTY) return false;
	if (!init(dst.func)) return false;
	dst.func.arg = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		if (dst.func.arg == NULL || !delete_any<FieldCount>(*dst.func.arg)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++)
			dst.func.vars[i] = 0;
		dst.func.function = DATALOG_LABEL_WILDCARD;
	} else {
		array<unsigned int> src_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
		src_variable_map[KeepHead ? 1 : 2] = 1;
		dst_variable_map[1] = 1;

		unsigned int head = DATALOG_LABEL_EMPTY;
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++) {
			if (src.func.vars[i] == 0) {
				dst.func.vars[i] = 0;
			} else {
				if (!KeepHead && src.func.vars[i] == 1) {
					free(dst.func.arg);
					return false;
				}
				dst.func.vars[i] = src.func.vars[i] - (KeepHead ? 0 : 1);
				head = choose_head(head, src.func.vars[i]);
				src_variable_map[src.func.vars[i]] = dst.func.vars[i];
				dst_variable_map[dst.func.vars[i]] = 1;
			}
		}
		if (dst.func.arg == NULL || !delete_right<FieldCount, KeepHead>(
				*src.func.arg, *dst.func.arg, src_variable_map, dst_variable_map, head, head)) {
			if (dst.func.arg != NULL)
				free(dst.func.arg);
			return false;
		}

		dst.func.function = src.func.function;
		if (src.func.excluded_count > 0 && !init_excluded(dst.func.excluded, src.func.excluded, src.func.excluded_count)) {
			free(*dst.func.arg); free(dst.func.arg);
			return false;
		}
		dst.func.excluded_count = src.func.excluded_count;
	}
	dst.type = DATALOG_FUNCTION;
	dst.reference_count = 1;
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool delete_left_answer_keep_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_left_keep_function<FieldCount, Predicate, KeepHead>(DATALOG_ANY_TREE, dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return delete_left_keep_function<FieldCount, Predicate, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool select_left_delete_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT) {
		dst.type = DATALOG_ANY;
		dst.reference_count = 1;
		return true;
	} else if (src.type != DATALOG_FUNCTION
	 || (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate))
		return false;
	unsigned int parent_head = DATALOG_LABEL_EMPTY;
	for (unsigned int i = 0; i < array_length(src.func.vars); i++)
		if (src.func.vars[i] != 0) parent_head = choose_head(parent_head, src.func.vars[i]);
	return select_left<FieldCount, KeepHead>(*src.func.arg, dst, parent_head);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool select_right_delete_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION
	 || (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate))
		return false;
	unsigned int parent_head = DATALOG_LABEL_EMPTY;
	for (unsigned int i = 0; i < array_length(src.func.vars); i++)
		if (src.func.vars[i] != 0) parent_head = choose_head(parent_head, src.func.vars[i]);
	return select_right<FieldCount, KeepHead>(*src.func.arg, dst, parent_head);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool delete_left_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION
	 || (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate))
		return false;
	unsigned int parent_head = DATALOG_LABEL_EMPTY;
	for (unsigned int i = 0; i < array_length(src.func.vars); i++)
		if (src.func.vars[i] != 0) parent_head = choose_head(parent_head, src.func.vars[i]);
	return delete_left<FieldCount, KeepHead>(*src.func.arg, dst, parent_head);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
bool select_left_delete_function_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_left_delete_function<FieldCount, Predicate, KeepHead>(DATALOG_ANY_TREE, dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return select_left_delete_function<FieldCount, Predicate, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
bool delete_left_function_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_left_function<FieldCount, Predicate, KeepHead>(DATALOG_ANY_TREE, dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return delete_left_function<FieldCount, Predicate, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, bool KeepHead>
bool select_left_delete_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return select_left<FieldCount, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, bool KeepHead>
bool select_right_delete_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return select_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return select_right<FieldCount, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, bool KeepHead>
bool delete_left_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return delete_left<FieldCount, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldCount, bool KeepHead>
bool delete_right_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_any<FieldCount>(dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return delete_right<FieldCount, KeepHead>(*src.func.arg, dst);
}

inline bool delete_answer(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		initialize_any(dst); return true;
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return init(dst, *src.func.arg);
}

template<unsigned int Predicate, bool KeepHead>
inline bool select_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_FUNCTION) {
		if (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate)
			return false;
	} else if (src.type != DATALOG_ANY && src.type != DATALOG_NON_EMPTY) return false;
	if (!init(dst.func)) return false;
	dst.func.arg = (datalog_expression*) malloc(sizeof(datalog_expression));
	if (dst.func.arg == NULL || !init(dst.func.arg->tuple, POSITION_EXACT, 1)) {
		if (dst.func.arg != NULL)
			free(dst.func.arg);
		return false;
	}
	dst.func.arg->type = DATALOG_TUPLE;
	dst.func.arg->reference_count = 1;
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++)
			dst.func.vars[i] = 0;
		dst.func.function = DATALOG_LABEL_WILDCARD;
	} else {
		unsigned int src_head = DATALOG_LABEL_EMPTY;
		for (unsigned int i = 0; i < array_length(dst.func.vars); i++) {
			if (src.func.vars[i] == 0) {
				dst.func.vars[i] = 0;
			} else if (src.func.vars[i] == 1 && !KeepHead) {
				free(*dst.func.arg); free(dst.func.arg);
				return false;
			} else {
				src_head = choose_head(src_head, src.func.vars[i]);
				dst.func.vars[i] = src.func.vars[i] - (KeepHead ? 0 : 1);
			}
		}
		if (KeepHead && src_head != 1 && src_head != DATALOG_LABEL_EMPTY) {
			free(*dst.func.arg); free(dst.func.arg);
			return false;
		}

		dst.func.function = src.func.function;
		if (src.func.excluded_count > 0 && !init_excluded(dst.func.excluded, src.func.excluded, src.func.excluded_count)) {
			free(*dst.func.arg); free(dst.func.arg);
			return false;
		}
		dst.func.excluded_count = src.func.excluded_count;
	}
	dst.type = DATALOG_FUNCTION;
	dst.reference_count = 1;
	return true;
}

template<unsigned int Predicate, bool KeepHead>
inline bool delete_function(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		initialize_any(dst);
		return true;
	} else if (src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT) {
		return init(dst, src);
	} else if (src.type != DATALOG_FUNCTION
	 || (Predicate != DATALOG_LABEL_WILDCARD && src.func.function != DATALOG_LABEL_WILDCARD && src.func.function != Predicate))
		return false;
	unsigned int child_head = get_head(*src.func.arg);
	if (KeepHead && child_head != DATALOG_LABEL_WILDCARD && child_head != 1) return false;
	if (!KeepHead && child_head != DATALOG_LABEL_WILDCARD && child_head == 1) return false;

	array<unsigned int> src_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(src_variable_map.data, 0, sizeof(unsigned int) * src_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	src_variable_map[KeepHead ? 1 : 2] = 1;
	dst_variable_map[1] = 1;

	if (!init(dst, *src.func.arg, src_variable_map, dst_variable_map))
		return false;
	if (!KeepHead) dst_variable_map[1] = 1;
	compress_variable_map(dst_variable_map);
	apply_variable_map(dst, dst_variable_map);
	return true;
}

template<unsigned int Predicate, bool KeepHead>
bool delete_function_answer(const datalog_expression& src, datalog_expression& dst)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return delete_function<Predicate, KeepHead>(DATALOG_ANY_TREE, dst);
	} else if (src.type != DATALOG_FUNCTION || src.func.function != PREDICATE_ANSWER)
		return false;
	return delete_function<Predicate, KeepHead>(*src.func.arg, dst);
}

template<unsigned int FieldIndex, bool OtherArgsEmpty>
inline bool select_arg(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		initialize_any(dst); return true;
	} else if (src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT) {
		initialize_any(dst); return true;
	} else if (src.type != DATALOG_PREDICATE || FieldIndex >= array_length(src.pred.args))
		return false;
	if (OtherArgsEmpty) {
		for (unsigned int i = 0; i < array_length(src.pred.args); i++) {
			if (i == FieldIndex) continue;
			if (src.pred.args[i] != NULL && !can_be_empty(*src.pred.args[i]))
				return false;
		}
	}
	if (src.pred.args[FieldIndex] == NULL) {
		dst.type = DATALOG_EMPTY;
		return true;
	}
	return init(dst, *src.pred.args[FieldIndex]);
}

template<unsigned int FieldIndex>
inline bool delete_arg(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY
	 || (src.type == DATALOG_TUPLE && src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT)) {
		dst.type = DATALOG_PREDICATE;
		dst.reference_count = 1;
		dst.pred.function = DATALOG_LABEL_WILDCARD;
		dst.pred.excluded_count = 0;
		for (unsigned int i = 0; i < array_length(dst.pred.args); i++)
			dst.pred.args[i] = &DATALOG_ANY_TREE;
		dst.pred.args[FieldIndex] = NULL;
		DATALOG_ANY_TREE.reference_count += (array_length(dst.pred.args) - 1);
	} else if (src.type != DATALOG_PREDICATE) {
		return false;
	} else {
		if (FieldIndex >= array_length(src.pred.args) || src.pred.args[FieldIndex] == NULL)
			return false;
		dst.type = DATALOG_PREDICATE;
		dst.reference_count = 1;
		dst.pred.function = src.pred.function;
		if (src.pred.excluded_count > 0 && !init_excluded(dst.pred.excluded, src.pred.excluded, src.pred.excluded_count)) {
			return false;
		}
		dst.pred.excluded_count = src.pred.excluded_count;
		for (unsigned int i = 0 ; i < array_length(dst.pred.args); i++) {
			if (i == FieldIndex) continue;
			dst.pred.args[i] = src.pred.args[i];
			if (src.pred.args[i] != NULL)
				src.pred.args[i]->reference_count++;
		}
		dst.pred.args[FieldIndex] = NULL;
	}
	return true;
}

inline bool delete_args(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		dst.type = DATALOG_PREDICATE;
		dst.reference_count = 1;
		dst.pred.function = DATALOG_LABEL_WILDCARD;
		dst.pred.excluded_count = 0;
		for (unsigned int i = 0; i < array_length(dst.pred.args); i++)
			dst.pred.args[i] = NULL;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_RIGHT || src.tuple.elements.length != 1)
			return false;
		else return delete_args(*src.tuple.elements[0], dst);
	} else if (src.type != DATALOG_PREDICATE) {
		return false;
	} else {
		dst.type = DATALOG_PREDICATE;
		dst.reference_count = 1;
		dst.pred.function = src.pred.function;
		if (src.pred.excluded_count > 0 && !init_excluded(dst.pred.excluded, src.pred.excluded, src.pred.excluded_count)) {
			return false;
		}
		dst.pred.excluded_count = src.pred.excluded_count;
		for (unsigned int i = 0 ; i < array_length(dst.pred.args); i++)
			dst.pred.args[i] = NULL;
	}
	return true;
}

template<unsigned int HeadIndex, unsigned int SelectIndex, bool OtherArgsEmpty>
inline bool head_arg_select_arg(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		initialize_any(dst); return true;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length == 0) {
			if (src.tuple.position == POSITION_EXACT) return false;
			initialize_any(dst); return true;
		} else if (src.tuple.elements.length > 1) {
			return false;
		} else {
			return head_arg_select_arg<HeadIndex, SelectIndex, OtherArgsEmpty>(*src.tuple.elements[0], dst);
		}
	} else if (src.type != DATALOG_PREDICATE || HeadIndex >= array_length(src.pred.args))
		return false;
	if (src.pred.args[HeadIndex] == NULL || src.pred.args[SelectIndex] == NULL) return false;
	if (src.pred.args[HeadIndex]->type != DATALOG_ANY && src.pred.args[HeadIndex]->type != DATALOG_NON_EMPTY
	 && (src.pred.args[HeadIndex]->type != DATALOG_VARIABLE || src.pred.args[HeadIndex]->variable != 1))
		return false;
	for (unsigned int i = 0; OtherArgsEmpty && i < array_length(src.pred.args); i++) {
		if (i == HeadIndex || i == SelectIndex) continue;
		if (src.pred.args[i] != NULL && !can_be_empty(*src.pred.args[i]))
			return false;
	}
	return select_arg<SelectIndex, false>(src, dst);
}

template<unsigned int Predicate>
inline bool check_predicate(const datalog_expression& src) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return true;
	} else if (src.type == DATALOG_PREDICATE) {
		if (src.pred.function == DATALOG_LABEL_WILDCARD) {
			if (src.pred.is_excluded(Predicate)) return false;
		} else if (src.pred.function != Predicate) return false;
		if (src.pred.args[0] == NULL || src.pred.args[1] == NULL)
			return false;

		if (src.pred.args[0]->type != DATALOG_ANY && src.pred.args[0]->type != DATALOG_NON_EMPTY
		 && (src.pred.args[0]->type != DATALOG_VARIABLE || src.pred.args[0]->variable != 2))
			return false;
		if (src.pred.args[1]->type != DATALOG_ANY && src.pred.args[1]->type != DATALOG_NON_EMPTY
		 && (src.pred.args[1]->type != DATALOG_VARIABLE || src.pred.args[1]->variable != 1))
			return false;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_RIGHT) return false;
		else if (src.tuple.elements.length == 0)
			return src.tuple.position == POSITION_LEFT;
		return check_predicate<Predicate>(*src.tuple.elements[0]);
	} else {
		return false;
	}
	return true;
}

template<unsigned int Predicate>
inline bool check_predicate_answer(const datalog_expression& src) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		return true;
	} else if (src.type == DATALOG_FUNCTION) {
		if (src.func.function == DATALOG_LABEL_WILDCARD) {
			if (src.func.is_excluded(PREDICATE_ANSWER)) return false;
		} else if (src.func.function != PREDICATE_ANSWER) return false;
		return check_predicate<Predicate>(*src.func.arg);
	} else {
		return false;
	}
	return true;
}

template<unsigned int Length>
inline bool tuple_length(const datalog_expression& src, datalog_expression& dst) {
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		if (!init(dst.tuple, POSITION_EXACT, Length)) return false;
		for (unsigned int i = 0; i < Length; i++)
			dst.tuple.elements[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += Length;
		dst.tuple.elements.length = Length;
		dst.type = DATALOG_TUPLE;
		dst.reference_count = 1;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length > Length) return false;
		if (!init(dst.tuple, POSITION_EXACT, Length)) return false;
		if (src.tuple.position == POSITION_LEFT) {
			for (unsigned int i = 0; i < src.tuple.elements.length; i++) {
				dst.tuple.elements[i] = src.tuple.elements[i];
				src.tuple.elements[i]->reference_count++;
			} for (unsigned int i = src.tuple.elements.length; i < Length; i++)
				dst.tuple.elements[i] = &DATALOG_ANY_TREE;
			DATALOG_ANY_TREE.reference_count += (Length - src.tuple.elements.length);
		} else if (src.tuple.position == POSITION_RIGHT) {
			for (unsigned int i = 0; i < Length - src.tuple.elements.length; i++)
				dst.tuple.elements[i] = &DATALOG_ANY_TREE;
			unsigned int offset = Length - src.tuple.elements.length;
			for (unsigned int i = offset; i < Length; i++) {
				dst.tuple.elements[i] = src.tuple.elements[i - offset];
				src.tuple.elements[i - offset]->reference_count++;
			}
			DATALOG_ANY_TREE.reference_count += offset;
		} else {
			if (src.tuple.elements.length != Length) {
				free(dst.tuple);
				return false;
			}
			for (unsigned int i = 0; i < Length; i++) {
				dst.tuple.elements[i] = src.tuple.elements[i];
				src.tuple.elements[i]->reference_count++;
			}
		}
		dst.tuple.elements.length = Length;
		dst.type = DATALOG_TUPLE;
		dst.reference_count = 1;
	} else {
		return false;
	}
	return true;
}

inline bool flip_predicate(datalog_predicate& pred) {
	if (pred.args[0] == NULL || pred.args[1] == NULL
	 || (pred.args[0]->type != DATALOG_ANY && pred.args[0]->type != DATALOG_NON_EMPTY && pred.args[0]->type != DATALOG_VARIABLE)
	 || (pred.args[1]->type != DATALOG_ANY && pred.args[1]->type != DATALOG_NON_EMPTY && pred.args[1]->type != DATALOG_VARIABLE))
		return false;
	for (unsigned int i = 2; i < array_length(pred.args); i++) {
		if (pred.args[i] == NULL) continue;
		if (!can_be_empty(*pred.args[i]))
			return false;
		free(*pred.args[i]);
		if (pred.args[i]->reference_count == 0)
			free(pred.args[i]);
		pred.args[i] = NULL;
	}
	swap(pred.args[0], pred.args[1]);
	return true;
}

template<bool InsideTuple = false>
inline bool flip_predicate(datalog_expression& exp) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (InsideTuple) {
			exp.type = DATALOG_PREDICATE;
			exp.reference_count = 1;
		} else {
			if (!init_tuple(exp, POSITION_LEFT, 1)
			 || !new_expression(exp.tuple.elements[0])) return false;
			exp.tuple.elements[0]->type = DATALOG_PREDICATE;
			exp.tuple.elements[0]->reference_count = 1;
			exp.tuple.elements.length = 1;
		}

		datalog_predicate& pred = InsideTuple ? exp.pred : exp.tuple.elements[0]->pred;
		pred.function = DATALOG_LABEL_WILDCARD;
		pred.excluded_count = 0;
		pred.args[0] = &DATALOG_ANY_TREE;
		pred.args[1] = &DATALOG_ANY_TREE;
		for (unsigned int i = 2; i < array_length(pred.args); i++)
			pred.args[i] = NULL;
		DATALOG_ANY_TREE.reference_count += 2;
		return true;
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position == POSITION_RIGHT) {
			return false;
		} else if (exp.tuple.elements.length == 0) {
			if (exp.tuple.position == POSITION_EXACT) return false;
			else if (!exp.tuple.elements.ensure_capacity(1)
				  || !new_expression(exp.tuple.elements[0])) exit(EXIT_FAILURE);
			exp.tuple.elements[0]->type = DATALOG_PREDICATE;
			exp.tuple.elements[0]->reference_count = 1;
			exp.tuple.elements.length = 1;

			datalog_predicate& pred = exp.tuple.elements[0]->pred;
			pred.function = DATALOG_LABEL_WILDCARD;
			pred.excluded_count = 0;
			pred.args[0] = &DATALOG_ANY_TREE;
			pred.args[1] = &DATALOG_ANY_TREE;
			for (unsigned int i = 2; i < array_length(pred.args); i++)
				pred.args[i] = NULL;
			DATALOG_ANY_TREE.reference_count += 2;
			return true;
		} else {
			datalog_expression* arg;
			if (exp.tuple.elements[0]->reference_count == 1) {
				arg = exp.tuple.elements[0];
			} else {
				if (!init(arg, *exp.tuple.elements[0])) exit(EXIT_FAILURE);
				free(*exp.tuple.elements[0]);
				exp.tuple.elements[0] = arg;
			}
			return flip_predicate<true>(*arg);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		return flip_predicate(exp.pred);
	} else {
		return false;
	}
}

bool apply(datalog_expression_root::function function, const datalog_expression_root& src, datalog_expression_root& dst)
{
	/* first apply the syntactic features */
	if (function.type == datalog_expression_root::FUNCTION_NULL
	 || function.type == datalog_expression_root::FUNCTION_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES) {
		dst.index = NUMBER_ALL;
		dst.concord = NUMBER_NONE;
		dst.inf = INFLECTION_NONE;
	} else if (function.type == datalog_expression_root::FUNCTION_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR) {
		dst.index = NUMBER_SINGULAR;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL) {
		dst.index = NUMBER_PLURAL;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_UNCOUNTABLE) {
		dst.index = NUMBER_UNCOUNTABLE;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR) {
		dst.index = src.index;
		dst.concord = NUMBER_SINGULAR;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL) {
		dst.index = src.index;
		dst.concord = NUMBER_PLURAL;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE) {
		dst.index = src.index;
		dst.concord = NUMBER_UNCOUNTABLE;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR) {
		dst.index = src.index;
		dst.concord = NUMBER_NON_SINGULAR;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL) {
		dst.index = src.index;
		dst.concord = NUMBER_NON_PLURAL;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR) {
		if (src.index != NUMBER_SINGULAR) return false;
		dst.index = src.index;
		dst.concord = NUMBER_NON_PLURAL;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_ALL) {
		if (src.index == NUMBER_SINGULAR) return false;
		dst.index = src.index;
		dst.concord = NUMBER_ALL;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR) {
		if (src.concord != NUMBER_SINGULAR && src.concord != NUMBER_NON_PLURAL
		 && src.concord != NUMBER_ALL && src.concord != NUMBER_ANY) return false;
		dst.index = src.index;
		dst.concord = NUMBER_SINGULAR;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL) {
		if (src.concord != NUMBER_PLURAL && src.concord != NUMBER_NON_SINGULAR
		 && src.concord != NUMBER_ALL && src.concord != NUMBER_ANY) return false;
		dst.index = src.index;
		dst.concord = NUMBER_PLURAL;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE) {
		if (src.concord != NUMBER_UNCOUNTABLE
		 && src.concord != NUMBER_NON_SINGULAR && src.concord != NUMBER_NON_PLURAL
		 && src.concord != NUMBER_ALL && src.concord != NUMBER_ANY) return false;
		dst.index = src.index;
		dst.concord = NUMBER_UNCOUNTABLE;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_SINGULAR) {
		if (src.index != NUMBER_SINGULAR && src.index != NUMBER_NON_PLURAL && src.index != NUMBER_ANY) return false;
		dst.index = NUMBER_SINGULAR;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL) {
		if (src.index != NUMBER_PLURAL && src.index != NUMBER_NON_SINGULAR && src.index != NUMBER_ANY) return false;
		dst.index = NUMBER_PLURAL;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE) {
		if (src.index != NUMBER_UNCOUNTABLE && src.index != NUMBER_NON_SINGULAR
		 && src.index != NUMBER_NON_PLURAL && src.index != NUMBER_ANY) return false;
		dst.index = NUMBER_UNCOUNTABLE;
		dst.concord = src.concord;
		dst.inf = src.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE) {
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = INFLECTION_PRESENT_PARTICIPLE;
	} else if (function.type == datalog_expression_root::FUNCTION_PAST_PARTICIPLE
			|| function.type == datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE) {
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = INFLECTION_PAST_PARTICIPLE;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE) {
		if (src.inf != INFLECTION_PRESENT_PARTICIPLE && src.inf != INFLECTION_ANY && src.inf != INFLECTION_NONE) return false;
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = INFLECTION_PRESENT_PARTICIPLE;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE
			|| function.type == datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE) {
		if (src.inf != INFLECTION_PAST_PARTICIPLE && src.inf != INFLECTION_ANY && src.inf != INFLECTION_NONE) return false;
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = INFLECTION_PAST_PARTICIPLE;
	} else if (function.type == datalog_expression_root::FUNCTION_INFINITIVE
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE) {
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = INFLECTION_INFINITIVE;
	} else if (function.type == datalog_expression_root::FUNCTION_IDENTITY_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD) {
		if (src.concord == NUMBER_NONE || (src.index == NUMBER_SINGULAR && src.concord == NUMBER_PLURAL))
			return false;
		dst.concord = src.concord;
		dst.index = src.concord;
		dst.inf = src.inf;
	} else {
		dst.index = src.index;
		dst.concord = src.concord;
		dst.inf = src.inf;
	}

	dst.root.reference_count = 1;
	switch (function.type) {
	case datalog_expression_root::FUNCTION_IDENTITY:
	case datalog_expression_root::FUNCTION_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SINGULAR:
	case datalog_expression_root::FUNCTION_PLURAL:
	case datalog_expression_root::FUNCTION_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_ALL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_INFINITIVE:
	case datalog_expression_root::FUNCTION_IDENTITY_COORD:
		return init(dst.root, src.root);
	case datalog_expression_root::FUNCTION_NULL:
	case datalog_expression_root::FUNCTION_KEEP_FEATURES:
		dst.root.type = DATALOG_EMPTY;
		return true;
	case datalog_expression_root::FUNCTION_SELECT_LEFT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD:
		return select_left<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR:
		return select_left<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DISJOINT:
		return select_left<3, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT4_DISJOINT:
		return select_left<4, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DISJOINT:
		return select_left<5, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT6_DISJOINT:
		return select_left<6, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT7_DISJOINT:
		return select_left<7, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD:
		return select_left<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT:
		return select_left_keep_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT:
		return select_left_keep_function<2, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_KEEP_FUNCTION:
		return select_left_keep_function<3, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES:
		return select_left_delete_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER:
		return select_left_delete_answer<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_ANSWER:
		return select_left_delete_answer<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT:
		return select_left_delete_answer<3, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT:
		return select_left_delete_answer<5, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD:
		return select_left_delete_answer<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER:
		return select_left_delete_function_answer<1, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER:
		return select_left_delete_function_answer<2, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER:
		return select_left_delete_function_answer<3, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER:
		return select_left_delete_function_answer<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER:
		return select_left_delete_function_answer<2, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER:
		return select_left_delete_function_answer<3, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES:
		return select_left_delete_function<1, PREDICATE_NOT, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT:
		return select_right<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DISJOINT:
		return select_right<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT3_DISJOINT:
		return select_right<3, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT4_DISJOINT:
		return select_right<4, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT5_DISJOINT:
		return select_right<5, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT6_DISJOINT:
		return select_right<6, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT7_DISJOINT:
		return select_right<7, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_HEAD:
		return select_right<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_FUNCTION:
		return select_right_delete_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DELETE_ANSWER:
		return select_right_delete_answer<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES:
		return select_function<DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES:
		return select_function<DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);

	case datalog_expression_root::FUNCTION_DELETE_LEFT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COORD:
		return delete_left<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD:
		return delete_left<1, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2:
		return delete_left<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES:
		return delete_left<2, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES:
		return delete_left<3, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT4_DISJOINT:
		return delete_left<4, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_DISJOINT:
		return delete_left<5, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT6_DISJOINT:
		return delete_left<6, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT7_DISJOINT:
		return delete_left<7, true, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES:
		return delete_left<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES:
		return delete_left<2, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT:
		return delete_left_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT:
		return delete_left_function<2, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_HEAD:
		return delete_left_function<1, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_HEAD:
		return delete_left_function<3, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_ANSWER:
		return delete_left_keep_function<1, PREDICATE_ANSWER, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION:
		return delete_left_keep_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION:
		return delete_left_keep_function<1, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT:
		return delete_left_keep_function<1, PREDICATE_NOT, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER:
		return delete_left_answer<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER:
		return delete_left_answer<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT:
		return delete_left_answer<3, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT:
		return delete_left_answer<5, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD:
		return delete_left_answer<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COUNT_ANSWER:
		return delete_left_function_answer<1, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_COUNT_ANSWER:
		return delete_left_function_answer<2, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_COUNT_ANSWER:
		return delete_left_function_answer<3, PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_ANSWER:
		return delete_left_function_answer<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER:
		return delete_left_function_answer<2, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER:
		return delete_left_function_answer<3, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION:
		return delete_left_answer_keep_function<1, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION:
		return delete_left_answer_keep_function<1, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION:
		return delete_left_answer_keep_function<2, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION:
		return delete_left_answer_keep_function<2, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION:
		return delete_left_answer_keep_function<3, DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION:
		return delete_left_answer_keep_function<3, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT:
		return delete_right<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2:
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_DISJOINT:
		return delete_right<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT3_DISJOINT:
		return delete_right<3, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT4_DISJOINT:
		return delete_right<4, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT5_DISJOINT:
		return delete_right<5, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT6_DISJOINT:
		return delete_right<6, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT7_DISJOINT:
		return delete_right<7, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD:
		return delete_right<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION:
		return delete_right_keep_function<1, DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_ANSWER:
		return delete_right_answer<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT:
		return delete_function<PREDICATE_COUNT, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL:
		return delete_function<PREDICATE_COUNT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT_ANSWER:
		return delete_function_answer<PREDICATE_COUNT, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_NOT:
	case datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL:
		return delete_function<PREDICATE_NOT,true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES:
		return delete_left_function<1, PREDICATE_NOT, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES:
		return delete_function<DATALOG_LABEL_WILDCARD, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_HEAD:
		return delete_function<DATALOG_LABEL_WILDCARD, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ANSWER:
		return delete_answer(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ANSWER_HAS_LOC:
		if (!has_predicate<PREDICATE_LOC>(src.root))
			return false;
		return delete_answer(src.root, dst.root);

	case datalog_expression_root::FUNCTION_SELECT_ARG1:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES:
		return select_arg<0, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES:
		return select_arg<0, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG2:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES:
		return select_arg<1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES:
		return select_arg<1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG3:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES:
		return select_arg<2, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES:
		return select_arg<2, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG1:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES:
		return delete_arg<0>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG2:
	case datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES:
		return delete_arg<1>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG3:
		return delete_arg<2>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_DELETE_ARGS:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL:
		return delete_args(src.root, dst.root);
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2:
		return head_arg_select_arg<0, 1, false>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY:
		return head_arg_select_arg<0, 1, true>(src.root, dst.root);
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY || src.root.type == DATALOG_TUPLE
		 || src.root.type == DATALOG_PREDICATE || src.root.type == DATALOG_FUNCTION) {
			if (!init(dst.root.tuple, POSITION_EXACT, 1)) return false;
			dst.root.type = DATALOG_TUPLE;
			dst.root.reference_count = 1;
		} else return false;
		return true;
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY:
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY || src.root.type == DATALOG_EMPTY
		 || (src.root.type == DATALOG_TUPLE && src.root.tuple.elements.length == 0)) {
			if (!init(dst.root.tuple, POSITION_EXACT, 1)) return false;
			dst.root.type = DATALOG_TUPLE;
			dst.root.reference_count = 1;
		} else return false;
		return true;
	case datalog_expression_root::FUNCTION_KEEP_NULL:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY || src.root.type == DATALOG_EMPTY) {
			dst.root.type = DATALOG_EMPTY;
			dst.root.reference_count = 1;
		} else return false;
		return true;
	case datalog_expression_root::FUNCTION_EMPTY_ARG2:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY) {
			dst.root.pred.function = DATALOG_LABEL_WILDCARD;
			dst.root.pred.excluded_count = 0;
			for (unsigned int i = 0; i < array_length(dst.root.pred.args); i++)
				if (i != 1) dst.root.pred.args[i] = &DATALOG_ANY_TREE;
			dst.root.pred.args[1] = NULL;
			DATALOG_ANY_TREE.reference_count += array_length(dst.root.pred.args) - 1;
		} else if (src.root.type == DATALOG_PREDICATE) {
			if (src.root.pred.args[1] != NULL && !can_be_empty(*src.root.pred.args[1]))
				return false;
			if (!init(dst.root.pred, src.root.pred)) return false;
			if (dst.root.pred.args[1] != NULL) {
				free(*dst.root.pred.args[1]);
				if (dst.root.pred.args[1]->reference_count == 0)
					free(dst.root.pred.args[1]);
				dst.root.pred.args[1] = NULL;
			}
		} else return false;
		dst.root.type = DATALOG_PREDICATE;
		dst.root.reference_count = 1;
		return true;
	case datalog_expression_root::FUNCTION_EMPTY_ARGS:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY) {
			dst.root.pred.function = DATALOG_LABEL_WILDCARD;
			dst.root.pred.excluded_count = 0;
		} else if (src.root.type == DATALOG_PREDICATE) {
			for (unsigned int i = 0; i < array_length(src.root.pred.args); i++) {
				if (src.root.pred.args[i] != NULL && !can_be_empty(*src.root.pred.args[i]))
					return false;
			}
			dst.root.pred.function = src.root.pred.function;
			if (src.root.pred.excluded_count > 0
			 && !init_excluded(dst.root.pred.excluded, src.root.pred.excluded, src.root.pred.excluded_count))
				return false;
			dst.root.pred.excluded_count = src.root.pred.excluded_count;
		} else return false;
		for (unsigned int i = 0; i < array_length(dst.root.pred.args); i++)
			dst.root.pred.args[i] = NULL;
		dst.root.type = DATALOG_PREDICATE;
		dst.root.reference_count = 1;
		return dst.root.pred.function != DATALOG_LABEL_WILDCARD
			|| dst.root.pred.exclude(&DATALOG_LABEL_EMPTY, 1);
	case datalog_expression_root::FUNCTION_ARG2_ZERO_ARITY:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY) {
			dst.root.pred.function = DATALOG_LABEL_WILDCARD;
			dst.root.pred.excluded_count = 0;
			if (!new_expression(dst.root.pred.args[1])) return false;
			datalog_expression& arg = *dst.root.pred.args[1];
			arg.type = DATALOG_PREDICATE;
			arg.reference_count = 1;
			arg.pred.function = DATALOG_LABEL_WILDCARD;
			arg.pred.excluded_count = 0;
			for (unsigned int i = 0; i < array_length(arg.pred.args); i++)
				arg.pred.args[i] = NULL;
			for (unsigned int i = 0; i < array_length(dst.root.pred.args); i++)
				if (i != 1) dst.root.pred.args[i] = &DATALOG_ANY_TREE;
			DATALOG_ANY_TREE.reference_count += array_length(dst.root.pred.args) - 1;
		} else if (src.root.type == DATALOG_PREDICATE) {
			if (src.root.pred.args[1] == NULL) {
				dst.root.pred.args[1] = NULL;
			} else if (src.root.pred.args[1]->type == DATALOG_ANY) {
				if (!new_expression(dst.root.pred.args[1])) return false;
				datalog_expression& arg = *dst.root.pred.args[1];
				arg.type = DATALOG_PREDICATE;
				arg.reference_count = 1;
				arg.pred.function = DATALOG_LABEL_WILDCARD;
				arg.pred.excluded_count = 0;
				for (unsigned int i = 0; i < array_length(arg.pred.args); i++)
					arg.pred.args[i] = NULL;
			} else if (src.root.pred.args[1]->type == DATALOG_NON_EMPTY) {
				if (!new_expression(dst.root.pred.args[1])) return false;
				datalog_expression& arg = *dst.root.pred.args[1];
				arg.type = DATALOG_PREDICATE;
				arg.reference_count = 1;
				arg.pred.function = DATALOG_LABEL_WILDCARD;
				arg.pred.excluded_count = 0;
				for (unsigned int i = 0; i < array_length(arg.pred.args); i++)
					arg.pred.args[i] = NULL;
				if (!init_excluded(arg.pred.excluded, &DATALOG_LABEL_EMPTY, 1)) return false;
				arg.pred.excluded_count = 1;
			} else if (src.root.pred.args[1]->type == DATALOG_PREDICATE) {
				datalog_expression& src_arg = *src.root.pred.args[1];
				for (unsigned int i = 0; i < array_length(src_arg.pred.args); i++) {
					if (src_arg.pred.args[i] != NULL && !can_be_empty(*src_arg.pred.args[i]))
						return false;
				}

				if (!new_expression(dst.root.pred.args[1])) return false;
				datalog_expression& dst_arg = *dst.root.pred.args[1];
				dst_arg.type = DATALOG_PREDICATE;
				dst_arg.reference_count = 1;
				dst_arg.pred.function = src_arg.pred.function;
				if (src_arg.pred.excluded_count > 0
				 && !init_excluded(dst_arg.pred.excluded, src_arg.pred.excluded, src_arg.pred.excluded_count)) {
					free(dst.root.pred.args[1]); return false;
				}
				dst_arg.pred.excluded_count = src_arg.pred.excluded_count;
				for (unsigned int i = 0; i < array_length(dst_arg.pred.args); i++)
					dst_arg.pred.args[i] = NULL;
			} else {
				return false;
			}

			/* initialize the root predicate */
			dst.root.pred.function = src.root.pred.function;
			if (src.root.pred.excluded_count > 0
			 && !init_excluded(dst.root.pred.excluded, src.root.pred.excluded, src.root.pred.excluded_count)) {
				if (dst.root.pred.args[1] != NULL) {
					free(*dst.root.pred.args[1]);
					free(dst.root.pred.args[1]);
				}
				return false;
			}
			dst.root.pred.excluded_count = src.root.pred.excluded_count;
			for (unsigned int i = 0; i < array_length(src.root.pred.args); i++) {
				if (i == 1) continue;
				dst.root.pred.args[i] = src.root.pred.args[i];
				if (dst.root.pred.args[i] != NULL) dst.root.pred.args[i]->reference_count++;
			}
		} else {
			return false;
		}
		dst.root.type = DATALOG_PREDICATE;
		dst.root.reference_count = 1;
		return dst.root.pred.function != DATALOG_LABEL_WILDCARD
			|| dst.root.pred.exclude(&DATALOG_LABEL_EMPTY, 1);

	case datalog_expression_root::FUNCTION_LOC:
		if (!check_predicate_answer<PREDICATE_LOC>(src.root))
			return false;
		dst.root.type = DATALOG_EMPTY;
		dst.root.reference_count = 1;
		return true;
	case datalog_expression_root::FUNCTION_TWO_PREDICATES:
		return tuple_length<2>(src.root, dst.root);

	case datalog_expression_root::FUNCTION_FLIP_PREDICATE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE:
		if (!init(dst.root, src.root)) {
			return false;
		} else if (!flip_predicate(dst.root)) {
			free(dst); return false;
		}
		return true;

	case datalog_expression_root::FUNCTION_EMPTY:
		break;
	}
	fprintf(stderr, "apply ERROR: Unrecognized transformation function.\n");
	return false;
}

inline bool copy_array(
		const unsigned int* src, unsigned int src_length,
		unsigned int*& dst, unsigned int& dst_length)
{
	dst = (unsigned int*) malloc(sizeof(unsigned int) * src_length);
	if (dst == nullptr) {
		fprintf(stderr, "copy_array ERROR: Out of memory.\n");
		return false;
	}
	memcpy(dst, src, sizeof(unsigned int) * src_length);
	dst_length = src_length;
	return true;
}

template<bool HigherOrderOnly>
inline bool get_function(const datalog_expression& src,
		unsigned int& value, unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_PREDICATE || src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT)
			value = DATALOG_LABEL_WILDCARD;
		else value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else if (src.type == DATALOG_FUNCTION) {
		if (HigherOrderOnly) {
			if (src.func.arg != NULL) {
				if (src.func.arg->type == DATALOG_PREDICATE
				 || (src.func.arg->type == DATALOG_TUPLE && src.func.arg->tuple.elements.length > 0))
					return false;
			}
		}
		value = src.func.function;
		if (src.func.excluded_count != 0 && !copy_array(src.func.excluded, src.func.excluded_count, excluded, excluded_count))
			return false;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else {
		return false;
	}
	return true;
}

inline bool has_function(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_PREDICATE || src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT)
			value = DATALOG_LABEL_WILDCARD;
		else value = DATALOG_LABEL_EMPTY;
	} else if (src.type == DATALOG_FUNCTION) {
		value = DATALOG_TRUE;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
	} else {
		return false;
	}
	excluded_count = 0;
	return true;
}

/* NOTE: this function assumes 'named_functions' is sorted */
template<size_t N>
inline bool has_function(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count,
		const unsigned int (&named_functions)[N])
{
	if (src.type == DATALOG_PREDICATE || src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length == 0 && src.tuple.position != POSITION_EXACT)
			value = DATALOG_LABEL_WILDCARD;
		else value = DATALOG_LABEL_EMPTY;
	} else if (src.type == DATALOG_FUNCTION) {
		if (src.func.function == DATALOG_LABEL_WILDCARD) {
			unsigned int intersection_size = 0;
			auto count = [&](unsigned int i, unsigned int j) { intersection_size++; };
			set_intersect(count, src.func.excluded, src.func.excluded_count, named_functions, N);
			if (intersection_size == N)
				value = DATALOG_TRUE;
			else value = DATALOG_LABEL_WILDCARD;
		} else {
			for (unsigned int i = 0; i < N; i++) {
				if (src.func.function == named_functions[i]) {
					value = named_functions[i];
					excluded_count = 0;
					return true;
				}
			}
			value = DATALOG_TRUE;
		}
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
	} else {
		return false;
	}
	excluded_count = 0;
	return true;
}

inline bool get_predicate(
		const datalog_expression& term, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (term.type == DATALOG_PREDICATE) {
		if (term.pred.excluded_count != 0 && !copy_array(term.pred.excluded, term.pred.excluded_count, excluded, excluded_count))
			return false;
		value = term.pred.function;
	} else if (term.type == DATALOG_FUNCTION) {
		if (term.func.excluded_count != 0 && !copy_array(term.func.excluded, term.func.excluded_count, excluded, excluded_count))
			return false;
		value = term.func.function;
	} else if (term.type == DATALOG_ANY || term.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index, bool Preterminal, bool AllowFunction>
bool get_predicate(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	static_assert(Index == 0 || !Preterminal, "If Preterminal is true, Index must be zero");

	if (src.type == DATALOG_TUPLE) {
		if (Preterminal && src.tuple.elements.length > 1)
			return false;
		if (src.tuple.position == POSITION_RIGHT) {
			if (Preterminal) {
				if (src.tuple.elements.length == 0) {
					value = DATALOG_LABEL_WILDCARD;
					excluded_count = 0;
				} else {
					return get_predicate(*src.tuple.elements[0], value, excluded, excluded_count);
				}
			} else {
				value = DATALOG_LABEL_WILDCARD;
				excluded_count = 0;
			}
		} else if (Index >= src.tuple.elements.length) {
			value = (src.tuple.position == POSITION_LEFT) ? DATALOG_LABEL_WILDCARD : DATALOG_LABEL_EMPTY;
			excluded_count = 0;
		} else return get_predicate(*src.tuple.elements[Index], value, excluded, excluded_count);
	} else if (src.type == DATALOG_PREDICATE) {
		if (Index != 0) {
			value = DATALOG_LABEL_EMPTY;
			excluded_count = 0;
			return true;
		}
		if (src.pred.excluded_count != 0 && !copy_array(src.pred.excluded, src.pred.excluded_count, excluded, excluded_count))
			return false;
		value = src.pred.function;
	} else if (src.type == DATALOG_FUNCTION) {
		if (!AllowFunction) return false;
		return get_predicate<Index, Preterminal, true>(*src.func.arg, value, excluded, excluded_count);
	} else if (src.type == DATALOG_STRING) {
		value = DATALOG_STRING;
		excluded_count = 0;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
bool get_right_predicate(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_LEFT) {
			value = DATALOG_LABEL_WILDCARD;
			excluded_count = 0;
		} else if (Index >= src.tuple.elements.length) {
			value = (src.tuple.position == POSITION_RIGHT) ? DATALOG_LABEL_WILDCARD : DATALOG_LABEL_EMPTY;
			excluded_count = 0;
		} else return get_predicate(*src.tuple.elements[src.tuple.elements.length - Index - 1], value, excluded, excluded_count);
	} else if (src.type == DATALOG_PREDICATE) {
		if (Index != 0) {
			value = DATALOG_LABEL_EMPTY;
			excluded_count = 0;
			return true;
		}
		if (src.pred.excluded_count != 0 && !copy_array(src.pred.excluded, src.pred.excluded_count, excluded, excluded_count))
			return false;
		value = src.pred.function;
	} else if (src.type == DATALOG_FUNCTION) {
		return get_right_predicate<Index>(*src.func.arg, value, excluded, excluded_count);
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		return false;
	}
	return true;
}

template<bool Preterminal>
bool get_direction(const datalog_expression& src, unsigned int& value) {
	if (src.type == DATALOG_PREDICATE) {
		if (src.pred.args[2] != NULL && !can_be_empty(*src.pred.args[2])) {
			return false;
		} else if (src.pred.args[0] == NULL) {
			value = DIRECTION_NONE;
			return true;
		} else if (src.pred.args[0]->type == DATALOG_ANY || src.pred.args[0]->type == DATALOG_NON_EMPTY) {
			if (src.pred.args[1] == NULL) {
				value = DIRECTION_SELF;
			} else if (src.pred.args[1]->type == DATALOG_ANY || src.pred.args[1]->type == DATALOG_NON_EMPTY) {
				value = DATALOG_LABEL_WILDCARD;
			} else {
				return false; /* prune search paths where the excluded directions is supposed to be non-empty */
			}
			return true;
		} else if (src.pred.args[0]->type != DATALOG_VARIABLE) {
			return false;
		} else if (src.pred.args[1] == NULL) {
			value = DIRECTION_SELF; /* this predicate is unary */
			return true;
		} else if (src.pred.args[1]->type == DATALOG_ANY || src.pred.args[1]->type == DATALOG_NON_EMPTY) {
			return false; /* prune search paths where the excluded directions is supposed to be non-empty */
		} else if (!Preterminal && src.pred.args[1]->type == DATALOG_PREDICATE) {
			value = DIRECTION_PREDICATE;
			return true;
		} else if (!Preterminal && src.pred.args[1]->type == DATALOG_CONSTANT) {
			value = DIRECTION_CONSTANT;
			return true;
		} else if (src.pred.args[1]->type != DATALOG_VARIABLE) {
			return false;
		}

		unsigned int first_head = src.pred.args[0]->variable;
		unsigned int second_head = src.pred.args[1]->variable;
		if (first_head < second_head) value = DIRECTION_FORWARD;
		else if (first_head > second_head) value = DIRECTION_BACKWARD;
		else value = DIRECTION_BOTH;
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
	} else if (src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
	} else if (src.type == DATALOG_FUNCTION) {
		return get_direction<Preterminal>(*src.func.arg, value);
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_RIGHT) {
			return false; /* we prune this part of the search space */
		} else if (src.tuple.position == POSITION_LEFT) {
			if (src.tuple.elements.length == 0) {
				value = DATALOG_LABEL_WILDCARD;
			} else {
				return get_direction<Preterminal>(*src.tuple.elements[0], value);
			}
		} else {
			if (src.tuple.elements.length == 0) {
				value = DATALOG_LABEL_EMPTY;
			} else {
				return get_direction<Preterminal>(*src.tuple.elements[0], value);
			}
		}
	} else {
		return false;
	}
	return true;
}

template<bool ConstantOnly>
inline bool get_constant(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (src.type == DATALOG_PREDICATE) {
		if (ConstantOnly) return false;
		value = src.pred.function;
		if (src.pred.excluded_count != 0 && !copy_array(src.pred.excluded, src.pred.excluded_count, excluded, excluded_count))
			return false;
	} else if (src.type == DATALOG_STRING && src.str.length > 0) {
		if (ConstantOnly) return false;
		value = DATALOG_STRING;
		excluded_count = 0;
	} else if (src.type != DATALOG_CONSTANT) {
		if (ConstantOnly) return false;
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		value = src.constant.label;
		if (src.constant.excluded_count != 0 && !copy_array(src.constant.excluded, src.constant.excluded_count, excluded, excluded_count))
			return false;
	}
	return true;
}

inline bool get_predicate_arity(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	static unsigned int excluded_arities_zero_one[] = { ARITY_TWO, ARITY_THREE, ARITY_NULL };
	static unsigned int excluded_arities_zero_two[] = { ARITY_THREE, ARITY_NULL };
	static unsigned int excluded_arities_zero_three[] = { ARITY_NULL };
	static unsigned int excluded_arities_one_two[] = { ARITY_ZERO, ARITY_THREE, ARITY_NULL };
	static unsigned int excluded_arities_one_three[] = { ARITY_ZERO, ARITY_NULL };
	static unsigned int excluded_arities_two_three[] = { ARITY_ZERO, ARITY_ONE, ARITY_NULL };

	excluded_count = 0;
	if (src.type == DATALOG_TUPLE) {
		if (src.tuple.elements.length == 0) {
			if (src.tuple.position == POSITION_EXACT)
				value = DATALOG_LABEL_EMPTY;
			else value = DATALOG_LABEL_WILDCARD; /* the arity could either be DATALOG_LABEL_EMPTY, ARITY_NULL, or the arity of the singleton predicate */
		} else if (src.tuple.elements.length == 1) {
			if (src.tuple.position == POSITION_EXACT)
				return get_predicate_arity(*src.tuple.elements[0], value, excluded, excluded_count);
			else value = DATALOG_LABEL_WILDCARD; /* the arity could be ARITY_NULL or the arity of the singleton predicate */
		} else value = ARITY_NULL;
	} else if (src.type == DATALOG_PREDICATE) {
		unsigned int min_arity = array_length(src.pred.args);
		unsigned int max_arity = array_length(src.pred.args);
		while (min_arity > 0) {
			if (src.pred.args[min_arity - 1] != NULL
			 && !can_be_empty(*src.pred.args[min_arity - 1]))
				break;
			min_arity--;
		} while (max_arity > min_arity) {
			if (src.pred.args[max_arity - 1] != NULL) break;
			max_arity--;
		}

		if (min_arity > 3 || max_arity > 3) {
			fprintf(stderr, "get_feature ERROR: Unrecognized arity.\n");
			return false;
		}

		value = DATALOG_LABEL_WILDCARD;
		if (min_arity == 0) {
			if (max_arity == 0) value = ARITY_ZERO;
			else if (max_arity == 1) {
				if (!copy_array(excluded_arities_zero_one, array_length(excluded_arities_zero_one), excluded, excluded_count))
					return false;
			} else if (max_arity == 2) {
				if (!copy_array(excluded_arities_zero_two, array_length(excluded_arities_zero_two), excluded, excluded_count))
					return false;
			} else if (max_arity == 3) {
				if (!copy_array(excluded_arities_zero_three, array_length(excluded_arities_zero_three), excluded, excluded_count))
					return false;
			}
		} else if (min_arity == 1) {
			if (max_arity == 1) value = ARITY_ONE;
			else if (max_arity == 2) {
				if (!copy_array(excluded_arities_one_two, array_length(excluded_arities_one_two), excluded, excluded_count))
					return false;
			} else if (max_arity == 3) {
				if (!copy_array(excluded_arities_one_three, array_length(excluded_arities_one_three), excluded, excluded_count))
					return false;
			}
		} else if (min_arity == 2) {
			if (max_arity == 2) value = ARITY_TWO;
			else if (max_arity == 3) {
				if (!copy_array(excluded_arities_two_three, array_length(excluded_arities_two_three), excluded, excluded_count))
					return false;
			}
		} else if (min_arity == 3) {
			if (max_arity == 3) value = ARITY_THREE;
		}
	} else if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
	} else if (src.type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index, bool ConstantOnly>
inline bool get_arg_constant(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (!ConstantOnly && src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_RIGHT) {
			return false; /* prune this region of the search */
		} else if (src.tuple.elements.length == 0) {
			if (src.tuple.position == POSITION_EXACT)
				value = ARG_OTHER;
			else value = DATALOG_LABEL_WILDCARD;
		} else {
			return get_arg_constant<Index, ConstantOnly>(*src.tuple.elements[0], value, excluded, excluded_count);
		}
		excluded_count = 0;
	} else if (src.type != DATALOG_PREDICATE) {
		if (ConstantOnly) return false;
		value = ARG_OTHER;
		excluded_count = 0;
	} else if (src.pred.args[Index] == NULL || src.pred.args[Index]->type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		return get_constant<ConstantOnly>(*src.pred.args[Index], value, excluded, excluded_count);
	}
	return true;
}

inline bool get_string(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
	} else if (src.type == DATALOG_STRING) {
		if (src.str.length != 1) return false;
		value = src.str.tokens[0];
	} else {
		return false;
	}
	excluded_count = 0;
	return true;
}

template<unsigned int Index>
inline bool get_arg_string(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (src.type != DATALOG_PREDICATE) {
		return false;
	} else if (src.pred.args[Index] == NULL || src.pred.args[Index]->type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		return get_string(*src.pred.args[Index], value, excluded, excluded_count);
	}
	return true;
}

template<unsigned int Index>
inline bool get_arg_arity(
		const datalog_expression& src, unsigned int& value,
		unsigned int*& excluded, unsigned int& excluded_count)
{
	if (src.type == DATALOG_ANY || src.type == DATALOG_NON_EMPTY) {
		value = DATALOG_LABEL_WILDCARD;
		excluded_count = 0;
	} else if (src.type == DATALOG_TUPLE) {
		if (src.tuple.position == POSITION_RIGHT) {
			return false; /* prune this region of the search */
		} else if (src.tuple.elements.length == 0) {
			if (src.tuple.position == POSITION_EXACT)
				value = ARG_OTHER;
			else value = DATALOG_LABEL_WILDCARD;
		} else {
			return get_arg_arity<Index>(*src.tuple.elements[0], value, excluded, excluded_count);
		}
		excluded_count = 0;
	} else if (src.type != DATALOG_PREDICATE) {
		value = ARG_OTHER;
		excluded_count = 0;
	} else if (src.pred.args[Index] == NULL || src.pred.args[Index]->type == DATALOG_EMPTY) {
		value = DATALOG_LABEL_EMPTY;
		excluded_count = 0;
	} else {
		return get_predicate_arity(*src.pred.args[Index], value, excluded, excluded_count);
	}
	return true;
}

bool get_feature(
		datalog_expression_root::feature feature, const datalog_expression_root& src,
		unsigned int& value, unsigned int*& excluded, unsigned int& excluded_count)
{
	static constexpr unsigned int PREDICATES_NOT[] = {PREDICATE_NOT};
	static constexpr unsigned int PREDICATES_COUNT_NOT[] = {PREDICATE_COUNT, PREDICATE_NOT};
	static unsigned int NUMBERS_SINGULAR[] = {NUMBER_OFFSET + NUMBER_SINGULAR - 1};
	static unsigned int NUMBERS_PLURAL[] = {NUMBER_OFFSET + NUMBER_PLURAL - 1};

	switch (feature) {
	case datalog_expression_root::FEATURE_FUNCTION:
		return get_function<false>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_FUNCTION_ONLY:
		return get_function<true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_FUNCTION_ANSWER:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY) {
			value = DATALOG_LABEL_WILDCARD;
			excluded_count = 0;
			return true;
		} else if (src.root.type == DATALOG_FUNCTION) {
			if (src.root.func.function == DATALOG_LABEL_WILDCARD) {
				if (src.root.func.is_excluded(PREDICATE_ANSWER)) return false;
			} else if (src.root.func.function != PREDICATE_ANSWER)
				return false;
			return get_function<false>(*src.root.func.arg, value, excluded, excluded_count);
		} else return false;
	case datalog_expression_root::FEATURE_HAS_FUNCTION:
		return has_function(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_NOT:
		return has_function(src.root, value, excluded, excluded_count, PREDICATES_NOT);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT:
		return has_function(src.root, value, excluded, excluded_count, PREDICATES_COUNT_NOT);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER:
		if (src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY) {
			value = DATALOG_LABEL_WILDCARD;
			excluded_count = 0;
			return true;
		} else if (src.root.type == DATALOG_FUNCTION) {
			if (src.root.func.function == DATALOG_LABEL_WILDCARD) {
				if (src.root.func.is_excluded(PREDICATE_ANSWER)) return false;
			} else if (src.root.func.function != PREDICATE_ANSWER)
				return false;
			return has_function(*src.root.func.arg, value, excluded, excluded_count);
		} else return false;
	case datalog_expression_root::FEATURE_PREDICATE:
		return get_predicate<0, true, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_PREDICATE_ONLY:
		return get_predicate<0, true, false>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_FIRST_PREDICATE:
		return get_predicate<0, false, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_SECOND_PREDICATE:
		return get_predicate<1, false, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_THIRD_PREDICATE:
		return get_predicate<2, false, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_LAST_PREDICATE:
		return get_right_predicate<0>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_DIRECTION:
		excluded_count = 0;
		return get_direction<true>(src.root, value);
	case datalog_expression_root::FEATURE_DIRECTION_ROOT:
		excluded_count = 0;
		return get_direction<false>(src.root, value);
	case datalog_expression_root::FEATURE_CONSTANT:
		return get_constant<true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_PREDICATE_ARITY:
		return get_predicate_arity(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG1:
		return get_arg_constant<0, false>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG2:
		return get_arg_constant<1, false>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG3:
		return get_arg_constant<2, false>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG1_ONLY:
		return get_arg_constant<0, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG2_ONLY:
		return get_arg_constant<1, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG3_ONLY:
		return get_arg_constant<2, true>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG1_STRING:
		return get_arg_string<0>(src.root, value, excluded, excluded_count);
	case datalog_expression_root::FEATURE_ARG2_ARITY:
		return get_arg_arity<1>(src.root, value, excluded, excluded_count);

	case datalog_expression_root::FEATURE_NUMBER:
		if (src.concord == NUMBER_ANY) {
			value = DATALOG_LABEL_WILDCARD;
			excluded_count = 0;
		} else if (src.concord == NUMBER_NON_SINGULAR) {
			value = DATALOG_LABEL_WILDCARD;
			if (!copy_array(NUMBERS_SINGULAR, array_length(NUMBERS_SINGULAR), excluded, excluded_count))
				return false;
		} else if (src.concord == NUMBER_NON_PLURAL) {
			value = DATALOG_LABEL_WILDCARD;
			if (!copy_array(NUMBERS_PLURAL, array_length(NUMBERS_PLURAL), excluded, excluded_count))
				return false;
		} else {
			value = NUMBER_OFFSET + src.index - 1;
		}
		return true;
	case datalog_expression_root::FEATURE_INFLECTION:
		if (src.inf == INFLECTION_ANY)
			value = DATALOG_LABEL_WILDCARD;
		else value = INFLECTION_OFFSET + src.inf - 1;
		return true;

	case datalog_expression_root::FEATURE_NULL:
		break;
	}
	fprintf(stderr, "get_feature ERROR: Unrecognized semantic feature.\n");
	return false;
}

template<unsigned int Predicate, typename std::enable_if<
	Predicate == PREDICATE_COUNT || Predicate == PREDICATE_SUM>::type* = nullptr>
inline void set_function_variables(datalog_function& func) {
	func.vars[0] = 2; func.vars[1] = 1;
}

template<unsigned int Predicate, typename std::enable_if<
	Predicate == PREDICATE_ANSWER || Predicate == PREDICATE_HIGHEST
 || Predicate == PREDICATE_LOWEST || Predicate == PREDICATE_LONGEST
 || Predicate == PREDICATE_SHORTEST || Predicate == PREDICATE_LARGEST
 || Predicate == PREDICATE_SMALLEST>::type* = nullptr>
inline void set_function_variables(datalog_function& func) {
	func.vars[0] = 1; func.vars[1] = 0;
}

template<unsigned int Predicate, typename std::enable_if<
	Predicate == PREDICATE_MOST || Predicate == PREDICATE_FEWEST>::type* = nullptr>
inline void set_function_variables(datalog_function& func) {
	func.vars[0] = 1; func.vars[1] = 2;
}

template<unsigned int Predicate, typename std::enable_if<
	Predicate == PREDICATE_NOT || Predicate == DATALOG_LABEL_WILDCARD>::type* = nullptr>
inline void set_function_variables(datalog_function& func) {
	func.vars[0] = 0; func.vars[1] = 0;
}

inline void set_function_variables(datalog_function& func, unsigned int predicate) {
	switch (predicate) {
	case PREDICATE_COUNT:
	case PREDICATE_SUM:
		func.vars[0] = 2; func.vars[1] = 1;
		return;

	case PREDICATE_ANSWER:
	case PREDICATE_HIGHEST:
	case PREDICATE_LOWEST:
	case PREDICATE_LONGEST:
	case PREDICATE_SHORTEST:
	case PREDICATE_LARGEST:
	case PREDICATE_SMALLEST:
		if (variable_count(*func.arg) <= 1)
			func.vars[0] = 1;
		else func.vars[0] = 2;
		func.vars[1] = 0;
		return;

	case PREDICATE_MOST:
	case PREDICATE_FEWEST:
		func.vars[0] = 1; func.vars[1] = 2;
		return;

	case PREDICATE_NOT:
		func.vars[0] = 0; func.vars[1] = 0;
		return;

	default:
		fprintf(stderr, "set_function_variables ERROR: Unrecognized datalog_function predicate.\n");
		exit(EXIT_FAILURE);
	}
}

template<bool HigherOrderOnly>
bool set_function(datalog_expression& exp, unsigned int value)
{
	if (value == DATALOG_LABEL_EMPTY) {
		return (exp.type != DATALOG_FUNCTION);
	} else {
		if (exp.type == DATALOG_FUNCTION) {
			if (exp.func.function == DATALOG_LABEL_WILDCARD) {
				if (!HigherOrderOnly) return false;
				if (index_of(value, exp.func.excluded, exp.func.excluded_count) < exp.func.excluded_count)
					return false;
				if (exp.func.excluded_count > 0)
					free(exp.func.excluded);
				exp.func.excluded_count = 0;
				exp.func.function = value;
				set_function_variables(exp.func, value);
			} else if (exp.func.function != value) return false;
		} else {
			return false; /* prune parts of the search space where the function isn't known by this point */
		}
	}
	return true;
}

bool set_has_function(datalog_expression& exp, unsigned int value) {
	if (value == DATALOG_LABEL_EMPTY) {
		return (exp.type != DATALOG_FUNCTION);
	} else if (value == DATALOG_TRUE) {
		if (exp.type == DATALOG_ANY) {
			exp.func.function = DATALOG_LABEL_WILDCARD;
			exp.func.excluded_count = 0;
			exp.func.arg = &DATALOG_ANY_TREE;
			set_function_variables<DATALOG_LABEL_WILDCARD>(exp.func);
			DATALOG_ANY_TREE.reference_count++;
			exp.type = DATALOG_FUNCTION;
		} else return (exp.type == DATALOG_FUNCTION);
	} else {
		return false;
	}
	return true;
}

template<size_t N>
bool set_has_function(
		datalog_expression& exp, unsigned int value,
		const unsigned int (&named_functions)[N])
{
	if (value == DATALOG_LABEL_EMPTY)
		return (exp.type != DATALOG_FUNCTION);
	else if (exp.type != DATALOG_FUNCTION)
		return false;

	if (value == DATALOG_TRUE) {
		/* the expression is a function, but not among the list of named functions */
		if (exp.func.function != DATALOG_LABEL_WILDCARD) {
			return index_of(exp.func.function, named_functions, N) == N;
		} else if (exp.func.function == DATALOG_LABEL_WILDCARD
				&& !exp.func.exclude(named_functions, N)) {
			exit(EXIT_FAILURE);
		}
		return true;
	}

	for (unsigned int i = 0; i < N; i++)
		if (value == named_functions[i])
			return exp.func.function == value;

	for (unsigned int i = 0; i < N; i++)
		if (exp.func.function == named_functions[i])
			return false;
	return true;
}

bool set_predicate(datalog_expression& exp, unsigned int predicate) {
	if (predicate <= NUM_PREDICATES) {
		exp.func.function = predicate;
		exp.func.arg = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count++;
		exp.func.excluded_count = 0;
		set_function_variables<DATALOG_LABEL_WILDCARD>(exp.func); /* we don't know the head of the function variables */
		exp.type = DATALOG_FUNCTION;
	} else if (predicate == DATALOG_STRING) {
		if (!init(exp.str, 1)) return false;
		exp.str[0] = DATALOG_LABEL_WILDCARD;
		exp.type = DATALOG_STRING;
	} else {
		exp.pred.function = predicate;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
			exp.pred.args[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += array_length(exp.pred.args);
		exp.pred.excluded_count = 0;
		exp.type = DATALOG_PREDICATE;
	}
	return true;
}

template<unsigned int Index, bool Preterminal, bool AllowFunction>
bool set_predicate(datalog_expression& exp, unsigned int predicate)
{
	static_assert(Index == 0 || !Preterminal, "If Preterminal is true, Index must be zero");

	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (predicate == DATALOG_LABEL_EMPTY) {
			if (Index == 0 && exp.type == DATALOG_NON_EMPTY) return false;
			if (Index == 0) exp.type = DATALOG_EMPTY;
			return true;
		}

		if (Index > 0 || !Preterminal) {
			if (!array_init(exp.tuple.elements, Index + 1))
				exit(EXIT_FAILURE);
			exp.tuple.position = POSITION_LEFT;
			exp.type = DATALOG_TUPLE;
			for (unsigned int i = 0; i < Index; i++)
				exp.tuple.elements[i] = &DATALOG_ANY_TREE;
			DATALOG_ANY_TREE.reference_count += Index;
			if (!new_expression(exp.tuple.elements[Index]))
				exit(EXIT_FAILURE);
			exp.tuple.elements[Index]->reference_count = 1;
			exp.tuple.elements[Index]->type = DATALOG_ANY;
			exp.tuple.elements.length = Index + 1;
			return set_predicate(*exp.tuple.elements[Index], predicate);
		} else {
			return set_predicate(exp, predicate);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (Index != 0) {
			if (predicate == DATALOG_LABEL_EMPTY) return true;
			if (predicate <= NUM_PREDICATES) return false;
		} else if (predicate == DATALOG_LABEL_EMPTY) {
			if (exp.pred.is_excluded(DATALOG_LABEL_EMPTY))
				return false;
			free(exp);
			exp.type = DATALOG_EMPTY;
			exp.reference_count = 1;
			return true;
		}
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (exp.pred.is_excluded(predicate))
				return false;
			if (exp.pred.excluded_count > 0)
				free(exp.pred.excluded);
			exp.pred.excluded_count = 0;
			exp.pred.function = predicate;
		} else if (exp.pred.function != predicate) return false;
	} else if (exp.type == DATALOG_FUNCTION) {
		if (!AllowFunction) return false;
		datalog_expression* arg;
		if (exp.func.arg->reference_count == 1) {
			arg = exp.func.arg;
		} else {
			if (!init(arg, *exp.func.arg)) exit(EXIT_FAILURE);
			free(*exp.func.arg);
			exp.func.arg = arg;
		}
		return set_predicate<Index, Preterminal, true>(*arg, predicate);
	} else if (exp.type == DATALOG_TUPLE) {
		if (Preterminal && exp.tuple.elements.length > 1)
			return false;
		if (exp.tuple.position == POSITION_RIGHT) return false;
		else if (Index >= exp.tuple.elements.length) {
			if (exp.tuple.position == POSITION_LEFT) {
				if (predicate == DATALOG_LABEL_EMPTY) {
					if (!exp.tuple.elements.ensure_capacity(Index)) exit(EXIT_FAILURE);
					for (unsigned int i = exp.tuple.elements.length; i < Index; i++)
						exp.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count += Index - exp.tuple.elements.length;
					exp.tuple.elements.length = Index;
					exp.tuple.position = POSITION_EXACT;
					return true;
				} else {
					if (!exp.tuple.elements.ensure_capacity(Index + 1)) exit(EXIT_FAILURE);
					for (unsigned int i = exp.tuple.elements.length; i < Index + 1; i++)
						exp.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count += Index + 1 - exp.tuple.elements.length;
					exp.tuple.elements.length = Index + 1;
					if (Preterminal) exp.tuple.position = POSITION_EXACT;
				}
			} else return predicate == DATALOG_LABEL_EMPTY;
		} else if (predicate == DATALOG_LABEL_EMPTY)
			return false;

		if (Preterminal) exp.tuple.position = POSITION_EXACT;
		datalog_expression* element;
		if (exp.tuple.elements[Index]->reference_count == 1) {
			element = exp.tuple.elements[Index];
		} else {
			if (!init(element, *exp.tuple.elements[Index]))
				exit(EXIT_FAILURE);
			free(*exp.tuple.elements[Index]);
			exp.tuple.elements[Index] = element;
		}
		if (element->type == DATALOG_ANY || element->type == DATALOG_NON_EMPTY) {
			return set_predicate(*element, predicate);
		} else if (element->type == DATALOG_PREDICATE) {
			if (predicate <= NUM_PREDICATES) return false;
			if (element->pred.function == DATALOG_LABEL_WILDCARD) {
				if (element->pred.is_excluded(predicate)) return false;
				if (element->pred.excluded_count > 0)
					free(element->pred.excluded);
				element->pred.excluded_count = 0;
				element->pred.function = predicate;
			} else if (element->pred.function != predicate) return false;
		} else if (element->type == DATALOG_FUNCTION) {
			if (predicate > NUM_PREDICATES) return false;
			if (element->func.function == DATALOG_LABEL_WILDCARD) {
				if (element->func.is_excluded(predicate)) return false;
				if (element->func.excluded_count > 0)
					free(element->func.excluded);
				element->func.excluded_count = 0;
				element->func.function = predicate;
				set_function_variables(element->func, predicate);
			} else if (element->func.function != predicate) return false;
		} else {
			return false;
		}
	} else if (exp.type == DATALOG_STRING) {
		return predicate == DATALOG_STRING;
	} else if (exp.type == DATALOG_EMPTY) {
		return predicate == DATALOG_LABEL_EMPTY;
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
bool set_right_predicate(datalog_expression& exp, unsigned int predicate) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		return false; /* preterminals don't use this feature, so we can prune away this part of the search */
	} else if (exp.type == DATALOG_PREDICATE) {
		if (Index != 0) {
			if (predicate == DATALOG_LABEL_EMPTY) return true;
			if (predicate <= NUM_PREDICATES) return false;
		}
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (exp.pred.is_excluded(predicate))
				return false;
			if (exp.pred.excluded_count > 0)
				free(exp.pred.excluded);
			exp.pred.excluded_count = 0;
			exp.pred.function = predicate;
		} else if (exp.pred.function != predicate) return false;
	} else if (exp.type == DATALOG_FUNCTION) {
		return set_right_predicate<Index>(*exp.func.arg, predicate);
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position == POSITION_LEFT) return false;
		else if (Index >= exp.tuple.elements.length) {
			if (exp.tuple.position == POSITION_RIGHT) {
				if (predicate == DATALOG_LABEL_EMPTY) {
					if (!exp.tuple.elements.ensure_capacity(Index)) exit(EXIT_FAILURE);
					for (unsigned int i = 0; i < exp.tuple.elements.length; i++)
						exp.tuple.elements[Index - i - 1] = exp.tuple.elements[exp.tuple.elements.length - i - 1];
					for (unsigned int i = 0; i < Index - exp.tuple.elements.length; i++)
						exp.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count += Index - exp.tuple.elements.length;
					exp.tuple.elements.length = Index;
					exp.tuple.position = POSITION_EXACT;
				} else {
					if (!exp.tuple.elements.ensure_capacity(Index + 1)) exit(EXIT_FAILURE);
					for (unsigned int i = 0; i < exp.tuple.elements.length; i++)
						exp.tuple.elements[Index - i] = exp.tuple.elements[exp.tuple.elements.length - i - 1];
					for (unsigned int i = 0; i < Index + 1 - exp.tuple.elements.length; i++)
						exp.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count += Index + 1 - exp.tuple.elements.length;
					exp.tuple.elements.length = Index + 1;
				}
			} else return predicate == DATALOG_LABEL_EMPTY;
		} else if (predicate == DATALOG_LABEL_EMPTY)
			return false;

		datalog_expression* element;
		if (exp.tuple.elements[exp.tuple.elements.length - Index - 1]->reference_count == 1) {
			element = exp.tuple.elements[exp.tuple.elements.length - Index - 1];
		} else {
			if (!init(element, *exp.tuple.elements[exp.tuple.elements.length - Index - 1]))
				exit(EXIT_FAILURE);
			free(*exp.tuple.elements[exp.tuple.elements.length - Index - 1]);
			exp.tuple.elements[exp.tuple.elements.length - Index - 1] = element;
		}
		if (element->type == DATALOG_ANY || element->type == DATALOG_NON_EMPTY) {
			return set_predicate(*element, predicate);
		} else if (element->type == DATALOG_PREDICATE) {
			if (predicate <= NUM_PREDICATES) return false;
			if (element->pred.function == DATALOG_LABEL_WILDCARD) {
				if (element->pred.is_excluded(predicate)) return false;
				if (element->pred.excluded_count > 0)
					free(element->pred.excluded);
				element->pred.excluded_count = 0;
				element->pred.function = predicate;
			} else if (element->pred.function != predicate) return false;
		} else if (element->type == DATALOG_FUNCTION) {
			if (predicate > NUM_PREDICATES) return false;
			if (element->func.function == DATALOG_LABEL_WILDCARD) {
				if (!element->func.is_excluded(predicate)) return false;
				if (element->func.excluded_count > 0)
					free(element->func.excluded);
				element->func.excluded_count = 0;
				element->func.function = predicate;
				set_function_variables(element->func, predicate);
			} else if (element->func.function != predicate) return false;
		} else {
			return false;
		}
	} else if (exp.type == DATALOG_EMPTY) {
		return predicate == DATALOG_LABEL_EMPTY;
	} else {
		return false;
	}
	return true;
}

template<bool Preterminal>
inline bool set_direction(datalog_expression& exp, unsigned int direction)
{
	if (Preterminal && (direction == DIRECTION_PREDICATE || direction == DIRECTION_CONSTANT))
		return false;
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (direction == DATALOG_LABEL_EMPTY) {
			/* this is an empty expression or empty tuple */
			return true;
		}

		exp.pred.function = DATALOG_LABEL_WILDCARD;
		exp.pred.excluded_count = 0;
		exp.type = DATALOG_PREDICATE;
		exp.reference_count = 1;
		switch (direction) {
		case DIRECTION_NONE:
			for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
				exp.pred.args[i] = NULL;
			break;
		case DIRECTION_SELF:
			if (!new_expression(exp.pred.args[0])) exit(EXIT_FAILURE);
			exp.pred.args[0]->variable = 1;
			exp.pred.args[0]->type = DATALOG_VARIABLE;
			exp.pred.args[0]->reference_count = 1;
			for (unsigned int i = 1; i < array_length(exp.pred.args); i++)
				exp.pred.args[i] = NULL;
			break;
		case DIRECTION_FORWARD:
			if (!new_expression(exp.pred.args[0]) || !new_expression(exp.pred.args[1])) exit(EXIT_FAILURE);
			exp.pred.args[0]->variable = 1;
			exp.pred.args[0]->type = DATALOG_VARIABLE;
			exp.pred.args[0]->reference_count = 1;
			exp.pred.args[1]->variable = 2;
			exp.pred.args[1]->type = DATALOG_VARIABLE;
			exp.pred.args[1]->reference_count = 1;
			for (unsigned int i = 2; i < array_length(exp.pred.args); i++)
				exp.pred.args[i] = NULL;
			break;
		case DIRECTION_BACKWARD:
			if (!new_expression(exp.pred.args[0]) || !new_expression(exp.pred.args[1])) exit(EXIT_FAILURE);
			exp.pred.args[0]->variable = 2;
			exp.pred.args[0]->type = DATALOG_VARIABLE;
			exp.pred.args[0]->reference_count = 1;
			exp.pred.args[1]->variable = 1;
			exp.pred.args[1]->type = DATALOG_VARIABLE;
			exp.pred.args[1]->reference_count = 1;
			for (unsigned int i = 2; i < array_length(exp.pred.args); i++)
				exp.pred.args[i] = NULL;
			break;
		case DIRECTION_BOTH:
			if (!new_expression(exp.pred.args[0]) || !new_expression(exp.pred.args[1])) exit(EXIT_FAILURE);
			exp.pred.args[0]->variable = 1;
			exp.pred.args[0]->type = DATALOG_VARIABLE;
			exp.pred.args[0]->reference_count = 1;
			exp.pred.args[1]->variable = 1;
			exp.pred.args[1]->type = DATALOG_VARIABLE;
			exp.pred.args[1]->reference_count = 1;
			for (unsigned int i = 2; i < array_length(exp.pred.args); i++)
				exp.pred.args[i] = NULL;
			break;
		case DIRECTION_PREDICATE:
		case DIRECTION_CONSTANT:
			/* we prune this part of the search since this direction should
			   already be determined in the logical form by this point */
			return false;
		default:
			fprintf(stderr, "set_direction ERROR: Unrecognized edge direction.\n");
			exit(EXIT_FAILURE);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (direction == DATALOG_LABEL_EMPTY)
			return false;

		unsigned int boundary;
		if (direction == DIRECTION_NONE) {
			boundary = 0;
		} else if (exp.pred.args[0] == NULL) {
			return false;
		} else if (exp.pred.args[0]->type == DATALOG_ANY || exp.pred.args[0]->type == DATALOG_NON_EMPTY) {
			if (exp.pred.args[0]->reference_count > 1) {
				free(*exp.pred.args[0]);
				if (!new_expression(exp.pred.args[0])) exit(EXIT_FAILURE);
			}
			exp.pred.args[0]->type = DATALOG_VARIABLE;
			exp.pred.args[0]->reference_count = 1;
			exp.pred.args[0]->variable = 1;
			if (exp.pred.args[1] == NULL) {
				if (direction != DIRECTION_SELF) return false;
				boundary = 1;
			} else if (exp.pred.args[1]->type == DATALOG_ANY || exp.pred.args[1]->type == DATALOG_NON_EMPTY) {
				if (exp.pred.args[1]->reference_count > 1) {
					free(*exp.pred.args[1]);
					if (!new_expression(exp.pred.args[1])) exit(EXIT_FAILURE);
				}
				if (direction == DIRECTION_PREDICATE) {
					exp.pred.args[1]->type = DATALOG_PREDICATE;
					exp.pred.args[1]->reference_count = 1;
					exp.pred.args[1]->pred.function = DATALOG_LABEL_WILDCARD;
					exp.pred.args[1]->pred.excluded_count = 0;
					for (unsigned int i = 0; i < array_length(exp.pred.args[1]->pred.args); i++)
						exp.pred.args[1]->pred.args[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count += array_length(exp.pred.args[1]->pred.args);
					boundary = 2;
				} else if (direction == DIRECTION_CONSTANT) {
					exp.pred.args[1]->type = DATALOG_CONSTANT;
					exp.pred.args[1]->reference_count = 1;
					exp.pred.args[1]->constant.label = DATALOG_LABEL_WILDCARD;
					exp.pred.args[1]->constant.excluded_count = 0;
					boundary = 2;
				} else {
					exp.pred.args[1]->type = DATALOG_VARIABLE;
					exp.pred.args[1]->reference_count = 1;
					boundary = 2;
					switch (direction) {
					case DIRECTION_SELF:
						exp.pred.args[0]->variable = 1;
						free(*exp.pred.args[1]);
						if (exp.pred.args[1]->reference_count == 0) free(exp.pred.args[1]);
						exp.pred.args[1] = NULL;
						break;
					case DIRECTION_FORWARD:
						exp.pred.args[0]->variable = 1;
						exp.pred.args[1]->variable = 2;
						break;
					case DIRECTION_BACKWARD:
						exp.pred.args[0]->variable = 2;
						exp.pred.args[1]->variable = 1;
						break;
					case DIRECTION_BOTH:
						exp.pred.args[0]->variable = 1;
						exp.pred.args[1]->variable = 1;
						break;
					case DIRECTION_PREDICATE:
					case DIRECTION_CONSTANT:
						/* we prune this part of the search since this direction should
						   already be determined in the logical form by this point */
						return false;
					default:
						fprintf(stderr, "set_direction ERROR: Unrecognized edge direction.\n");
						exit(EXIT_FAILURE);
					}
				}
			} else if (exp.pred.args[1]->type == DATALOG_VARIABLE) {
				if (exp.pred.args[1]->reference_count > 1) {
					free(*exp.pred.args[1]);
					if (!init(exp.pred.args[1], *exp.pred.args[1])) exit(EXIT_FAILURE);
				}
				boundary = 2;
				switch (direction) {
				case DIRECTION_SELF:
					return false;
				case DIRECTION_FORWARD:
					if (Preterminal && exp.pred.args[1]->variable != 2) return false;
					exp.pred.args[0]->variable = 1;
					break;
				case DIRECTION_BACKWARD:
					if (Preterminal && exp.pred.args[1]->variable != 1) return false;
					exp.pred.args[0]->variable = 2;
					break;
				case DIRECTION_BOTH:
					if (Preterminal && exp.pred.args[1]->variable != 1) return false;
					exp.pred.args[0]->variable = 1;
					break;
				case DIRECTION_PREDICATE:
				case DIRECTION_CONSTANT:
					/* we prune this part of the search since this direction should
					   already be determined in the logical form by this point */
					return false;
				default:
					fprintf(stderr, "set_direction ERROR: Unrecognized edge direction.\n");
					exit(EXIT_FAILURE);
				}
			} else {
				return false;
			}
		} else if (exp.pred.args[0]->type == DATALOG_VARIABLE) {
			if (exp.pred.args[0]->reference_count > 1) {
				free(*exp.pred.args[0]);
				if (!init(exp.pred.args[0], *exp.pred.args[0])) exit(EXIT_FAILURE);
			}
			boundary = 2;
			if (exp.pred.args[1] != NULL && exp.pred.args[1]->type != DATALOG_VARIABLE) {
				if ((direction != DIRECTION_PREDICATE || exp.pred.args[1]->type != DATALOG_PREDICATE)
				 && (direction != DIRECTION_CONSTANT || exp.pred.args[1]->type != DATALOG_CONSTANT))
					return false;
			} else {
				switch (direction) {
				case DIRECTION_SELF:
					return (!Preterminal || exp.pred.args[0]->variable != 2);
				case DIRECTION_FORWARD:
					if (Preterminal && (exp.pred.args[0]->variable != 1 || exp.pred.args[1]->variable != 2)) return false;
					break;
				case DIRECTION_BACKWARD:
					if (Preterminal && (exp.pred.args[0]->variable != 2 || exp.pred.args[1]->variable != 1)) return false;
					break;
				case DIRECTION_BOTH:
					if (Preterminal && (exp.pred.args[0]->variable != 1 || exp.pred.args[1]->variable != 1)) return false;
					break;
				default:
					fprintf(stderr, "set_direction ERROR: Unrecognized edge direction.\n");
					exit(EXIT_FAILURE);
				}
			}
		} else {
			return false;
		}

		for (unsigned int i = boundary; i < array_length(exp.pred.args); i++) {
			if (exp.pred.args[i] == NULL) continue;
			if (!can_be_empty(*exp.pred.args[i]))
				return false;
			free(*exp.pred.args[i]);
			if (exp.pred.args[i]->reference_count == 0)
				free(exp.pred.args[i]);
			exp.pred.args[i] = NULL;
		}
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position == POSITION_RIGHT) {
			return false;
		} else {
			if (exp.tuple.elements.length == 0) {
				if (direction == DATALOG_LABEL_EMPTY) {
					exp.tuple.position = POSITION_EXACT;
				} else {
					if (!new_expression(exp.tuple.elements[0])) exit(EXIT_FAILURE);
					exp.tuple.elements[0]->type = DATALOG_ANY;
					exp.tuple.elements[0]->reference_count = 1;
					exp.tuple.elements.length = 1;
					return set_direction<Preterminal>(*exp.tuple.elements[0], direction);
				}
			} else {
				datalog_expression* arg;
				if (exp.tuple.elements[0]->reference_count == 1) {
					arg = exp.tuple.elements[0];
				} else {
					if (!init(arg, *exp.tuple.elements[0])) exit(EXIT_FAILURE);
					free(*exp.tuple.elements[0]);
					exp.tuple.elements[0] = arg;
				}
				return set_direction<Preterminal>(*arg, direction);
			}
		}
	} else if (exp.type == DATALOG_FUNCTION) {
		datalog_expression* arg;
		if (exp.func.arg->reference_count == 1) {
			arg = exp.func.arg;
		} else {
			if (!init(arg, *exp.func.arg)) exit(EXIT_FAILURE);
			free(*exp.func.arg);
			exp.func.arg = arg;
		}
		return set_direction<Preterminal>(*arg, direction);
	} else if (exp.type == DATALOG_EMPTY) {
		return (direction == DATALOG_LABEL_EMPTY);
	} else {
		return false;
	}
	return true;
}

template<bool ConstantOnly>
inline bool set_constant(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		exp.type = DATALOG_CONSTANT;
		exp.constant.label = value;
		exp.constant.excluded_count = 0;
	} else if (exp.type == DATALOG_CONSTANT) {
		if (exp.constant.label == DATALOG_LABEL_WILDCARD) {
			if (exp.constant.is_excluded(value)) return false;
			if (exp.constant.excluded_count > 0)
				free(exp.constant.excluded);
			exp.constant.excluded_count = 0;
			exp.constant.label = value;
		} else return exp.constant.label == value;
	} else if (exp.type == DATALOG_PREDICATE) {
		if (ConstantOnly) return false;
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (exp.pred.is_excluded(value)) return false;
			if (exp.pred.excluded_count > 0)
				free(exp.pred.excluded_count);
			exp.pred.excluded_count = 0;
			exp.pred.function = value;
		} else return exp.pred.function == value;
	} else if (exp.type == DATALOG_STRING && exp.str.length > 0) {
		if (ConstantOnly) return false;
		return value == DATALOG_STRING;
	} else {
		if (ConstantOnly) return false;
		return value == DATALOG_LABEL_EMPTY;
	}
	return true;
}

inline bool predicate_arg_not_empty(datalog_expression*& arg) {
	if (arg->type == DATALOG_CONSTANT) {
		datalog_expression* new_arg;
		if (arg->reference_count == 1) {
			new_arg = arg;
		} else {
			if (!init(new_arg, *arg)) exit(EXIT_FAILURE);
			free(*arg);
			arg = new_arg;
		}
		if (new_arg->constant.label == DATALOG_LABEL_WILDCARD
		 && !new_arg->constant.exclude(&DATALOG_LABEL_EMPTY, 1))
			exit(EXIT_FAILURE);
	} else if (arg->type == DATALOG_PREDICATE) {
		datalog_expression* new_arg;
		if (arg->reference_count == 1) {
			new_arg = arg;
		} else {
			if (!init(new_arg, *arg)) exit(EXIT_FAILURE);
			free(*arg);
			arg = new_arg;
		}
		if (new_arg->pred.function == DATALOG_LABEL_WILDCARD
		 && !new_arg->pred.exclude(&DATALOG_LABEL_EMPTY, 1))
			exit(EXIT_FAILURE);
	} else if (arg->type == DATALOG_ANY || arg->type == DATALOG_NON_EMPTY) {
		free(*arg);
		if (arg->reference_count == 0)
			free(arg);
		arg = &DATALOG_NON_EMPTY_TREE;
		DATALOG_NON_EMPTY_TREE.reference_count++;
	} else if (arg->type != DATALOG_VARIABLE && arg->type != DATALOG_STRING) {
		return false;
	}
	return true;
}

inline bool set_predicate_arity_any(datalog_predicate& pred, unsigned int value) {
	pred.function = DATALOG_LABEL_WILDCARD;
	pred.excluded_count = 0;
	pred.args[0] = &DATALOG_ANY_TREE;
	DATALOG_ANY_TREE.reference_count++;

	unsigned int boundary;
	if (value == ARITY_ZERO) boundary = 0;
	else if (value == ARITY_ONE) boundary = 1;
	else if (value == ARITY_TWO) boundary = 2;
	else if (value == ARITY_THREE) boundary = 3;
	else {
		fprintf(stderr, "set_predicate_arity ERROR: Unrecognized arity.\n");
		return false;
	}

	for (unsigned int i = 0; i < boundary; i++)
		pred.args[i] = &DATALOG_ANY_TREE;
	DATALOG_ANY_TREE.reference_count += boundary;
	for (unsigned int i = boundary; i < array_length(pred.args); i++)
		pred.args[i] = NULL;
	return true;
}

inline bool set_predicate_arity(datalog_predicate& pred, unsigned int value) {
	unsigned int boundary;
	if (value == ARITY_ZERO) boundary = 0;
	else if (value == ARITY_ONE) boundary = 1;
	else if (value == ARITY_TWO) boundary = 2;
	else if (value == ARITY_THREE) boundary = 3;
	else {
		fprintf(stderr, "set_predicate_arity ERROR: Unrecognized arity.\n");
		return false;
	}

	if (boundary > 0 && (pred.args[boundary - 1] == NULL || !predicate_arg_not_empty(pred.args[boundary - 1])))
		return false;
	for (unsigned int i = boundary; i < array_length(pred.args); i++) {
		if (pred.args[i] == NULL) continue;
		if (!can_be_empty(*pred.args[i])) return false;
		free(*pred.args[i]);
		if (pred.args[i]->reference_count == 0)
			free(pred.args[i]);
		pred.args[i] = NULL;
	}
	return true;
}

inline bool set_predicate_arity(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (value == ARITY_NULL) {
			if (!init(exp.tuple, POSITION_LEFT, 1)) exit(EXIT_FAILURE);
			exp.type = DATALOG_TUPLE;
		} else if (value == DATALOG_LABEL_EMPTY) {
			exp.type = DATALOG_EMPTY;
		} else {
			exp.type = DATALOG_PREDICATE;
			if (!set_predicate_arity_any(exp.pred, value))
				return false;
		}
	} else if (exp.type == DATALOG_TUPLE) {
		if (value == DATALOG_LABEL_EMPTY) {
			if (exp.tuple.elements.length > 0) return false;
			exp.tuple.position = POSITION_EXACT;
		} else if (value != ARITY_NULL) {
			if (exp.tuple.elements.length > 1) return false;
			else if (exp.tuple.elements.length == 0) {
				if (exp.tuple.position == POSITION_EXACT) return false;
				free(exp.tuple);
				exp.type = DATALOG_PREDICATE;
				if (!set_predicate_arity_any(exp.pred, value))
					return false;
			} else {
				datalog_expression element = *exp.tuple.elements[0];
				swap(exp, element);
				if (!set_predicate_arity(exp, value))
					return false;
			}
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (value == ARITY_NULL || value == DATALOG_LABEL_EMPTY
		 || !set_predicate_arity(exp.pred, value))
			return false;
	} else if (exp.type == DATALOG_EMPTY) {
		return (value == DATALOG_LABEL_EMPTY);
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index, bool ConstantOnly>
inline bool set_arg_constant(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (value == ARG_OTHER) return true;
		else if (!new_expression(exp.pred.args[Index])) exit(EXIT_FAILURE);
		exp.pred.args[Index]->type = DATALOG_ANY;
		exp.pred.args[Index]->reference_count = 1;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
			if (i != Index) exp.pred.args[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += (array_length(exp.pred.args) - 1);
		exp.pred.function = DATALOG_LABEL_WILDCARD;
		exp.pred.excluded_count = 0;
		exp.type = DATALOG_PREDICATE;
		if (value == DATALOG_LABEL_EMPTY) {
			exp.pred.args[Index] = NULL;
		} else {
			//return set_constant<ConstantOnly>(*exp.pred.args[Index], value);
			return set_predicate<0, true, false>(*exp.pred.args[Index], value);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.args[Index] == NULL)
			return value == DATALOG_LABEL_EMPTY;

		if (value == DATALOG_LABEL_EMPTY) {
			if (exp.pred.args[Index]->type != DATALOG_ANY
			 && exp.pred.args[Index]->type != DATALOG_EMPTY)
				return false;
			free(*exp.pred.args[Index]);
			if (exp.pred.args[Index]->reference_count == 0)
				free(exp.pred.args[Index]);
			exp.pred.args[Index] = NULL;
		} else {
			datalog_expression* arg;
			if (exp.pred.args[Index]->reference_count == 1) {
				arg = exp.pred.args[Index];
			} else {
				if (!init(arg, *exp.pred.args[Index])) exit(EXIT_FAILURE);
				free(*exp.pred.args[Index]);
				exp.pred.args[Index] = arg;
			}
			//return set_constant<ConstantOnly>(*arg, value);
			return set_predicate<0, true, false>(*arg, value);
		}
	} else if (!ConstantOnly && exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position == POSITION_RIGHT) {
			return false; /* prune this region of the search */
		} else if (exp.tuple.elements.length == 0) {
			if (exp.tuple.position == POSITION_EXACT)
				return value == ARG_OTHER;
			else return value == DATALOG_LABEL_WILDCARD;
		} else {
			datalog_expression* element;
			if (exp.tuple.elements[0]->reference_count == 1) {
				element = exp.tuple.elements[0];
			} else {
				if (!init(element, *exp.tuple.elements[0])) exit(EXIT_FAILURE);
				free(*exp.tuple.elements[0]);
				exp.tuple.elements[0] = element;
			}
			return set_arg_constant<Index, ConstantOnly>(*element, value);
		}
	} else {
		return value == ARG_OTHER;
	}
	return true;
}

inline bool set_string(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		exp.type = DATALOG_STRING;
		if (!init(exp.str, 1)) return false;
		exp.str.tokens[0] = value;
	} else if (exp.type == DATALOG_STRING) {
		return exp.str.length == 1 && exp.str.tokens[0] == value;
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
inline bool set_arg_string(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
			if (i != Index) exp.pred.args[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += (array_length(exp.pred.args) - 1);
		exp.pred.function = DATALOG_LABEL_WILDCARD;
		exp.pred.excluded_count = 0;
		exp.type = DATALOG_PREDICATE;
		if (value == DATALOG_LABEL_EMPTY) {
			exp.pred.args[Index] = NULL;
		} else {
			if (!new_expression(exp.pred.args[Index])) exit(EXIT_FAILURE);
			exp.pred.args[Index]->type = DATALOG_ANY;
			exp.pred.args[Index]->reference_count = 1;
			return set_string(*exp.pred.args[Index], value);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.args[Index] == NULL)
			return value == DATALOG_LABEL_EMPTY;

		if (value == DATALOG_LABEL_EMPTY) {
			if (exp.pred.args[Index]->type != DATALOG_ANY
			 && exp.pred.args[Index]->type != DATALOG_EMPTY)
				return false;
			free(*exp.pred.args[Index]);
			if (exp.pred.args[Index]->reference_count == 0)
				free(exp.pred.args[Index]);
			exp.pred.args[Index] = NULL;
		} else {
			datalog_expression* arg;
			if (exp.pred.args[Index]->reference_count == 1) {
				arg = exp.pred.args[Index];
			} else {
				if (!init(arg, *exp.pred.args[Index])) exit(EXIT_FAILURE);
				free(*exp.pred.args[Index]);
				exp.pred.args[Index] = arg;
			}
			return set_string(*arg, value);
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
inline bool set_arg_arity(datalog_expression& exp, unsigned int value) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		if (value == ARG_OTHER) return true;
		else if (!new_expression(exp.pred.args[Index])) exit(EXIT_FAILURE);
		exp.pred.args[Index]->type = DATALOG_ANY;
		exp.pred.args[Index]->reference_count = 1;
		for (unsigned int i = 0; i < array_length(exp.pred.args); i++)
			if (i != Index) exp.pred.args[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += (array_length(exp.pred.args) - 1);
		exp.pred.function = DATALOG_LABEL_WILDCARD;
		exp.pred.excluded_count = 0;
		exp.type = DATALOG_PREDICATE;
		if (value == DATALOG_LABEL_EMPTY) {
			exp.pred.args[Index] = NULL;
		} else {
			return set_predicate_arity(*exp.pred.args[Index], value);
		}
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.args[Index] == NULL)
			return value == DATALOG_LABEL_EMPTY;

		if (value == DATALOG_LABEL_EMPTY) {
			if (exp.pred.args[Index]->type != DATALOG_ANY
			 && exp.pred.args[Index]->type != DATALOG_EMPTY)
				return false;
			free(*exp.pred.args[Index]);
			if (exp.pred.args[Index]->reference_count == 0)
				free(exp.pred.args[Index]);
			exp.pred.args[Index] = NULL;
		} else {
			datalog_expression* arg;
			if (exp.pred.args[Index]->reference_count == 1) {
				arg = exp.pred.args[Index];
			} else {
				if (!init(arg, *exp.pred.args[Index])) exit(EXIT_FAILURE);
				free(*exp.pred.args[Index]);
				exp.pred.args[Index] = arg;
			}
			return set_predicate_arity(*arg, value);
		}
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position == POSITION_RIGHT) {
			return false; /* prune this region of the search */
		} else if (exp.tuple.elements.length == 0) {
			if (exp.tuple.position == POSITION_EXACT)
				return value == ARG_OTHER;
			else return value == DATALOG_LABEL_WILDCARD;
		} else {
			datalog_expression* element;
			if (exp.tuple.elements[0]->reference_count == 1) {
				element = exp.tuple.elements[0];
			} else {
				if (!init(element, *exp.tuple.elements[0])) exit(EXIT_FAILURE);
				free(*exp.tuple.elements[0]);
				exp.tuple.elements[0] = element;
			}
			return set_arg_arity<Index>(*element, value);
		}
	} else {
		return value == ARG_OTHER;
	}
	return true;
}

bool set_feature(
		datalog_expression_root::feature feature,
		datalog_expression_root& exp, unsigned int value)
{
	static constexpr unsigned int PREDICATES_NOT[] = {PREDICATE_NOT};
	static constexpr unsigned int PREDICATES_COUNT_NOT[] = {PREDICATE_COUNT, PREDICATE_NOT};

	datalog_expression* arg;
	switch (feature) {
	case datalog_expression_root::FEATURE_FUNCTION:
		return set_function<false>(exp.root, value);
	case datalog_expression_root::FEATURE_FUNCTION_ONLY:
		return set_function<true>(exp.root, value);
	case datalog_expression_root::FEATURE_FUNCTION_ANSWER:
		if (exp.root.type == DATALOG_ANY || exp.root.type == DATALOG_NON_EMPTY) {
			exp.root.func.function = PREDICATE_ANSWER;
			set_function_variables<PREDICATE_ANSWER>(exp.root.func);
			exp.root.type = DATALOG_FUNCTION;
			if (!new_expression(exp.root.func.arg)) exit(EXIT_FAILURE);
			exp.root.func.arg->type = DATALOG_ANY;
			exp.root.func.arg->reference_count = 1;
			return set_function<false>(*exp.root.func.arg, value);
		} else if (exp.root.type == DATALOG_FUNCTION) {
			if (exp.root.func.function == DATALOG_LABEL_WILDCARD) {
				if (exp.root.func.is_excluded(PREDICATE_ANSWER)) return false;
				exp.root.func.function = PREDICATE_ANSWER;
				if (exp.root.func.excluded_count > 0) {
					free(exp.root.func.excluded);
					exp.root.func.excluded_count = 0;
				}
				set_function_variables<PREDICATE_ANSWER>(exp.root.func);
			} else if (exp.root.func.function != PREDICATE_ANSWER)
				return false;
			if (exp.root.func.arg->reference_count == 1) {
				arg = exp.root.func.arg;
			} else {
				if (!init(arg, *exp.root.func.arg)) exit(EXIT_FAILURE);
				free(*exp.root.func.arg);
				exp.root.func.arg = arg;
			}
			return set_function<false>(*exp.root.func.arg, value);
		} else return false;
	case datalog_expression_root::FEATURE_HAS_FUNCTION:
		return set_has_function(exp.root, value);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_NOT:
		return set_has_function(exp.root, value, PREDICATES_NOT);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT:
		return set_has_function(exp.root, value, PREDICATES_COUNT_NOT);
	case datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER:
		if (exp.root.type == DATALOG_ANY || exp.root.type == DATALOG_NON_EMPTY) {
			exp.root.func.function = PREDICATE_ANSWER;
			exp.root.func.excluded_count = 0;
			set_function_variables<PREDICATE_ANSWER>(exp.root.func);
			exp.root.type = DATALOG_FUNCTION;
			if (!new_expression(exp.root.func.arg)) exit(EXIT_FAILURE);
			exp.root.func.arg->type = DATALOG_ANY;
			exp.root.func.arg->reference_count = 1;
			return set_has_function(*exp.root.func.arg, value);
		} else if (exp.root.type == DATALOG_FUNCTION) {
			if (exp.root.func.function == DATALOG_LABEL_WILDCARD) {
				if (exp.root.func.is_excluded(PREDICATE_ANSWER)) return false;
				exp.root.func.function = PREDICATE_ANSWER;
				if (exp.root.func.excluded_count > 0) {
					free(exp.root.func.excluded);
					exp.root.func.excluded_count = 0;
				}
				set_function_variables<PREDICATE_ANSWER>(exp.root.func);
			} else if (exp.root.func.function != PREDICATE_ANSWER)
				return false;
			if (exp.root.func.arg->reference_count == 1) {
				arg = exp.root.func.arg;
			} else {
				if (!init(arg, *exp.root.func.arg)) exit(EXIT_FAILURE);
				free(*exp.root.func.arg);
				exp.root.func.arg = arg;
			}
			return set_has_function(*exp.root.func.arg, value);
		} else return false;
	case datalog_expression_root::FEATURE_PREDICATE:
		return set_predicate<0, true, true>(exp.root, value);
	case datalog_expression_root::FEATURE_PREDICATE_ONLY:
		return set_predicate<0, true, false>(exp.root, value);
	case datalog_expression_root::FEATURE_FIRST_PREDICATE:
		return set_predicate<0, false, true>(exp.root, value);
	case datalog_expression_root::FEATURE_SECOND_PREDICATE:
		return set_predicate<1, false, true>(exp.root, value);
	case datalog_expression_root::FEATURE_THIRD_PREDICATE:
		return set_predicate<2, false, true>(exp.root, value);
	case datalog_expression_root::FEATURE_LAST_PREDICATE:
		return set_right_predicate<0>(exp.root, value);
	case datalog_expression_root::FEATURE_DIRECTION:
		return set_direction<true>(exp.root, value);
	case datalog_expression_root::FEATURE_DIRECTION_ROOT:
		return set_direction<false>(exp.root, value);
	case datalog_expression_root::FEATURE_CONSTANT:
		return set_constant<true>(exp.root, value);
	case datalog_expression_root::FEATURE_PREDICATE_ARITY:
		return set_predicate_arity(exp.root, value);
	case datalog_expression_root::FEATURE_ARG1:
		return set_arg_constant<0, false>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG2:
		return set_arg_constant<1, false>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG3:
		return set_arg_constant<2, false>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG1_ONLY:
		return set_arg_constant<0, true>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG2_ONLY:
		return set_arg_constant<1, true>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG3_ONLY:
		return set_arg_constant<2, true>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG1_STRING:
		return set_arg_string<0>(exp.root, value);
	case datalog_expression_root::FEATURE_ARG2_ARITY:
		return set_arg_arity<1>(exp.root, value);

	case datalog_expression_root::FEATURE_NUMBER:
		return intersect(exp.concord, exp.concord, static_cast<grammatical_number>(value + 1 - NUMBER_OFFSET));
	case datalog_expression_root::FEATURE_INFLECTION:
		return intersect(exp.inf, exp.inf, static_cast<inflection>(value + 1 - INFLECTION_OFFSET));

	case datalog_expression_root::FEATURE_NULL:
		break;
	}
	fprintf(stderr, "set_feature ERROR: Unrecognized semantic feature.\n");
	exit(EXIT_FAILURE);
}

bool exclude_function(datalog_expression& exp, const unsigned int* values, unsigned int count) {
	if (exp.type != DATALOG_FUNCTION)
		return false;
	if (exp.func.function == DATALOG_LABEL_WILDCARD) {
		if (!exp.func.exclude(values, count))
			exit(EXIT_FAILURE);
		return (exp.func.excluded_count < NUM_PREDICATES);
	} else {
		return (index_of(exp.func.function, values, count) == count);
	}
}

template<unsigned int Index>
bool exclude_predicates(datalog_expression& exp, const unsigned int* values, unsigned int count) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		return false;
	} else if (exp.type == DATALOG_PREDICATE) {
		if (Index != 0)
			return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (!exp.pred.exclude(values, count)) exit(EXIT_FAILURE);
		} else return index_of(exp.pred.function, values, count) == count;
	} else if (exp.type == DATALOG_FUNCTION) {
		datalog_expression* arg;
		if (exp.func.arg->reference_count == 1) {
			arg = exp.func.arg;
		} else {
			if (!init(arg, *exp.func.arg)) exit(EXIT_FAILURE);
			free(*exp.func.arg);
			exp.func.arg = arg;
		}
		return exclude_predicates<Index>(*arg, values, count);
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position != POSITION_EXACT)
			return false;
		else if (Index >= exp.tuple.elements.length)
			return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
		datalog_expression* element;
		if (exp.tuple.elements[Index]->reference_count == 1) {
			element = exp.tuple.elements[Index];
		} else {
			if (!init(element, *exp.tuple.elements[Index])) exit(EXIT_FAILURE);
			free(*exp.tuple.elements[Index]);
			exp.tuple.elements[Index] = element;
		}
		if (element->type == DATALOG_ANY || element->type == DATALOG_NON_EMPTY) {
			return false;
		} else if (element->type == DATALOG_PREDICATE) {
			if (element->pred.function == DATALOG_LABEL_WILDCARD) {
				if (!element->pred.exclude(values, count)) exit(EXIT_FAILURE);
			} else return index_of(exp.pred.function, values, count) == count;
		} else if (element->type == DATALOG_FUNCTION) {
			if (element->func.function == DATALOG_LABEL_WILDCARD) {
				if (!element->func.exclude(values, count)) exit(EXIT_FAILURE);
			} else return index_of(exp.func.function, values, count) == count;
		} else {
			return false;
		}
	} else if (exp.type == DATALOG_EMPTY) {
		return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
bool exclude_right_predicates(datalog_expression& exp, const unsigned int* values, unsigned int count) {
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		return false;
	} else if (exp.type == DATALOG_PREDICATE) {
		if (Index != 0)
			return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (!exp.pred.exclude(values, count)) exit(EXIT_FAILURE);
		} else return index_of(exp.pred.function, values, count) == count;
	} else if (exp.type == DATALOG_FUNCTION) {
		datalog_expression* arg;
		if (exp.func.arg->reference_count == 1) {
			arg = exp.func.arg;
		} else {
			if (!init(arg, *exp.func.arg)) exit(EXIT_FAILURE);
			free(*exp.func.arg);
			exp.func.arg = arg;
		}
		return exclude_right_predicates<Index>(*arg, values, count);
	} else if (exp.type == DATALOG_TUPLE) {
		if (exp.tuple.position != POSITION_EXACT)
			return false;
		else if (Index >= exp.tuple.elements.length)
			return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
		datalog_expression* element;
		if (exp.tuple.elements[exp.tuple.elements.length - Index - 1]->reference_count == 1) {
			element = exp.tuple.elements[exp.tuple.elements.length - Index - 1];
		} else {
			if (!init(element, *exp.tuple.elements[exp.tuple.elements.length - Index - 1])) exit(EXIT_FAILURE);
			free(*exp.tuple.elements[exp.tuple.elements.length - Index - 1]);
			exp.tuple.elements[exp.tuple.elements.length - Index - 1] = element;
		}
		if (element->type == DATALOG_ANY || element->type == DATALOG_NON_EMPTY) {
			return false;
		} else if (element->type == DATALOG_PREDICATE) {
			if (element->pred.function == DATALOG_LABEL_WILDCARD) {
				if (!element->pred.exclude(values, count)) exit(EXIT_FAILURE);
			} else return index_of(exp.pred.function, values, count) == count;
		} else if (element->type == DATALOG_FUNCTION) {
			if (element->func.function == DATALOG_LABEL_WILDCARD) {
				if (!element->func.exclude(values, count)) exit(EXIT_FAILURE);
			} else return index_of(exp.func.function, values, count) == count;
		} else {
			return false;
		}
	} else if (exp.type == DATALOG_EMPTY) {
		return last_index_of(DATALOG_LABEL_EMPTY, values, count) == static_cast<unsigned int>(-1);
	} else {
		return false;
	}
	return true;
}

inline bool exclude_constant(datalog_expression& exp,
		const unsigned int* values, unsigned int count)
{
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		exp.type = DATALOG_CONSTANT;
		exp.constant.label = DATALOG_LABEL_WILDCARD;
		if (count > 0 && !init_excluded(exp.constant.excluded, values, count))
			return false;
		exp.constant.excluded_count = count;
	} else if (exp.type == DATALOG_CONSTANT) {
		if (exp.constant.label == DATALOG_LABEL_WILDCARD) {
			if (!exp.constant.exclude(values, count)) exit(EXIT_FAILURE);
		} else if (index_of(exp.constant.label, values, count) < count) return false;
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.function == DATALOG_LABEL_WILDCARD) {
			if (!exp.pred.exclude(values, count)) exit(EXIT_FAILURE);
		} else if (index_of(exp.pred.function, values, count) < count) return false;
	} else if (exp.type == DATALOG_STRING && exp.str.length > 0) {
		return false; /* disallow ambiguity in strings, so prune this part of the search */
	} else {
		return false;
	}
	return true;
}

template<unsigned int Index>
inline bool exclude_arg_constant(datalog_expression& exp,
		const unsigned int* values, unsigned int count)
{
	if (exp.type == DATALOG_ANY || exp.type == DATALOG_NON_EMPTY) {
		return false; /* this feature is only used in one nonterminal, so we can prune some unnecessary searching */
	} else if (exp.type == DATALOG_PREDICATE) {
		if (exp.pred.args[Index] == NULL)
			return (index_of(DATALOG_LABEL_EMPTY, values, count) == count);

		datalog_expression* arg;
		if (exp.pred.args[Index]->reference_count == 1) {
			arg = exp.pred.args[Index];
		} else {
			if (!init(arg, *exp.pred.args[Index])) exit(EXIT_FAILURE);
			free(*exp.pred.args[Index]);
			exp.pred.args[Index] = arg;
		}
		return exclude_constant(*arg, values, count);
	} else if (exp.type == DATALOG_EMPTY) {
		return (index_of(ARG_OTHER, values, count) == count);
	}
	return true;
}

bool exclude_features(datalog_expression_root::feature feature,
		datalog_expression_root& exp, const unsigned int* values, unsigned int count)
{
	switch (feature) {
	case datalog_expression_root::FEATURE_FUNCTION:
	case datalog_expression_root::FEATURE_FUNCTION_ONLY:
		return exclude_function(exp.root, values, count);
	case datalog_expression_root::FEATURE_FUNCTION_ANSWER:
	case datalog_expression_root::FEATURE_HAS_FUNCTION_ANSWER:
		return false; /* this feature is only used in the root nonterminal, so we can prune some unnecessary searching */
	case datalog_expression_root::FEATURE_HAS_FUNCTION:
	case datalog_expression_root::FEATURE_HAS_FUNCTION_NOT:
	case datalog_expression_root::FEATURE_HAS_FUNCTION_COUNT_NOT:
		return false;
	case datalog_expression_root::FEATURE_PREDICATE:
	case datalog_expression_root::FEATURE_PREDICATE_ONLY:
		return false; /* this feature is only used in preterminals, so we can prune some unnecessary searching */
	case datalog_expression_root::FEATURE_FIRST_PREDICATE:
		return exclude_predicates<0>(exp.root, values, count);
	case datalog_expression_root::FEATURE_SECOND_PREDICATE:
		return exclude_predicates<1>(exp.root, values, count);
	case datalog_expression_root::FEATURE_THIRD_PREDICATE:
		return exclude_predicates<2>(exp.root, values, count);
	case datalog_expression_root::FEATURE_LAST_PREDICATE:
		return exclude_right_predicates<0>(exp.root, values, count);
	case datalog_expression_root::FEATURE_DIRECTION:
	case datalog_expression_root::FEATURE_DIRECTION_ROOT:
		return false; /* this feature is only used in preterminals, so we can prune some unnecessary searching */
	case datalog_expression_root::FEATURE_CONSTANT:
		return exclude_constant(exp.root, values, count);
	case datalog_expression_root::FEATURE_PREDICATE_ARITY:
		return false;
	case datalog_expression_root::FEATURE_ARG1:
	case datalog_expression_root::FEATURE_ARG1_ONLY:
		return exclude_arg_constant<0>(exp.root, values, count);
	case datalog_expression_root::FEATURE_ARG2:
	case datalog_expression_root::FEATURE_ARG2_ONLY:
		return exclude_arg_constant<1>(exp.root, values, count);
	case datalog_expression_root::FEATURE_ARG3:
	case datalog_expression_root::FEATURE_ARG3_ONLY:
		return exclude_arg_constant<2>(exp.root, values, count);
	case datalog_expression_root::FEATURE_ARG1_STRING:
		return false;
	case datalog_expression_root::FEATURE_ARG2_ARITY:
		return false;
	case datalog_expression_root::FEATURE_NUMBER:
	case datalog_expression_root::FEATURE_INFLECTION:
		return false;
	case datalog_expression_root::FEATURE_NULL:
		break;
	}
	fprintf(stderr, "exclude_features ERROR: Unrecognized semantic feature.\n");
	exit(EXIT_FAILURE);
}

bool intersect_labels(
		unsigned int& dst, unsigned int*& dst_excluded, unsigned int& dst_excluded_count,
		unsigned int first, unsigned int* first_excluded, unsigned int first_excluded_count,
		unsigned int second, unsigned int* second_excluded, unsigned int second_excluded_count)
{
	if (first == DATALOG_LABEL_WILDCARD) {
		if (second == DATALOG_LABEL_WILDCARD) {
			unsigned int total = first_excluded_count + second_excluded_count;
			dst = DATALOG_LABEL_WILDCARD;
			dst_excluded_count = 0;
			if (total > 0) {
				dst_excluded = (unsigned int*) malloc(sizeof(unsigned int) * total);
				if (dst_excluded == NULL) {
					fprintf(stderr, "intersect_label ERROR: Out of memory.\n");
					return false;
				}
				set_union(dst_excluded, dst_excluded_count,
						first_excluded, first_excluded_count,
						second_excluded, second_excluded_count);
			}
		} else {
			if (index_of(second, first_excluded, first_excluded_count) < first_excluded_count)
				return false;
			dst = second;
			dst_excluded_count = 0;
		}
	} else if (second == DATALOG_LABEL_WILDCARD) {
		if (index_of(first, second_excluded, second_excluded_count) < second_excluded_count)
			return false;
		dst = first;
		dst_excluded_count = 0;
	} else if (first == second) {
		dst = first;
		dst_excluded_count = 0;
	} else {
		return false;
	}
	return true;
}

unsigned int intersect_variable(unsigned int first, unsigned int second,
		array<unsigned int>& first_variables, array<unsigned int>& second_variables,
		array<unsigned int>& dst_variables)
{
	if (!ensure_variable_map_capacity(first_variables, first)
	 || !ensure_variable_map_capacity(second_variables, second))
		exit(EXIT_FAILURE);

	unsigned int i, j;
	for (i = first; i > 0; i--)
		if (first_variables[i] != 0) break;
	for (j = second; j > 0; j--)
		if (second_variables[j] != 0) break;
	unsigned int first_variable = first_variables[i] + first - i;
	unsigned int second_variable = second_variables[j] + second - j;

	unsigned int new_variable;
	if (first_variables[first] == 0) {
		/* first variable is unmapped */
		if (second_variables[second] == 0) {
			new_variable = max(first_variable, second_variable);
			first_variables[first] = new_variable;
			second_variables[second] = new_variable;
		} else {
			if (index_of(second_variable, first_variables.data, first_variables.capacity) < first_variables.capacity)
				return 0;
			first_variables[first] = second_variable;
			new_variable = second_variable;
		}
	} else if (second_variables[second] == 0) {
		if (index_of(first_variable, second_variables.data, second_variables.capacity) < second_variables.capacity)
			return 0;
		second_variables[second] = first_variable;
		new_variable = first_variable;
	} else if (first_variable == second_variable) {
		new_variable = first_variable;
	} else {
		return 0;
	}
	if (!ensure_variable_map_capacity(dst_variables, new_variable))
		exit(EXIT_FAILURE);
	if (dst_variables[new_variable] == 0)
		dst_variables[new_variable] = 1;
	return new_variable;
}

inline bool intersect(datalog_expression*& intersection,
		const datalog_expression* first, const datalog_expression* second,
		array<unsigned int>& first_variables, array<unsigned int>& second_variables,
		array<unsigned int>& dst_variables)
{
	if (!new_expression(intersection)) return false;
	if (!intersect(*intersection, first, second, first_variables, second_variables, dst_variables)) {
		free(intersection); return false;
	}
	return true;
}

bool intersect(datalog_expression& intersection,
		const datalog_expression* first, const datalog_expression* second,
		array<unsigned int>& first_variables, array<unsigned int>& second_variables,
		array<unsigned int>& dst_variables)
{
	if (first->type == DATALOG_EMPTY) {
		if (!can_be_empty(*second)) return false;
		return init(intersection, *first, first_variables, dst_variables);
	} else if (second->type == DATALOG_EMPTY) {
		if (!can_be_empty(*first)) return false;
		intersection.type = DATALOG_EMPTY;
		intersection.reference_count = 1;
	} else if (first->type == DATALOG_ANY) {
		return init(intersection, *second, second_variables, dst_variables);
	} else if (second->type == DATALOG_ANY) {
		return init(intersection, *first, first_variables, dst_variables);
	} else if (first->type == DATALOG_NON_EMPTY) {
		if (!init(intersection, *second, second_variables, dst_variables)) return false;
		if (intersection.type == DATALOG_ANY)
			intersection.type = DATALOG_NON_EMPTY;
	} else if (second->type == DATALOG_NON_EMPTY) {
		if (!init(intersection, *first, first_variables, dst_variables)) return false;
		if (intersection.type == DATALOG_ANY)
			intersection.type = DATALOG_NON_EMPTY;
	} else if (first->type == DATALOG_TUPLE) {
		if (second->type == DATALOG_TUPLE) {
			tuple_position position;
			unsigned int tuple_length;
			if (first->tuple.position == POSITION_LEFT) {
				if (second->tuple.position == POSITION_LEFT) {
					tuple_length = max(first->tuple.elements.length, second->tuple.elements.length);
					position = POSITION_LEFT;
				} else if (second->tuple.position == POSITION_EXACT) {
					if (first->tuple.elements.length > second->tuple.elements.length) return false;
					tuple_length = second->tuple.elements.length;
					position = POSITION_EXACT;
				} else return false;
			} else if (first->tuple.position == POSITION_RIGHT) {
				if (second->tuple.position == POSITION_RIGHT) {
					tuple_length = max(first->tuple.elements.length, second->tuple.elements.length);
					position = POSITION_RIGHT;
				} else if (second->tuple.position == POSITION_EXACT) {
					if (first->tuple.elements.length > second->tuple.elements.length) return false;
					tuple_length = second->tuple.position;
					position = POSITION_EXACT;
				} else return false;
			} else {
				if (second->tuple.position == POSITION_EXACT) {
					if (first->tuple.elements.length != second->tuple.elements.length)
						return false;
					tuple_length = first->tuple.elements.length;
					position = POSITION_EXACT;
				} else {
					if (first->tuple.elements.length < second->tuple.elements.length) return false;
					tuple_length = first->tuple.elements.length;
					position = POSITION_EXACT;
				}
			}

			unsigned int intersection_length = min(first->tuple.elements.length, second->tuple.elements.length);
			if (!init_tuple(intersection, position, tuple_length)) return false;
			if (first->tuple.position == POSITION_RIGHT || second->tuple.position == POSITION_RIGHT) {
				if (first->tuple.elements.length < second->tuple.elements.length) {
					for (unsigned int i = 0; i < tuple_length - first->tuple.elements.length; i++) {
						if (!init(intersection.tuple.elements[i], second->tuple.elements[i], second_variables, dst_variables)) {
							free(intersection); return false;
						}
						intersection.tuple.elements.length++;
					}
				} else {
					for (unsigned int i = 0; i < tuple_length - second->tuple.elements.length; i++) {
						if (!init(intersection.tuple.elements[i], first->tuple.elements[i], first_variables, dst_variables)) {
							free(intersection); return false;
						}
						intersection.tuple.elements.length++;
					}
				}
				for (unsigned int i = 0; i < intersection_length; i++) {
					if (!intersect(intersection.tuple.elements[tuple_length - intersection_length + i],
							first->tuple.elements[first->tuple.elements.length - intersection_length + i],
							second->tuple.elements[second->tuple.elements.length - intersection_length + i],
							first_variables, second_variables, dst_variables))
					{
						free(intersection); return false;
					}
					intersection.tuple.elements.length++;
				}
			} else {
				for (unsigned int i = 0; i < intersection_length; i++) {
					if (!intersect(intersection.tuple.elements[i],
							first->tuple.elements[i], second->tuple.elements[i],
							first_variables, second_variables, dst_variables))
					{
						free(intersection); return false;
					}
					intersection.tuple.elements.length++;
				}
				if (first->tuple.elements.length < second->tuple.elements.length) {
					for (unsigned int i = 0; i < tuple_length - first->tuple.elements.length; i++) {
						if (!init(intersection.tuple.elements[intersection_length + i],
								second->tuple.elements[intersection_length + i],
								second_variables, dst_variables)) {
							free(intersection); return false;
						}
						intersection.tuple.elements.length++;
					}
				} else {
					for (unsigned int i = 0; i < tuple_length - second->tuple.elements.length; i++) {
						if (!init(intersection.tuple.elements[intersection_length + i],
								first->tuple.elements[intersection_length + i],
								first_variables, dst_variables)) {
							free(intersection); return false;
						}
						intersection.tuple.elements.length++;
					}
				}
			}
		} else if (second->type == DATALOG_PREDICATE || second->type == DATALOG_FUNCTION) {
			if (first->tuple.elements.length == 0) {
				if (first->tuple.position == POSITION_EXACT) return false;
				return init(intersection, *second, second_variables, dst_variables);
			} else if (first->tuple.elements.length > 1) {
				return false;
			} else {
				return intersect(intersection, first->tuple.elements[0], second, first_variables, second_variables, dst_variables);
			}
		} else {
			return false;
		}
	} else if (first->type == DATALOG_FUNCTION) {
		if (second->type == DATALOG_FUNCTION) {
			if (!intersect_labels(
					intersection.func.function, intersection.func.excluded, intersection.func.excluded_count,
					first->func.function, first->func.excluded, first->func.excluded_count,
					second->func.function, second->func.excluded, second->func.excluded_count))
			{
				return false;
			}
			intersection.func.arg = NULL;
			intersection.type = DATALOG_FUNCTION;
			intersection.reference_count = 1;

			unsigned int i;
			for (i = 0; i < array_length(first->func.vars); i++) {
				if (first->func.vars[i] == 0) {
					if (second->func.vars[i] != 0) {
						if (!init(intersection.func.vars[i], second->func.vars[i], second_variables, dst_variables))
							return false;
					} else break;
				} else if (second->func.vars[i] == 0) {
					if (!init(intersection.func.vars[i], first->func.vars[i], first_variables, dst_variables))
						return false;
				} else {
					intersection.func.vars[i] = intersect_variable(
							first->func.vars[i], second->func.vars[i], first_variables, second_variables, dst_variables);
					if (intersection.func.vars[i] == 0) {
						free(intersection); return false;
					}
				}
			}
			for (; i < array_length(first->func.vars); i++)
				intersection.func.vars[i] = 0;

			if (!intersect(intersection.func.arg,
					first->func.arg, second->func.arg,
					first_variables, second_variables, dst_variables))
			{
				intersection.func.arg = NULL;
				free(intersection); return false;
			}
		} else {
			return false;
		}
	} else if (first->type == DATALOG_PREDICATE) {
		if (second->type == DATALOG_PREDICATE) {
			if (!intersect_labels(
					intersection.pred.function, intersection.pred.excluded, intersection.pred.excluded_count,
					first->pred.function, first->pred.excluded, first->pred.excluded_count,
					second->pred.function, second->pred.excluded, second->pred.excluded_count))
			{
				return false;
			}
			intersection.type = DATALOG_PREDICATE;
			intersection.reference_count = 1;

			for (unsigned int i = 0; i < array_length(intersection.pred.args); i++)
				intersection.pred.args[i] = NULL;
			for (unsigned int i = 0; i < array_length(intersection.pred.args); i++) {
				if (first->pred.args[i] == NULL) {
					if (second->pred.args[i] != NULL && second->pred.args[i]->type != DATALOG_ANY) {
						free(intersection); return false;
					} else continue;
				} else if (second->pred.args[i] == NULL) {
					if (first->pred.args[i]->type != DATALOG_ANY) {
						free(intersection); return false;
					} else continue;
				}
				if (!intersect(intersection.pred.args[i],
						first->pred.args[i], second->pred.args[i],
						first_variables, second_variables, dst_variables))
				{
					intersection.pred.args[i] = NULL;
					free(intersection); return false;
				}
			}
		} else if (second->type == DATALOG_TUPLE) {
			if (second->tuple.elements.length == 0) {
				if (second->tuple.position == POSITION_EXACT) return false;
				return init(intersection, *first, first_variables, dst_variables);
			} else if (second->tuple.elements.length > 1) {
				return false;
			} else {
				return intersect(intersection, first, second->tuple.elements[0], first_variables, second_variables, dst_variables);
			}
		} else {
			return false;
		}
	} else if (first->type == DATALOG_VARIABLE) {
		if (second->type == DATALOG_VARIABLE) {
			unsigned int new_variable = intersect_variable(
					first->variable, second->variable, first_variables, second_variables, dst_variables);
			if (new_variable == 0) return false;

			intersection.variable = new_variable;
			intersection.type = DATALOG_VARIABLE;
			intersection.reference_count = 1;
		} else {
			return false;
		}
	} else if (first->type == DATALOG_CONSTANT) {
		if (second->type == DATALOG_CONSTANT) {
			if (!intersect_labels(
					intersection.constant.label, intersection.constant.excluded, intersection.constant.excluded_count,
					first->constant.label, first->constant.excluded, first->constant.excluded_count,
					second->constant.label, second->constant.excluded, second->constant.excluded_count))
			{
				return false;
			}
			intersection.type = DATALOG_CONSTANT;
			intersection.reference_count = 1;
		} else {
			return false;
		}
	} else if (first->type == DATALOG_INTEGER) {
		if (second->type == DATALOG_INTEGER) {
			if (first->integer != second->integer) return false;
			intersection.integer = first->integer;
			intersection.type = DATALOG_INTEGER;
			intersection.reference_count = 1;
		} else {
			return false;
		}
	} else if (first->type == DATALOG_STRING) {
		if (second->type == DATALOG_STRING) {
			if (first->str.length == 1 && first->str[0] == DATALOG_LABEL_WILDCARD) {
				intersection.str = second->str;
			} else if (second->str.length == 1 && second->str[0] == DATALOG_LABEL_WILDCARD) {
				intersection.str = first->str;
			} else if (first->str == second->str) {
				intersection.str = first->str;
			} else {
				return false;
			}
			intersection.type = DATALOG_STRING;
			intersection.reference_count = 1;
		} else {
			return false;
		}
	} else {
		return false;
	}
	return true;
}

inline bool intersect(datalog_expression& intersection,
		const datalog_expression* first, const datalog_expression* second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return intersect(intersection, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

inline bool intersect(datalog_expression*& intersection,
		const datalog_expression* first, const datalog_expression* second)
{
	if (!new_expression(intersection)) return false;
	if (!intersect(*intersection, first, second)) {
		free(intersection); return false;
	}
	return true;
}

inline bool is_subset(const datalog_expression_root& first, const datalog_expression_root& second)
{
	grammatical_number num;
	if (!intersect(num, first.index, second.index)) return false;
	if (num != first.index) return false;
	if (!intersect(num, first.concord, second.concord)) return false;
	if (num != first.concord) return false;

	inflection inf;
	if (!intersect(inf, first.inf, second.inf)) return false;
	if (inf != first.inf) return false;

	datalog_expression* intersection;
	if (!intersect(intersection, &first.root, &second.root)) return false;
	bool result = (intersection == &first.root || *intersection == first.root);
	free(*intersection); if (intersection->reference_count == 0) free(intersection);
	return result;
}

template<bool KeepHead, bool MergeVariables>
inline bool init_variable_maps(const datalog_expression& second,
		array<bool>& first_variables, array<unsigned int>& first_variable_map,
		array<unsigned int>& second_variable_map, array<unsigned int>& dst_variable_map)
{
	if (!first_variables.ensure_capacity(first_variable_map.capacity)) exit(EXIT_FAILURE);
	memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
	for (unsigned int i = 1; i < first_variable_map.capacity; i++)
		if (first_variable_map[i] != 0) first_variables[first_variable_map[i]] = true;
	second_variable_map[1] = (KeepHead ? 1 : 2);
	dst_variable_map[KeepHead ? 1 : 2] = 1;
	return (!MergeVariables || build_variable_map<KeepHead ? 0 : 1>(second, second_variable_map, dst_variable_map));
}

template<bool MergeVariables>
inline bool check_variable_overlap(datalog_expression& inverse,
		array<bool>& first_variables, const array<unsigned int>& second_variable_map)
{
	unsigned int unmapped_second_variables = 0;
	for (unsigned int i = 1; i < second_variable_map.capacity; i++) {
		if (second_variable_map[i] != 0) {
			if (second_variable_map[i] < first_variables.capacity && first_variables[second_variable_map[i]]) {
				if (!MergeVariables && second_variable_map[i] != 1) {
					free(inverse); return false;
				}
				first_variables[second_variable_map[i]] = false;
			} else if (second_variable_map[i] != 1) {
				unmapped_second_variables++;
			}
		}
	}

	unsigned int unmapped_first_variables = 0;
	for (unsigned int i = 2; MergeVariables && i < first_variables.capacity; i++)
		if (first_variables[i]) unmapped_first_variables++;
	if (MergeVariables && unmapped_first_variables != 0 && unmapped_second_variables != 0) {
		free(inverse); return false;
	}
	return true;
}

/* this implements select_left and select_right when 'first' is ANY */
template<unsigned int FieldCount, bool KeepHead, tuple_position Position>
bool invert_select_any(datalog_expression& inverse, const datalog_expression& second) {
	if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
		if (FieldCount != 1 || !init_tuple<FieldCount, Position>(inverse))
			return false;
		inverse.tuple.elements[0] = (datalog_expression*) malloc(sizeof(datalog_expression));
		if (inverse.tuple.elements[0] == NULL
		 || (KeepHead && !init(*inverse.tuple.elements[0], second))
		 || (!KeepHead && !init(*inverse.tuple.elements[0], second, 1)))
		{
			if (inverse.tuple.elements[0] != NULL) free(inverse.tuple.elements[0]);
			fprintf(stderr, "invert_select_any ERROR: Unable to initialize tuple element.\n");
			free(inverse); return false;
		}
		inverse.tuple.elements.length = 1;
	} else if (second.type == DATALOG_TUPLE) {
		if (second.tuple.elements.length != FieldCount
		 || !init_tuple<FieldCount, Position>(inverse))
			return false;
		for (unsigned int i = 0; i < FieldCount; i++) {
			if ((KeepHead && !init(inverse.tuple.elements[i], second.tuple.elements[i]))
			 || (!KeepHead && !init(inverse.tuple.elements[i], second.tuple.elements[i], 1)))
			{
				fprintf(stderr, "invert_select_any ERROR: Unable to initialize tuple element.\n");
				free(inverse); return false;
			}
			inverse.tuple.elements.length++;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_select_left(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	array<bool> first_variables = array<bool>(8);
	memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		return invert_select_any<FieldCount, KeepHead, POSITION_LEFT>(inverse, second);
	} else if (first.type == DATALOG_PREDICATE) {
		if (FieldCount != 1) return false;

		/* construct a variable map from the remaining elements of 'first' since we're not intersecting them */
		if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
			free(inverse); return false;
		}

		if (second.type == DATALOG_PREDICATE) {
			if (!intersect(inverse, &first, &second, first_variable_map, second_variable_map, dst_variable_map))
				return false;
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.elements.length != 1) return false;
			if (!intersect(inverse, &first, second.tuple.elements[0],
					first_variable_map, second_variable_map, dst_variable_map))
				return false;
		} else {
			return false;
		}
		if (!KeepHead) dst_variable_map[1] = 1;
		compress_variable_map(dst_variable_map);
		apply_variable_map(inverse, dst_variable_map);
		apply_variable_map(first_variable_map, dst_variable_map);
		apply_variable_map(second_variable_map, dst_variable_map);
	} else if (first.type == DATALOG_TUPLE) {
		/* construct a variable map from the remaining elements of 'first' since we're not intersecting them */
		for (unsigned int i = FieldCount; i < first.tuple.elements.length; i++) {
			if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
				free(inverse); return false;
			}
		}
		if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
			free(inverse); return false;
		}

		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (FieldCount != 1 || first.tuple.position == POSITION_RIGHT
			 || (first.tuple.position == POSITION_EXACT && first.tuple.elements.length < FieldCount))
				return false;
			else if (!init_tuple(inverse, first.tuple.position, max(FieldCount, (unsigned int) first.tuple.elements.length)))
				return false;

			if (first.tuple.elements.length == 0) {
				if (!init(inverse.tuple.elements[0],
						second, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}
			} else {
				if (!intersect(inverse.tuple.elements[0],
						first.tuple.elements[0], &second,
						first_variable_map, second_variable_map, dst_variable_map))
				{
					free(inverse); return false;
				}
			}
			inverse.tuple.elements.length = 1;
			if (!KeepHead) dst_variable_map[1] = 1;
			compress_variable_map(dst_variable_map);
			apply_variable_map(*inverse.tuple.elements[0], dst_variable_map);
			apply_variable_map(first_variable_map, dst_variable_map);
			apply_variable_map(second_variable_map, dst_variable_map);
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.elements.length != FieldCount || first.tuple.position == POSITION_RIGHT
			 || (first.tuple.position == POSITION_EXACT && first.tuple.elements.length < FieldCount))
				return false;
			else if (!init_tuple(inverse, first.tuple.position, max(FieldCount, (unsigned int) first.tuple.elements.length)))
				return false;

			inverse.tuple.elements.length = 0;
			for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
				if (!intersect(inverse.tuple.elements[i],
						first.tuple.elements[i], second.tuple.elements[i],
						first_variable_map, second_variable_map, dst_variable_map))
				{
					free(inverse); return false;
				}
				inverse.tuple.elements.length++;
			}
			for (unsigned int i = first.tuple.elements.length; i < FieldCount; i++) {
				if (!init(inverse.tuple.elements[i], second.tuple.elements[i], second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}
				inverse.tuple.elements.length++;
			}
			if (!KeepHead) dst_variable_map[1] = 1;
			compress_variable_map(dst_variable_map);
			for (unsigned int i = 0; i < FieldCount; i++)
				apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
			apply_variable_map(first_variable_map, dst_variable_map);
			apply_variable_map(second_variable_map, dst_variable_map);
		} else {
			return false;
		}

		for (unsigned int i = FieldCount; i < first.tuple.elements.length; i++) {
			inverse.tuple.elements[i] = first.tuple.elements[i];
			first.tuple.elements[i]->reference_count++;
		}
		inverse.tuple.elements.length = max(FieldCount, (unsigned int) first.tuple.elements.length);
	} else {
		return false;
	}

	return check_variable_overlap<MergeVariables>(inverse, first_variables, second_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
inline bool invert_select_left(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_select_left<FieldCount, KeepHead, MergeVariables>(
			inverse, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_select_right(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	array<bool> first_variables = array<bool>(8);
	memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		return invert_select_any<FieldCount, KeepHead, POSITION_RIGHT>(inverse, second);
	} else if (first.type == DATALOG_PREDICATE) {
		if (FieldCount != 1) return false;

		/* construct a variable map from the remaining elements of 'first' since we're not intersecting them */
		if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
			free(inverse); return false;
		}

		if (second.type == DATALOG_PREDICATE) {
			if (!intersect(inverse, &first, &second, first_variable_map, second_variable_map, dst_variable_map))
				return false;
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.elements.length != 1)
				return false;

			if (!intersect(inverse, &first, second.tuple.elements[0],
					first_variable_map, second_variable_map, dst_variable_map))
				return false;
			inverse.tuple.elements.length++;
		} else {
			return false;
		}
		if (!KeepHead) dst_variable_map[1] = 1;
		compress_variable_map(dst_variable_map);
		apply_variable_map(inverse, dst_variable_map);
		apply_variable_map(first_variable_map, dst_variable_map);
		apply_variable_map(second_variable_map, dst_variable_map);
	} else if (first.type == DATALOG_TUPLE) {
		/* construct a variable map from the remaining elements of 'first' since we're not intersecting them */
		for (unsigned int i = 0; i < max(FieldCount, (unsigned int) first.tuple.elements.length) - FieldCount; i++) {
			if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
				free(inverse); return false;
			}
		}
		if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
			free(inverse); return false;
		}

		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (FieldCount != 1 || first.tuple.position == POSITION_LEFT
			 || (first.tuple.position == POSITION_EXACT && first.tuple.elements.length < FieldCount))
				return false;
			else if (!init_tuple(inverse, first.tuple.position, max(FieldCount, (unsigned int) first.tuple.elements.length)))
				return false;

			if (first.tuple.elements.length == 0) {
				if (!init(inverse.tuple.elements[0], second, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}
				inverse.tuple.elements.length = FieldCount;
			} else {
				for (unsigned int i = 0; i < first.tuple.elements.length - FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				inverse.tuple.elements.length = first.tuple.elements.length - FieldCount;
				if (!intersect(inverse.tuple.elements[inverse.tuple.elements.length],
						first.tuple.elements.last(), &second,
						first_variable_map, second_variable_map, dst_variable_map))
				{
					free(inverse); return false;
				}
				inverse.tuple.elements.length = first.tuple.elements.length;
			}
			if (!KeepHead) dst_variable_map[1] = 1;
			compress_variable_map(dst_variable_map);
			apply_variable_map(*inverse.tuple.elements.last(), dst_variable_map);
			apply_variable_map(first_variable_map, dst_variable_map);
			apply_variable_map(second_variable_map, dst_variable_map);
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.elements.length != FieldCount || first.tuple.position == POSITION_LEFT
			 || (first.tuple.position == POSITION_EXACT && first.tuple.elements.length < FieldCount))
				return false;
			else if (!init_tuple(inverse, first.tuple.position, max(FieldCount, (unsigned int) first.tuple.elements.length)))
				return false;

			if (first.tuple.elements.length < FieldCount) {
				for (unsigned int i = 0; i < FieldCount - first.tuple.elements.length; i++) {
					inverse.tuple.elements[i] = second.tuple.elements[i];
					second.tuple.elements[i]->reference_count++;
				}
				inverse.tuple.elements.length = FieldCount - first.tuple.elements.length;
				for (unsigned int i = FieldCount - first.tuple.elements.length; i < FieldCount; i++) {
					if (!intersect(inverse.tuple.elements[i],
							first.tuple.elements[i - FieldCount + first.tuple.elements.length],
							second.tuple.elements[i], first_variable_map, second_variable_map, dst_variable_map))
					{
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
			} else {
				for (unsigned int i = 0; i < first.tuple.elements.length - FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				inverse.tuple.elements.length = first.tuple.elements.length - FieldCount;
				for (unsigned int i = first.tuple.elements.length - FieldCount; i < first.tuple.elements.length; i++) {
					if (!intersect(inverse.tuple.elements[i], first.tuple.elements[i],
							second.tuple.elements[i - first.tuple.elements.length + FieldCount],
							first_variable_map, second_variable_map, dst_variable_map))
					{
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
			}
			if (!KeepHead) dst_variable_map[1] = 1;
			compress_variable_map(dst_variable_map);
			for (unsigned int i = first.tuple.elements.length - FieldCount; i < first.tuple.elements.length; i++)
				apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
			apply_variable_map(first_variable_map, dst_variable_map);
			apply_variable_map(second_variable_map, dst_variable_map);
		} else {
			return false;
		}
	} else {
		return false;
	}

	return check_variable_overlap<MergeVariables>(inverse, first_variables, second_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_select_right(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_select_right<FieldCount, KeepHead, MergeVariables>(inverse, first, second,
			first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_delete_left(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	array<bool> first_variables = array<bool>(8);
	memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (!init_tuple<FieldCount + 1, POSITION_EXACT>(inverse))
				return false;

			for (unsigned int i = 0; i < FieldCount; i++) {
				inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			inverse.tuple.elements.length = FieldCount;

			inverse.tuple.elements[FieldCount] = (datalog_expression*) malloc(sizeof(datalog_expression));
			if (inverse.tuple.elements[FieldCount] == NULL
			 || (KeepHead && !init(*inverse.tuple.elements[FieldCount], second))
			 || (!KeepHead && !init(*inverse.tuple.elements[FieldCount], second, 1)))
			{
				fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
				free(inverse); return false;
			}
			inverse.tuple.elements.length = FieldCount + 1;
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.position == POSITION_RIGHT) return false;
			else if (!init_tuple(inverse, second.tuple.position, FieldCount + second.tuple.elements.length))
				return false;
			for (unsigned int i = 0; i < FieldCount; i++) {
				inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			inverse.tuple.elements.length = FieldCount;
			for (unsigned int i = 0; i < second.tuple.elements.length; i++) {
				if ((KeepHead && !init(inverse.tuple.elements[FieldCount + i], second.tuple.elements[i]))
				 || (!KeepHead && !init(inverse.tuple.elements[FieldCount + i], second.tuple.elements[i], 1)))
				{
					fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
					free(inverse); return false;
				}
				inverse.tuple.elements.length++;
			}
		} else {
			return false;
		}
	} else if (first.type == DATALOG_PREDICATE) {
		if (FieldCount != 1) return false;
		if (second.type == DATALOG_EMPTY || (second.type == DATALOG_TUPLE && second.tuple.elements.length == 0))
			return init(inverse, first);
		else return false;
	} else if (first.type == DATALOG_TUPLE) {
		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (first.tuple.position == POSITION_LEFT)
			{
				if (first.tuple.elements.length > 1 + FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, max((unsigned int) first.tuple.elements.length, FieldCount + 1)))
					return false;

				for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				for (unsigned int i = min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++)
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count += FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length);
				inverse.tuple.elements.length = FieldCount;
				if (FieldCount < first.tuple.elements.length) {
					if (!intersect(inverse.tuple.elements[FieldCount],
							first.tuple.elements[FieldCount], &second,
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					if (!KeepHead) dst_variable_map[1] = 1;
					compress_variable_map(dst_variable_map);
					apply_variable_map(*inverse.tuple.elements[FieldCount], dst_variable_map);
					apply_variable_map(first_variable_map, dst_variable_map);
					apply_variable_map(second_variable_map, dst_variable_map);
				} else {
					if (!init(inverse.tuple.elements[FieldCount], second, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				inverse.tuple.elements.length = FieldCount + 1;
			} else {
				if (first.tuple.elements.length > 1 + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_RIGHT && first.tuple.elements.length < 1 + FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, max((unsigned int) first.tuple.elements.length, FieldCount + 1)))
					return false;

				for (unsigned int i = 0; i < max(1u, (unsigned int) first.tuple.elements.length) - 1; i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				unsigned int offset = min((unsigned int) (1 + FieldCount - first.tuple.elements.length), FieldCount);
				for (unsigned int i = 0; i < offset; i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
				}
				for (unsigned int i = offset; i < FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i - offset];
					first.tuple.elements[i - offset]->reference_count++;
				}
				inverse.tuple.elements.length = FieldCount;
				if (first.tuple.elements.length == 0) {
					if (!init(inverse.tuple.elements[FieldCount], second, second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
				} else {
					if (!intersect(inverse.tuple.elements[FieldCount],
							first.tuple.elements[first.tuple.elements.length - 1], &second,
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					if (!KeepHead) dst_variable_map[1] = 1;
					compress_variable_map(dst_variable_map);
					apply_variable_map(*inverse.tuple.elements[FieldCount], dst_variable_map);
					apply_variable_map(first_variable_map, dst_variable_map);
					apply_variable_map(second_variable_map, dst_variable_map);
				}
				inverse.tuple.elements.length = FieldCount + 1;
			}
		}
		else if (second.type == DATALOG_EMPTY)
		{
			if (first.tuple.position == POSITION_LEFT)
			{
				if (first.tuple.elements.length > FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_LEFT && first.tuple.elements.length < FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount))
					return false;

				for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				for (unsigned int i = min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++)
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count += FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length);
				inverse.tuple.elements.length = FieldCount;
				for (unsigned int i = FieldCount; i < first.tuple.elements.length; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
					inverse.tuple.elements.length++;
				}
			} else {
				if (first.tuple.elements.length > FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_RIGHT && first.tuple.elements.length < FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount))
					return false;

				unsigned int offset = FieldCount - first.tuple.elements.length;
				for (unsigned int i = 0; i < offset; i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
				}
				for (unsigned int i = offset; i < FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i - offset];
					first.tuple.elements[i - offset]->reference_count++;
				}
				inverse.tuple.elements.length = FieldCount;
			}
		}
		else if (second.type == DATALOG_TUPLE)
		{
			if (second.tuple.position == POSITION_LEFT || first.tuple.position == POSITION_LEFT)
			{
				if ((first.tuple.position == POSITION_RIGHT && second.tuple.position == POSITION_LEFT)
				 || (first.tuple.position == POSITION_LEFT && second.tuple.position == POSITION_RIGHT)) {
					return false;
				} else if (second.tuple.position != POSITION_LEFT && first.tuple.elements.length > second.tuple.elements.length + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_LEFT && first.tuple.elements.length < second.tuple.elements.length + FieldCount) {
					return false;
				} else if (!init_tuple(inverse,
						(first.tuple.position == POSITION_LEFT && second.tuple.position == POSITION_LEFT) ? POSITION_LEFT : POSITION_EXACT,
						max(first.tuple.elements.length, FieldCount + second.tuple.elements.length)))
					return false;

				for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				for (unsigned int i = 0; i < min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				for (unsigned int i = min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++)
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count += FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length);
				inverse.tuple.elements.length = FieldCount;
				for (unsigned int i = FieldCount; i < min(first.tuple.elements.length, FieldCount + second.tuple.elements.length); i++) {
					if (!intersect(inverse.tuple.elements[i],
							first.tuple.elements[i], second.tuple.elements[i - FieldCount],
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = inverse.tuple.elements.length; i < FieldCount + second.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], second.tuple.elements[i - FieldCount], second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = FieldCount + second.tuple.elements.length; i < first.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount + second.tuple.elements.length; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			} else {
				if (second.tuple.position != POSITION_RIGHT && first.tuple.elements.length > second.tuple.elements.length + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_RIGHT && first.tuple.elements.length < second.tuple.elements.length + FieldCount) {
					return false;
				} else if (!init_tuple(inverse,
						(first.tuple.position == POSITION_RIGHT && second.tuple.position == POSITION_RIGHT) ? POSITION_RIGHT : POSITION_EXACT,
								max(first.tuple.elements.length, FieldCount + second.tuple.elements.length)))
					return false;

				unsigned int offset = min((unsigned int) (max(second.tuple.elements.length + FieldCount, first.tuple.elements.length) - first.tuple.elements.length), FieldCount);
				for (unsigned int i = 0; i < max(offset, FieldCount) - offset; i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				for (unsigned int i = 0; i < offset; i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
				}
				for (unsigned int i = offset; i < FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i - offset];
					first.tuple.elements[i - offset]->reference_count++;
				}
				inverse.tuple.elements.length = FieldCount;
				for (unsigned int i = FieldCount; i < max(second.tuple.elements.length + FieldCount, first.tuple.elements.length) - first.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], second.tuple.elements[i - FieldCount], second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_left ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				offset = max((unsigned int) (max(second.tuple.elements.length + FieldCount, first.tuple.elements.length) - first.tuple.elements.length), FieldCount);
				unsigned int end = min(first.tuple.elements.length, second.tuple.elements.length);
				for (unsigned int i = 0; i < min(first.tuple.elements.length, second.tuple.elements.length); i++) {
					if (!intersect(inverse.tuple.elements[offset + i],
							first.tuple.elements[first.tuple.elements.length - end + i],
							second.tuple.elements[second.tuple.elements.length - end + i],
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = FieldCount; i < inverse.tuple.elements.length; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			}
		} else {
			return false;
		}
	} else {
		return false;
	}

	return check_variable_overlap<MergeVariables>(inverse, first_variables, second_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
inline bool invert_delete_left(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_delete_left<FieldCount, KeepHead, MergeVariables>(
			inverse, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_delete_right(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	array<bool> first_variables = array<bool>(8);
	memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (!init_tuple<FieldCount + 1, POSITION_EXACT>(inverse))
				return false;

			inverse.tuple.elements[0] = (datalog_expression*) malloc(sizeof(datalog_expression));
			if (inverse.tuple.elements[0] == NULL
			 || (KeepHead && !init(*inverse.tuple.elements[0], second))
			 || (!KeepHead && !init(*inverse.tuple.elements[0], second, 1)))
			{
				fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
				free(inverse); return false;
			}
			inverse.tuple.elements.length = FieldCount;

			for (unsigned int i = 1; i < FieldCount + 1; i++) {
				inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			inverse.tuple.elements.length = FieldCount + 1;
		} else if (second.type == DATALOG_TUPLE) {
			if (second.tuple.position == POSITION_LEFT) return false;
			else if (!init_tuple(inverse, second.tuple.position, FieldCount + second.tuple.elements.length))
				return false;
			for (unsigned int i = 0; i < second.tuple.elements.length; i++) {
				if ((KeepHead && !init(inverse.tuple.elements[i], second.tuple.elements[i]))
				 || (!KeepHead && !init(inverse.tuple.elements[i], second.tuple.elements[i], 1)))
				{
					fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
					free(inverse); return false;
				}
				inverse.tuple.elements.length++;
			}
			for (unsigned int i = second.tuple.elements.length; i < second.tuple.elements.length + FieldCount; i++) {
				inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			inverse.tuple.elements.length = second.tuple.elements.length + FieldCount;
		} else if (second.type == DATALOG_EMPTY) {
			if (!init_tuple(inverse, POSITION_EXACT, FieldCount))
				return false;
			for (unsigned int i = 0; i < FieldCount; i++) {
				inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count++;
			}
			inverse.tuple.elements.length = FieldCount;
		} else {
			return false;
		}
	} else if (first.type == DATALOG_PREDICATE) {
		if (FieldCount != 1) return false;
		if (second.type == DATALOG_EMPTY || (second.type == DATALOG_TUPLE && second.tuple.elements.length == 0))
			return init(inverse, first);
		else return false;
	} else if (first.type == DATALOG_TUPLE) {
		if (second.type == DATALOG_PREDICATE || second.type == DATALOG_FUNCTION) {
			if (first.tuple.position == POSITION_RIGHT)
			{
				if (first.tuple.elements.length > 1 + FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount + 1))
					return false;

				for (unsigned int i = FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++) {
					if (!build_variable_map(
							*first.tuple.elements[i - (FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length))],
							first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				if (first.tuple.elements.length == FieldCount + 1) {
					if (!intersect(inverse.tuple.elements[0], first.tuple.elements[0], &second,
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				} else {
					if (!init(inverse.tuple.elements[0], second, second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
				}
				inverse.tuple.elements.length = 1;
				for (unsigned int i = 0; i < FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					inverse.tuple.elements[1 + i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++) {
					if (!init(inverse.tuple.elements[1 + i],
							first.tuple.elements[i - (FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length))],
							first_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = 0; i < FieldCount + 1; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			} else {
				if (first.tuple.elements.length > 1 + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_LEFT && first.tuple.elements.length < 1 + FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount + 1))
					return false;

				for (unsigned int i = 1; i < first.tuple.elements.length; i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				inverse.tuple.elements.length = 0;
				if (first.tuple.elements.length > 0) {
					if (!intersect(inverse.tuple.elements[0], first.tuple.elements[0], &second,
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				} else {
					if (!init(inverse.tuple.elements[0], second.tuple.elements[0], second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
				}
				inverse.tuple.elements.length = 1;
				for (unsigned int i = 1; i < first.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = max((unsigned int) first.tuple.elements.length, 1u); i < max((unsigned int) first.tuple.elements.length, 1 + FieldCount); i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = 0; i < first.tuple.elements.length; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			}
		}
		else if (second.type == DATALOG_EMPTY)
		{
			if (first.tuple.position == POSITION_RIGHT)
			{
				if (first.tuple.elements.length > FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_RIGHT && first.tuple.elements.length < FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount))
					return false;

				for (unsigned int i = 0; i < FieldCount - first.tuple.elements.length; i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = FieldCount - first.tuple.elements.length; i < FieldCount; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i - (FieldCount - first.tuple.elements.length)];
					inverse.tuple.elements[i]->reference_count++;
					inverse.tuple.elements.length++;
				}
			} else {
				if (first.tuple.elements.length > FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_LEFT && first.tuple.elements.length < FieldCount) {
					return false;
				} else if (!init_tuple(inverse, POSITION_EXACT, FieldCount))
					return false;

				for (unsigned int i = 0; i < first.tuple.elements.length; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = first.tuple.elements.length; i < FieldCount; i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
			}
		}
		else if (second.type == DATALOG_TUPLE)
		{
			if (second.tuple.position == POSITION_RIGHT || first.tuple.position == POSITION_RIGHT)
			{
				if ((first.tuple.position == POSITION_RIGHT && second.tuple.position == POSITION_LEFT)
				 || (first.tuple.position == POSITION_LEFT && second.tuple.position == POSITION_RIGHT)) {
					return false;
				} else if (second.tuple.position != POSITION_RIGHT && first.tuple.elements.length > second.tuple.elements.length + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_RIGHT && first.tuple.elements.length < second.tuple.elements.length + FieldCount) {
					return false;
				} else if (!init_tuple(inverse,
						(first.tuple.position == POSITION_RIGHT && second.tuple.position == POSITION_RIGHT) ? POSITION_RIGHT : POSITION_EXACT,
						max(first.tuple.elements.length, FieldCount + second.tuple.elements.length)))
					return false;

				unsigned int offset = first.tuple.elements.length - min(first.tuple.elements.length, second.tuple.elements.length + FieldCount);
				for (unsigned int i = 0; i < offset; i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				for (unsigned int i = FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++) {
					if (!build_variable_map(*first.tuple.elements[i - (FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length))], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				for (unsigned int i = 0; i < offset; i++) {
					inverse.tuple.elements[i] = first.tuple.elements[i];
					first.tuple.elements[i]->reference_count++;
				}
				inverse.tuple.elements.length = offset;
				unsigned int next;
				if (first.tuple.elements.length < FieldCount)
					next = second.tuple.elements.length;
				else if (first.tuple.elements.length < FieldCount + second.tuple.elements.length)
					next = FieldCount + second.tuple.elements.length - first.tuple.elements.length;
				else next = offset;
				for (unsigned int i = offset; i < next; i++)
				{
					if (!init(inverse.tuple.elements[i], second.tuple.elements[i - offset], second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				unsigned int end, first_offset, second_offset;
				if (first.tuple.elements.length < FieldCount) {
					end = next;
				} else if (first.tuple.elements.length < FieldCount + second.tuple.elements.length) {
					end = second.tuple.elements.length;
					first_offset = next; second_offset = 0;
				} else {
					end = first.tuple.elements.length - FieldCount;
					first_offset = 0; second_offset = next;
				}
				for (unsigned int i = next; i < end; i++) {
					if (!intersect(inverse.tuple.elements[i],
							first.tuple.elements[i - first_offset], second.tuple.elements[i - second_offset],
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = 0; i < FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i++) {
					inverse.tuple.elements[end + i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length); i < FieldCount; i++) {
					if (!init(inverse.tuple.elements[end + i],
							first.tuple.elements[i - (FieldCount - min(FieldCount, (unsigned int) first.tuple.elements.length))],
							first_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = offset; i < end; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			} else {
				if (second.tuple.position != POSITION_LEFT && first.tuple.elements.length > second.tuple.elements.length + FieldCount) {
					return false;
				} else if (first.tuple.position != POSITION_LEFT && first.tuple.elements.length < second.tuple.elements.length + FieldCount) {
					return false;
				} else if (!init_tuple(inverse,
						(first.tuple.position == POSITION_LEFT && second.tuple.position == POSITION_LEFT) ? POSITION_RIGHT : POSITION_EXACT,
								max(first.tuple.elements.length, FieldCount + second.tuple.elements.length)))
					return false;

				for (unsigned int i = second.tuple.elements.length; i < first.tuple.elements.length; i++) {
					if (!build_variable_map(*first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
				}
				if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
					free(inverse); return false;
				}

				inverse.tuple.elements.length = 0;
				for (unsigned int i = 0; i < min(first.tuple.elements.length, second.tuple.elements.length); i++) {
					if (!intersect(inverse.tuple.elements[i],
							first.tuple.elements[i], second.tuple.elements[i],
							first_variable_map, second_variable_map, dst_variable_map)) {
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = min(first.tuple.elements.length, second.tuple.elements.length); i < second.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], second.tuple.elements[i], second_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = second.tuple.elements.length; i < first.tuple.elements.length; i++) {
					if (!init(inverse.tuple.elements[i], first.tuple.elements[i], first_variable_map, dst_variable_map)) {
						fprintf(stderr, "invert_delete_right ERROR: Unable to initialize tuple element.\n");
						free(inverse); return false;
					}
					inverse.tuple.elements.length++;
				}
				for (unsigned int i = max(first.tuple.elements.length, second.tuple.elements.length); i < max(first.tuple.elements.length, second.tuple.elements.length + FieldCount); i++) {
					inverse.tuple.elements[i] = &DATALOG_ANY_TREE;
					DATALOG_ANY_TREE.reference_count++;
					inverse.tuple.elements.length++;
				}
				if (!KeepHead) dst_variable_map[1] = 1;
				compress_variable_map(dst_variable_map);
				for (unsigned int i = 0; i < second.tuple.elements.length; i++)
					apply_variable_map(*inverse.tuple.elements[i], dst_variable_map);
				apply_variable_map(first_variable_map, dst_variable_map);
				apply_variable_map(second_variable_map, dst_variable_map);
			}
		} else {
			return false;
		}
	} else {
		return false;
	}

	return check_variable_overlap<MergeVariables>(inverse, first_variables, second_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
inline bool invert_delete_right(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_delete_right<FieldCount, KeepHead, MergeVariables>(inverse, first, second,
			first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_select_left_delete_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!init(inverse.func)) {
			fprintf(stderr, "invert_select_left_delete_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, first, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if ((first.func.function != DATALOG_LABEL_WILDCARD && first.func.function != PREDICATE_ANSWER)
		 || (first.func.function == DATALOG_LABEL_WILDCARD && index_of(PREDICATE_ANSWER, first.func.excluded, first.func.excluded_count) < first.func.excluded_count))
			return false;

		if (!init(inverse.func)) {
			fprintf(stderr, "invert_select_left_delete_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_select_right_delete_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!init(inverse.func)) {
			fprintf(stderr, "invert_select_left_delete_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_right<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, first, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if ((first.func.function != DATALOG_LABEL_WILDCARD && first.func.function != PREDICATE_ANSWER)
		 || (first.func.function == DATALOG_LABEL_WILDCARD && index_of(PREDICATE_ANSWER, first.func.excluded, first.func.excluded_count) < first.func.excluded_count))
			return false;

		if (!init(inverse.func)) {
			fprintf(stderr, "invert_select_left_delete_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_right<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_delete_left_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, DATALOG_ANY_TREE, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if ((first.func.function != DATALOG_LABEL_WILDCARD && first.func.function != PREDICATE_ANSWER)
		 || (first.func.function == DATALOG_LABEL_WILDCARD && index_of(PREDICATE_ANSWER, first.func.excluded, first.func.excluded_count) < first.func.excluded_count))
			return false;

		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, bool KeepHead, bool MergeVariables = true>
bool invert_delete_right_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_right<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, DATALOG_ANY_TREE, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if ((first.func.function != DATALOG_LABEL_WILDCARD && first.func.function != PREDICATE_ANSWER)
		 || (first.func.function == DATALOG_LABEL_WILDCARD && index_of(PREDICATE_ANSWER, first.func.excluded, first.func.excluded_count) < first.func.excluded_count))
			return false;

		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_right<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, second))
		{
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

bool invert_delete_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!init(inverse.func.arg, second)) {
			free(inverse);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if ((first.func.function != DATALOG_LABEL_WILDCARD && first.func.function != PREDICATE_ANSWER)
		 || (first.func.function == DATALOG_LABEL_WILDCARD && index_of(PREDICATE_ANSWER, first.func.excluded, first.func.excluded_count) < first.func.excluded_count))
			return false;

		if (!init(inverse.func)) {
			fprintf(stderr, "invert_delete_left_answer ERROR: Out of memory.\n");
			return false;
		}
		inverse.reference_count = 1;
		inverse.type = DATALOG_FUNCTION;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.vars[0] = 1;
		for (unsigned int i = 1; i < array_length(inverse.func.vars); i++)
			inverse.func.vars[i] = 0;
		if (!intersect(inverse.func.arg, first.func.arg, &second)) {
			inverse.func.arg = NULL;
			free(inverse); return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldIndex, bool OtherArgsEmpty>
bool invert_select_arg(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.type = DATALOG_PREDICATE;
		inverse.pred.function = DATALOG_LABEL_WILDCARD;
		if (!init_excluded(inverse.pred.excluded, &DATALOG_LABEL_EMPTY, 1))
			return false;
		inverse.pred.excluded_count = 1;
		inverse.reference_count = 1;

		if (OtherArgsEmpty) {
			for (unsigned int i = 0; i < FieldIndex; i++)
				inverse.pred.args[i] = NULL;
		} else {
			for (unsigned int i = 0; i < FieldIndex; i++)
				inverse.pred.args[i] = &DATALOG_ANY_TREE;
			DATALOG_ANY_TREE.reference_count += FieldIndex;
		}
		if (second.type == DATALOG_EMPTY) {
			for (unsigned int i = FieldIndex; i < array_length(inverse.pred.args); i++)
				inverse.pred.args[i] = NULL;
		} else {
			if (!init(inverse.pred.args[FieldIndex], second)) {
				inverse.pred.args[FieldIndex] = NULL;
				free(inverse); return false;
			}
			if (OtherArgsEmpty) {
				for (unsigned int i = FieldIndex + 1; i < array_length(inverse.pred.args); i++)
					inverse.pred.args[i] = NULL;
			} else {
				for (unsigned int i = FieldIndex + 1; i < array_length(inverse.pred.args); i++)
					inverse.pred.args[i] = &DATALOG_ANY_TREE;
				DATALOG_ANY_TREE.reference_count += array_length(inverse.pred.args) - FieldIndex - 1;
			}
		}
	} else if (first.type == DATALOG_PREDICATE) {
		inverse.type = DATALOG_PREDICATE;
		inverse.pred.function = first.pred.function;
		inverse.reference_count = 1;
		if (first.pred.excluded_count > 0 && !init_excluded(inverse.pred.excluded, first.pred.excluded, first.pred.excluded_count)) {
			free(inverse); return false;
		}
		inverse.pred.excluded_count = first.pred.excluded_count;
		if (inverse.pred.function == DATALOG_LABEL_WILDCARD
		 && !inverse.pred.exclude(&DATALOG_LABEL_EMPTY, 1)) {
			free(inverse); return false;
		}

		for (unsigned int i = 0; i < array_length(inverse.pred.args); i++)
			inverse.pred.args[i] = NULL;
		for (unsigned int i = 0; i < FieldIndex; i++) {
			if (OtherArgsEmpty) {
				if (first.pred.args[i] != NULL && !can_be_empty(*first.pred.args[i])) {
					free(inverse); return false;
				}
				inverse.pred.args[i] = NULL;
			} else {
				inverse.pred.args[i] = first.pred.args[i];
				if (first.pred.args[i] != NULL)
					first.pred.args[i]->reference_count++;
			}
		}

		const datalog_expression* arg = first.pred.args[FieldIndex];
		if (arg == NULL) {
			if (second.type != DATALOG_EMPTY && second.type != DATALOG_ANY) {
				free(inverse); return false;
			}
		} else if (!intersect(inverse.pred.args[FieldIndex], arg, &second)) {
			inverse.pred.args[FieldIndex] = NULL;
			free(inverse); return false;
		}
		if (inverse.pred.args[FieldIndex] != NULL && inverse.pred.args[FieldIndex]->type == DATALOG_EMPTY) {
			free(*inverse.pred.args[FieldIndex]);
			if (inverse.pred.args[FieldIndex]->reference_count == 0)
				free(inverse.pred.args[FieldIndex]);
			inverse.pred.args[FieldIndex] = NULL;

			for (unsigned int i = FieldIndex + 1; i < array_length(inverse.pred.args); i++) {
				if (first.pred.args[i] != NULL && !can_be_empty(*first.pred.args[i])) {
					free(inverse); return false;
				}
				inverse.pred.args[i] = NULL;
			}
		} else {
			for (unsigned int i = FieldIndex + 1; i < array_length(inverse.pred.args); i++) {
				if (OtherArgsEmpty) {
					if (first.pred.args[i] != NULL && !can_be_empty(*first.pred.args[i])) {
						free(inverse); return false;
					}
					inverse.pred.args[i] = NULL;
				} else {
					inverse.pred.args[i] = first.pred.args[i];
					if (first.pred.args[i] != NULL)
						first.pred.args[i]->reference_count++;
				}
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldIndex>
bool invert_delete_arg(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY
	 || (first.type == DATALOG_TUPLE && first.tuple.elements.length == 0 && first.tuple.position != POSITION_EXACT)) {
		if (second.type != DATALOG_PREDICATE || (second.pred.args[FieldIndex] != NULL && second.pred.args[FieldIndex]->type != DATALOG_EMPTY))
			return false;

		inverse.type = DATALOG_PREDICATE;
		inverse.pred.function = second.pred.function;
		if (second.pred.excluded_count > 0 && !init_excluded(inverse.pred.excluded, second.pred.excluded, second.pred.excluded_count))
			return false;
		inverse.pred.excluded_count = second.pred.excluded_count;
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(second.pred.args); i++) {
			if (i == FieldIndex) continue;
			inverse.pred.args[i] = second.pred.args[i];
			if (second.pred.args[i] != NULL)
				second.pred.args[i]->reference_count++;
		}
		inverse.pred.args[FieldIndex] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count++;
	} else if (first.type == DATALOG_PREDICATE) {
		if (second.type != DATALOG_PREDICATE || (second.pred.args[FieldIndex] != NULL && second.pred.args[FieldIndex]->type != DATALOG_EMPTY))
			return false;

		inverse.type = DATALOG_PREDICATE;
		if (!intersect_labels(
				inverse.pred.function, inverse.pred.excluded, inverse.pred.excluded_count,
				first.pred.function, first.pred.excluded, first.pred.excluded_count,
				second.pred.function, second.pred.excluded, second.pred.excluded_count)) {
			free(inverse); return false;
		}
		inverse.reference_count = 1;

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		for (unsigned int i = 0; i < array_length(inverse.pred.args); i++)
			inverse.pred.args[i] = NULL;
		for (unsigned int i = 0; i < array_length(inverse.pred.args); i++) {
			if (i == FieldIndex) continue;
			if (first.pred.args[i] == NULL) {
				if (second.pred.args[i] != NULL && second.pred.args[i]->type != DATALOG_ANY && second.pred.args[i]->type != DATALOG_EMPTY) {
					free(inverse); return false;
				}
			} else if (second.pred.args[i] == NULL) {
				if (first.pred.args[i]->type != DATALOG_ANY && first.pred.args[i]->type != DATALOG_EMPTY) {
					free(inverse); return false;
				}
			} else if (!intersect(
					inverse.pred.args[i], first.pred.args[i], second.pred.args[i],
					first_variable_map, second_variable_map, dst_variable_map)) {
				inverse.pred.args[i] = NULL;
				free(inverse); return false;
			}
		}

		inverse.pred.args[FieldIndex] = first.pred.args[FieldIndex];
		if (first.pred.args[FieldIndex] != NULL)
			first.pred.args[FieldIndex]->reference_count++;
	} else {
		return false;
	}
	return true;
}

bool invert_delete_args(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (second.type != DATALOG_PREDICATE)
			return false;
		for (unsigned int i = 0; i < array_length(second.pred.args); i++)
			if (second.pred.args[i] != NULL && !can_be_empty(*second.pred.args[i])) return false;

		inverse.type = DATALOG_PREDICATE;
		inverse.pred.function = second.pred.function;
		if (second.pred.excluded_count > 0 && !init_excluded(inverse.pred.excluded, second.pred.excluded, second.pred.excluded_count))
			return false;
		inverse.pred.excluded_count = second.pred.excluded_count;
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(second.pred.args); i++)
			inverse.pred.args[i] = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count += array_length(second.pred.args);
	} else if (first.type == DATALOG_PREDICATE) {
		if (second.type != DATALOG_PREDICATE)
			return false;
		for (unsigned int i = 0; i < array_length(second.pred.args); i++)
			if (second.pred.args[i] != NULL && !can_be_empty(*second.pred.args[i])) return false;

		inverse.type = DATALOG_PREDICATE;
		if (!intersect_labels(
				inverse.pred.function, inverse.pred.excluded, inverse.pred.excluded_count,
				first.pred.function, first.pred.excluded, first.pred.excluded_count,
				second.pred.function, second.pred.excluded, second.pred.excluded_count)) {
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(inverse.pred.args); i++) {
			inverse.pred.args[i] = first.pred.args[i];
			if (first.pred.args[i] != NULL)
				first.pred.args[i]->reference_count++;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int HeadIndex, unsigned int SelectIndex, bool OtherArgsEmpty>
bool invert_head_arg_select_arg(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_TUPLE) {
		if (first.tuple.elements.length == 0) {
			if (first.tuple.position == POSITION_EXACT) return false;
			if (!invert_select_arg<SelectIndex, false>(inverse, DATALOG_NON_EMPTY_TREE, second))
				return false;
		} else if (first.tuple.elements.length > 1) {
			return false;
		} else {
			if (!invert_select_arg<SelectIndex, false>(inverse, *first.tuple.elements[0], second))
				return false;
		}
	} else {
		if (first.type == DATALOG_EMPTY) return false;
		if (!invert_select_arg<SelectIndex, false>(inverse, first, second))
			return false;
	}

	if (inverse.pred.args[HeadIndex]->type == DATALOG_ANY || inverse.pred.args[HeadIndex]->type == DATALOG_NON_EMPTY) {
		if (inverse.pred.args[HeadIndex]->reference_count > 1) {
			free(*inverse.pred.args[HeadIndex]);
			if (!new_expression(inverse.pred.args[HeadIndex])) return false;
			inverse.pred.args[HeadIndex]->reference_count = 1;
		}
		inverse.pred.args[HeadIndex]->variable = 1;
		inverse.pred.args[HeadIndex]->type = DATALOG_VARIABLE;
	} else if (inverse.pred.args[HeadIndex]->type == DATALOG_VARIABLE) {
		if (inverse.pred.args[HeadIndex]->variable == 0) {
			if (inverse.pred.args[HeadIndex]->reference_count > 1) {
				free(*inverse.pred.args[HeadIndex]);
				if (!new_expression(inverse.pred.args[HeadIndex])) return false;
				inverse.pred.args[HeadIndex]->reference_count = 1;
			}
			inverse.pred.args[HeadIndex]->variable = 1;
		} else if (inverse.pred.args[HeadIndex]->variable != 1)
			return false;
	} else {
		return false;
	}

	for (unsigned int i = 0; OtherArgsEmpty && i < array_length(inverse.pred.args); i++) {
		if (i == HeadIndex || i == SelectIndex || inverse.pred.args[i] == NULL) continue;
		else if (!can_be_empty(*inverse.pred.args[i]))
			return false;
		free(*inverse.pred.args[i]);
		if (inverse.pred.args[i]->reference_count == 0)
			free(inverse.pred.args[i]);
		inverse.pred.args[i] = NULL;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool MergeVariables = true>
bool invert_select_left_keep_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		/* we're not implementing the any-any case */
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (second.func.excluded_count > 0)
				/* prune cases where the variables are unknown */
				return false;
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = second.func.function;
			if (second.func.excluded_count > 0
			 && !inverse.func.exclude(second.func.excluded, second.func.excluded_count))
				return false;
		}
		for (unsigned int i = 0; i < array_length(second.func.vars); i++)
			inverse.func.vars[i] = second.func.vars[i];

		if (!new_expression(inverse.func.arg)
		 || !invert_select_any<FieldCount, true, POSITION_LEFT>(*inverse.func.arg, *second.func.arg)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;
	} else if (first.type == DATALOG_FUNCTION) {
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate)) {
					return false;
				} else if (second.func.function == DATALOG_LABEL_WILDCARD) {
					/* prune cases where the variables are unknown */
					return false;
				} else if (second.func.function != Predicate) {
					return false;
				}
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else if (!intersect_labels(
				inverse.func.function, inverse.func.excluded, inverse.func.excluded_count,
				first.func.function, first.func.excluded, first.func.excluded_count,
				second.func.function, second.func.excluded, second.func.excluded_count)) {
			return false;
		}

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		for (unsigned int i = 0; MergeVariables && i < array_length(inverse.func.vars); i++) {
			unsigned int var = second.func.vars[i];
			if (var != 0) {
				second_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_select_left<FieldCount, true, MergeVariables>(*inverse.func.arg, *first.func.arg, *second.func.arg, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(inverse.func.vars); i++) {
			unsigned int first_var = first.func.vars[i];
			unsigned int second_var = second.func.vars[i];
			if (first_var == 0) {
				if (second_var == 0) {
					inverse.func.vars[i] = 0;
				} else {
					if (second_variable_map[second_var] != 0) {
						inverse.func.vars[i] = second_variable_map[second_var];
					} else {
						inverse.func.vars[i] = second_var;
						second_variable_map[second_var] = second_var;
						dst_variable_map[second_var] = 1;
					}
				}
			} else if (second_var == 0) {
				if (first_variable_map[first_var] != 0) {
					inverse.func.vars[i] = first_variable_map[first_var];
				} else {
					inverse.func.vars[i] = first_var;
					second_variable_map[first_var] = first_var;
					dst_variable_map[first_var] = 1;
				}
			} else {
				unsigned int intersection = intersect_variable(first_var, second_var,
						first_variable_map, second_variable_map, dst_variable_map);
				if (intersection == 0) {
					/* the intersection is empty */
					free(inverse);
					return false;
				}
				inverse.func.vars[i] = intersection;
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_delete_left_keep_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		/* we're not implementing the any-any case */
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (second.func.excluded_count > 0)
				/* prune cases where the variables are unknown */
				return false;
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = second.func.function;
			if (second.func.excluded_count > 0
			 && !inverse.func.exclude(second.func.excluded, second.func.excluded_count)) {
				return false;
			}
		}
		for (unsigned int i = 0; i < array_length(second.func.vars); i++) {
			if (second.func.vars[i] == 0) inverse.func.vars[i] = 0;
			else inverse.func.vars[i] = second.func.vars[i] + (KeepHead ? 0 : 1);
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, DATALOG_ANY_TREE, *second.func.arg)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;
	} else if (first.type == DATALOG_FUNCTION) {
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate)) {
					return false;
				} else if (second.func.function == DATALOG_LABEL_WILDCARD) {
					/* prune cases where the variables are unknown */
					return false;
				} else if (second.func.function != Predicate) {
					return false;
				}
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else if (!intersect_labels(
				inverse.func.function, inverse.func.excluded, inverse.func.excluded_count,
				first.func.function, first.func.excluded, first.func.excluded_count,
				second.func.function, second.func.excluded, second.func.excluded_count)) {
			return false;
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, *second.func.arg, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(inverse.func.vars); i++) {
			unsigned int first_var = first.func.vars[i];
			unsigned int second_var = second.func.vars[i];
			if (first_var == 0) {
				if (second_var == 0) {
					inverse.func.vars[i] = 0;
				} else {
					if (second_variable_map[second_var] != 0) {
						inverse.func.vars[i] = second_variable_map[second_var];
					} else {
						inverse.func.vars[i] = second_var + (KeepHead ? 0 : 1);
						second_variable_map[second_var] = second_var + (KeepHead ? 0 : 1);
						dst_variable_map[second_var + (KeepHead ? 0 : 1)] = 1;
					}
				}
			} else if (second_var == 0) {
				if (first_variable_map[first_var] != 0) {
					inverse.func.vars[i] = first_variable_map[first_var];
				} else {
					inverse.func.vars[i] = first_var;
					second_variable_map[first_var] = first_var;
					dst_variable_map[first_var] = 1;
				}
			} else {
				unsigned int intersection = intersect_variable(first_var, second_var,
						first_variable_map, second_variable_map, dst_variable_map);
				if (intersection == 0) {
					/* the intersection is empty */
					free(inverse);
					return false;
				}
				inverse.func.vars[i] = intersection;
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool invert_delete_left_keep_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_delete_left_keep_function<FieldCount, Predicate, KeepHead>(
			inverse, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
bool invert_delete_right_keep_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		/* we're not implementing the any-any case */
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (second.func.excluded_count > 0)
				/* prune cases where the variables are unknown */
				return false;
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = second.func.function;
			if (second.func.excluded_count > 0
			 && !inverse.func.exclude(second.func.excluded, second.func.excluded_count)) {
				return false;
			}
		}
		for (unsigned int i = 0; i < array_length(second.func.vars); i++) {
			if (second.func.vars[i] == 0) inverse.func.vars[i] = 0;
			else inverse.func.vars[i] = second.func.vars[i] + (KeepHead ? 0 : 1);
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_delete_right<FieldCount, KeepHead>(*inverse.func.arg, DATALOG_ANY_TREE, *second.func.arg)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;
	} else if (first.type == DATALOG_FUNCTION) {
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate)) {
					return false;
				} else if (second.func.function == DATALOG_LABEL_WILDCARD) {
					/* prune cases where the variables are unknown */
					return false;
				} else if (second.func.function != Predicate) {
					return false;
				}
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else if (!intersect_labels(
				inverse.func.function, inverse.func.excluded, inverse.func.excluded_count,
				first.func.function, first.func.excluded, first.func.excluded_count,
				second.func.function, second.func.excluded, second.func.excluded_count)) {
			return false;
		}

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		if (!new_expression(inverse.func.arg)
		 || !invert_delete_right<FieldCount, KeepHead>(*inverse.func.arg, *first.func.arg, *second.func.arg, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(inverse.func.vars); i++) {
			unsigned int first_var = first.func.vars[i];
			unsigned int second_var = second.func.vars[i];
			if (first_var == 0) {
				if (second_var == 0) {
					inverse.func.vars[i] = 0;
				} else {
					if (second_variable_map[second_var] != 0) {
						inverse.func.vars[i] = second_variable_map[second_var];
					} else {
						inverse.func.vars[i] = second_var + (KeepHead ? 0 : 1);
						second_variable_map[second_var] = second_var + (KeepHead ? 0 : 1);
						dst_variable_map[second_var + (KeepHead ? 0 : 1)] = 1;
					}
				}
			} else if (second_var == 0) {
				if (first_variable_map[first_var] != 0) {
					inverse.func.vars[i] = first_variable_map[first_var];
				} else {
					inverse.func.vars[i] = first_var;
					second_variable_map[first_var] = first_var;
					dst_variable_map[first_var] = 1;
				}
			} else {
				unsigned int intersection = intersect_variable(first_var, second_var,
						first_variable_map, second_variable_map, dst_variable_map);
				if (intersection == 0) {
					/* the intersection is empty */
					free(inverse);
					return false;
				}
				inverse.func.vars[i] = intersection;
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_select_left_delete_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY
	 || (first.type == DATALOG_TUPLE && first.tuple.elements.length == 0 && first.tuple.position != POSITION_EXACT)) {
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		inverse.func.excluded_count = 0;
		inverse.func.function = Predicate;
		set_function_variables<Predicate>(inverse.func);
		if (!new_expression(inverse.func.arg)
		 || !invert_select_any<FieldCount, KeepHead, POSITION_LEFT>(*inverse.func.arg, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate))
					return false;
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = first.func.function;
			if (first.func.excluded_count > 0
			 && !inverse.func.exclude(first.func.excluded, first.func.excluded_count)) {
				free(inverse);
				return false;
			}
		}

		for (unsigned int i = 0; MergeVariables && i < array_length(first.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			if (var != 0) {
				first_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_select_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, *first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			if (var != 0) {
				if (first_variable_map[var] == 0) {
					first_variable_map[var] = var;
					dst_variable_map[var] = 1;
				} else {
					var = first_variable_map[var];
				}
			}
			inverse.func.vars[i] = var;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
inline bool invert_select_left_delete_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_select_left_delete_function<FieldCount, Predicate, KeepHead>(
			inverse, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
bool invert_select_right_delete_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		inverse.func.excluded_count = 0;
		inverse.func.function = Predicate;
		set_function_variables<Predicate>(inverse.func);
		if (!new_expression(inverse.func.arg)
		 || !invert_select_any<FieldCount, KeepHead, POSITION_RIGHT>(*inverse.func.arg, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate))
					return false;
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = first.func.function;
			if (first.func.excluded_count > 0
			 && !inverse.func.exclude(first.func.excluded, first.func.excluded_count)) {
				return false;
			}
		}

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		if (!new_expression(inverse.func.arg)
		 || !invert_select_right<FieldCount, KeepHead>(*inverse.func.arg, *first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			inverse.func.vars[i] = var;
			if (var != 0) {
				first_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_delete_left_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second,
		array<unsigned int>& first_variable_map, array<unsigned int>& second_variable_map,
		array<unsigned int>& dst_variable_map)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		inverse.func.excluded_count = 0;
		inverse.func.function = Predicate;
		set_function_variables<Predicate>(inverse.func);
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg, DATALOG_ANY_TREE, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.function == DATALOG_LABEL_WILDCARD) {
				if (first.func.is_excluded(Predicate))
					return false;
			} else if (first.func.function != Predicate) {
				return false;
			}
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = first.func.function;
			if (first.func.excluded_count > 0
			 && !inverse.func.exclude(first.func.excluded, first.func.excluded_count)) {
				return false;
			}
		}

		for (unsigned int i = 0; MergeVariables && i < array_length(inverse.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			if (var != 0) {
				first_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}

		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left<FieldCount, KeepHead, MergeVariables>(*inverse.func.arg,
				*first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			if (inverse.func.excluded_count > 0) free(inverse.func.excluded);
			return false;
		}
		inverse.reference_count = 1;

		for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			inverse.func.vars[i] = var;
			if (var != 0) {
				first_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
inline bool invert_delete_left_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	array<unsigned int> first_variable_map = array<unsigned int>(8);
	array<unsigned int> second_variable_map = array<unsigned int>(8);
	array<unsigned int> dst_variable_map = array<unsigned int>(8);
	memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
	memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
	memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
	return invert_delete_left_function<FieldCount, Predicate, KeepHead, MergeVariables>(
			inverse, first, second, first_variable_map, second_variable_map, dst_variable_map);
}

template<unsigned int Predicate, bool KeepHead>
bool invert_select_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (second.func.excluded_count > 0 && second.func.is_excluded(Predicate)) {
				return false;
			}
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = second.func.function;
			if (second.func.excluded_count > 0
			 && !inverse.func.exclude(second.func.excluded, second.func.excluded_count)) {
				return false;
			}
		}
		inverse.reference_count = 1;
		inverse.func.arg = &DATALOG_ANY_TREE;
		DATALOG_ANY_TREE.reference_count++;
		for (unsigned int i = 0; i < array_length(second.func.vars); i++) {
			if (second.func.vars[i] == 0) inverse.func.vars[i] = 0;
			else inverse.func.vars[i] = second.func.vars[i] + (KeepHead ? 0 : 1);
		}
	} else if (first.type == DATALOG_FUNCTION) {
		if (second.type != DATALOG_FUNCTION) return false;

		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if ((first.func.excluded_count > 0 && first.func.is_excluded(Predicate))
			 || (second.func.excluded_count > 0 && second.func.is_excluded(Predicate))) {
				return false;
			}
			inverse.func.function = Predicate;
		} else {
			if (!intersect_labels(
					inverse.func.function, inverse.func.excluded, inverse.func.excluded_count,
					first.func.function, first.func.excluded, first.func.excluded_count,
					second.func.function, second.func.excluded, second.func.excluded_count)) {
				return false;
			}
		}
		inverse.reference_count = 1;
		inverse.func.arg = first.func.arg;
		first.func.arg->reference_count++;

		for (unsigned int i = 0; i < array_length(inverse.func.vars); i++) {
			unsigned int first_var = first.func.vars[i];
			unsigned int second_var = second.func.vars[i];
			if (second_var != 0 && !KeepHead) second_var++;
			if (first_var == 0) {
				if (second_var == 0)
					inverse.func.vars[i] = 0;
				else inverse.func.vars[i] = second_var;
			} else {
				if (second_var == 0) {
					inverse.func.vars[i] = first_var;
				} else if (first_var != second_var) {
					free(inverse);
					return false;
				} else {
					inverse.func.vars[i] = second_var;
				}
			}
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_delete_left_answer_keep_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		set_function_variables<PREDICATE_ANSWER>(inverse.func);
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left_keep_function<FieldCount, Predicate, KeepHead>(*inverse.func.arg, DATALOG_ANY_TREE, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		if (first.func.function == DATALOG_LABEL_WILDCARD) {
			if (first.func.is_excluded(PREDICATE_ANSWER))
				return false;
			set_function_variables<PREDICATE_ANSWER>(inverse.func);
		} else if (first.func.function == PREDICATE_ANSWER) {
			for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
				unsigned int var = first.func.vars[i];
				inverse.func.vars[i] = var;
				if (var != 0) {
					first_variable_map[var] = var;
					dst_variable_map[var] = 1;
				}
			}
		} else return false;

		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left_keep_function<FieldCount, Predicate, KeepHead, MergeVariables>(*inverse.func.arg,
				*first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_select_left_delete_function_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		set_function_variables<PREDICATE_ANSWER>(inverse.func);
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_left_delete_function<FieldCount, Predicate, KeepHead>(*inverse.func.arg, DATALOG_ANY_TREE, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		if (first.func.function == DATALOG_LABEL_WILDCARD) {
			if (first.func.is_excluded(PREDICATE_ANSWER))
				return false;
			set_function_variables<PREDICATE_ANSWER>(inverse.func);
		} else if (first.func.function == PREDICATE_ANSWER) {
			for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
				unsigned int var = first.func.vars[i];
				inverse.func.vars[i] = var;
				if (var != 0) {
					first_variable_map[var] = var;
					dst_variable_map[var] = 1;
				}
			}
		} else return false;
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_select_left_delete_function<FieldCount, Predicate, KeepHead, MergeVariables>(*inverse.func.arg,
				*first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int FieldCount, unsigned int Predicate, bool KeepHead>
bool invert_delete_left_function_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		set_function_variables<PREDICATE_ANSWER>(inverse.func);
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left_function<FieldCount, Predicate, KeepHead>(*inverse.func.arg, DATALOG_ANY_TREE, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {

		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);

		if (first.func.function == DATALOG_LABEL_WILDCARD) {
			if (first.func.is_excluded(PREDICATE_ANSWER))
				return false;
			set_function_variables<PREDICATE_ANSWER>(inverse.func);
		} else if (first.func.function == PREDICATE_ANSWER) {
			for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
				unsigned int var = first.func.vars[i];
				inverse.func.vars[i] = var;
				if (var != 0) {
					first_variable_map[var] = var;
					dst_variable_map[var] = 1;
				}
			}
		} else return false;
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_left_function<FieldCount, Predicate, KeepHead>(*inverse.func.arg,
				*first.func.arg, second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int Predicate, bool KeepHead, bool MergeVariables = true>
bool invert_delete_function(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY
	 || (first.type == DATALOG_TUPLE && first.tuple.elements.length == 0 && first.tuple.position != POSITION_EXACT)) {
		inverse.type = DATALOG_FUNCTION;
		inverse.func.function = Predicate;
		inverse.func.excluded_count = 0;
		inverse.reference_count = 1;
		if (!init(inverse.func.arg, second, KeepHead ? 0 : 1))
			return false;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			set_function_variables<Predicate>(inverse.func);
		} else {
			for (unsigned int i = 0; i < array_length(inverse.func.vars); i++)
				inverse.func.vars[i] = 0;
		}
	} else if (first.type == DATALOG_FUNCTION) {
		inverse.type = DATALOG_FUNCTION;
		inverse.func.excluded_count = 0;
		if (Predicate != DATALOG_LABEL_WILDCARD) {
			if (first.func.excluded_count > 0 && first.func.is_excluded(Predicate))
				return false;
			inverse.func.function = Predicate;
		} else {
			inverse.func.function = first.func.function;
			if (first.func.excluded_count > 0
			 && !inverse.func.exclude(first.func.excluded, first.func.excluded_count))
				return false;
		}

		array<bool> first_variables = array<bool>(8);
		memset(first_variables.data, 0, sizeof(bool) * first_variables.capacity);
		array<unsigned int> first_variable_map = array<unsigned int>(8);
		array<unsigned int> second_variable_map = array<unsigned int>(8);
		array<unsigned int> dst_variable_map = array<unsigned int>(8);
		memset(first_variable_map.data, 0, sizeof(unsigned int) * first_variable_map.capacity);
		memset(second_variable_map.data, 0, sizeof(unsigned int) * second_variable_map.capacity);
		memset(dst_variable_map.data, 0, sizeof(unsigned int) * dst_variable_map.capacity);
		for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
			unsigned int var = first.func.vars[i];
			if (var != 0) {
				first_variable_map[var] = var;
				dst_variable_map[var] = 1;
			}
		}
		if (!init_variable_maps<KeepHead, MergeVariables>(second, first_variables, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.excluded_count > 0)
				free(inverse.func.excluded);
			return false;
		}

		inverse.reference_count = 1;
		if (!intersect(inverse.func.arg, first.func.arg, &second, first_variable_map, second_variable_map, dst_variable_map)) {
			if (inverse.func.excluded_count > 0)
				free(inverse.func.excluded);
			return false;
		}
		for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
			if (first.func.vars[i] == 0) {
				inverse.func.vars[i] = 0;
				continue;
			}

			unsigned int var = first.func.vars[i];
			if (first_variable_map[var] != 0) {
				inverse.func.vars[i] = first_variable_map[var];
				continue;
			}

			while (dst_variable_map[var] != 0) {
				var++;
				if (!ensure_variable_map_capacity(dst_variable_map, var)) {
					free(inverse);
					return false;
				}
			}

			inverse.func.vars[i] = var;
		}

	} else {
		return false;
	}
	return true;
}

template<unsigned int Predicate, bool KeepHead>
bool invert_delete_function_answer(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		set_function_variables<PREDICATE_ANSWER>(inverse.func);
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_function<Predicate, KeepHead>(*inverse.func.arg, DATALOG_ANY_TREE, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else if (first.type == DATALOG_FUNCTION) {

		if (first.func.function == DATALOG_LABEL_WILDCARD) {
			if (first.func.is_excluded(PREDICATE_ANSWER))
				return false;
			set_function_variables<PREDICATE_ANSWER>(inverse.func);
		} else if (first.func.function == PREDICATE_ANSWER) {
			for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
				unsigned int var = first.func.vars[i];
				inverse.func.vars[i] = var;
			}
		} else return false;
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)
		 || !invert_delete_function<Predicate, KeepHead>(*inverse.func.arg, *first.func.arg, second)) {
			if (inverse.func.arg != NULL) free(inverse.func.arg);
			return false;
		}
	} else {
		return false;
	}
	return true;
}

template<unsigned int Predicate, unsigned int Direction>
inline bool set_predicate_answer(datalog_expression& inverse, const datalog_expression& first) {
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		set_function_variables<PREDICATE_ANSWER>(inverse.func);
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!new_expression(inverse.func.arg)) free(inverse);
		inverse.func.arg->type = DATALOG_ANY;
		inverse.func.arg->reference_count = 1;
	} else if (first.type == DATALOG_FUNCTION) {

		if (first.func.function == DATALOG_LABEL_WILDCARD) {
			if (first.func.is_excluded(PREDICATE_ANSWER))
				return false;
			set_function_variables<PREDICATE_ANSWER>(inverse.func);
		} else if (first.func.function == PREDICATE_ANSWER) {
			for (unsigned int i = 0; i < array_length(first.func.vars); i++) {
				unsigned int var = first.func.vars[i];
				inverse.func.vars[i] = var;
			}
		} else {
			return false;
		}
		inverse.func.function = PREDICATE_ANSWER;
		inverse.func.excluded_count = 0;
		inverse.type = DATALOG_FUNCTION;
		inverse.reference_count = 1;
		if (!init(inverse.func.arg, first.func.arg))
			return false;
	} else {
		return false;
	}
	if (!set_predicate<0, false, true>(*inverse.func.arg, Predicate)
	 || !set_direction<false>(*inverse.func.arg, Direction)) {
		free(inverse);
		return false;
	}
	return true;
}

template<unsigned int Length>
inline bool invert_tuple_length(datalog_expression& inverse,
		const datalog_expression& first, const datalog_expression& second)
{
	if (first.type == DATALOG_ANY || first.type == DATALOG_NON_EMPTY) {
		if (!tuple_length<Length>(second, inverse))
			return false;
	} else if (first.type == DATALOG_TUPLE) {
		datalog_expression* intermediate;
		if (!new_expression(intermediate)) return false;
		if (!tuple_length<Length>(first, *intermediate)) {
			free(intermediate); return false;
		}

		if (!intersect(inverse, intermediate, &second)) {
			free(*intermediate); free(intermediate);
			return false;
		}
		free(*intermediate); free(intermediate);
	} else {
		return false;
	}
	return true;
}

bool invert(
	datalog_expression_root& inverse, datalog_expression_root::function function,
	const datalog_expression_root& first, const datalog_expression_root& second)
{
	/* first invert the syntactic features */
	if (function.type == datalog_expression_root::FUNCTION_NULL
	 || function.type == datalog_expression_root::FUNCTION_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION
	 || function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES
	 || function.type == datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES)
	{
		inverse.inf = first.inf;
		inverse.index = first.index;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_SINGULAR)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_UNCOUNTABLE) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_UNCOUNTABLE)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_SINGULAR)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_UNCOUNTABLE)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_NON_SINGULAR)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_NON_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_NON_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		if (inverse.index != NUMBER_SINGULAR) return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_CONCORD_ALL) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_ALL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.index, first.index, second.index))
			return false;
		if (inverse.index == NUMBER_SINGULAR) return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR
			|| function.type == datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_ALL
		  && second.concord != NUMBER_NON_PLURAL && second.concord != NUMBER_SINGULAR)
		 || !intersect(inverse.inf, first.inf, second.inf)
		 || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_ALL
		  && second.concord != NUMBER_NON_SINGULAR && second.concord != NUMBER_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf)
		 || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE) {
		if ((second.concord != NUMBER_ANY && second.concord != NUMBER_ALL
		  && second.concord != NUMBER_NON_PLURAL && second.concord != NUMBER_NON_SINGULAR
		  && second.concord != NUMBER_UNCOUNTABLE)
		 || !intersect(inverse.inf, first.inf, second.inf)
		 || !intersect(inverse.index, first.index, second.index))
			return false;
		inverse.concord = first.concord;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_SINGULAR) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_SINGULAR)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PLURAL
			|| function.type == datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_PLURAL)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE) {
		if ((second.index != NUMBER_ANY && second.index != NUMBER_UNCOUNTABLE)
		 || !intersect(inverse.inf, first.inf, second.inf) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.index = first.index;
	} else if (function.type == datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE) {
		if ((second.inf != INFLECTION_PRESENT_PARTICIPLE && second.inf != INFLECTION_ANY)
		 || !intersect(inverse.index, first.index, second.index) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.inf = first.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_PAST_PARTICIPLE
			|| function.type == datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE) {
		if ((second.inf != INFLECTION_PAST_PARTICIPLE && second.inf != INFLECTION_ANY)
		 || !intersect(inverse.index, first.index, second.index) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.inf = first.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE) {
		if ((first.inf != INFLECTION_PRESENT_PARTICIPLE && first.inf != INFLECTION_ANY && first.inf != INFLECTION_NONE)
		 || (second.inf != INFLECTION_PRESENT_PARTICIPLE && second.inf != INFLECTION_ANY)
		 || !intersect(inverse.index, first.index, second.index) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.inf = first.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE
			|| function.type == datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE) {
		if ((first.inf != INFLECTION_PAST_PARTICIPLE && first.inf != INFLECTION_ANY && first.inf != INFLECTION_NONE)
		 || (second.inf != INFLECTION_PAST_PARTICIPLE && second.inf != INFLECTION_ANY)
		 || !intersect(inverse.index, first.index, second.index) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.inf = first.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_INFINITIVE
			|| function.type == datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE) {
		if ((second.inf != INFLECTION_INFINITIVE && second.inf != INFLECTION_ANY)
		 || !intersect(inverse.index, first.index, second.index) || !intersect(inverse.concord, first.concord, second.concord))
			return false;
		inverse.inf = first.inf;
	} else if (function.type == datalog_expression_root::FUNCTION_IDENTITY_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD
			|| function.type == datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD) {
		if ((first.index == NUMBER_SINGULAR && second.concord == NUMBER_PLURAL) || first.concord == NUMBER_NONE
		 || !intersect(inverse.inf, first.inf, second.inf)
		 || !intersect(inverse.concord, first.concord, second.concord)
		 || !has_intersection(inverse.concord, second.index))
			return false;
		inverse.index = first.index;
	} else {
		if (!intersect(inverse.index, first.index, second.index)
		 || !intersect(inverse.concord, first.concord, second.concord)
		 || !intersect(inverse.inf, first.inf, second.inf))
			return false;
	}

	/* next, invert the semantic component */
	datalog_expression exp;
	switch (function.type) {
	case datalog_expression_root::FUNCTION_IDENTITY:
	case datalog_expression_root::FUNCTION_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SINGULAR:
	case datalog_expression_root::FUNCTION_PLURAL:
	case datalog_expression_root::FUNCTION_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_ALL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_INFINITIVE:
	case datalog_expression_root::FUNCTION_IDENTITY_COORD:
		return intersect(inverse.root, &first.root, &second.root);
	case datalog_expression_root::FUNCTION_NULL:
	case datalog_expression_root::FUNCTION_KEEP_FEATURES:
		if (second.root.type != DATALOG_ANY && second.root.type != DATALOG_EMPTY)
			return false;
		return init(inverse.root, first.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_COORD:
		return invert_select_left<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD:
		return invert_select_left<1, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES:
		return invert_select_left<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD:
		return invert_select_left<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DISJOINT:
		return invert_select_left<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT4_DISJOINT:
		return invert_select_left<4, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DISJOINT:
		return invert_select_left<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT6_DISJOINT:
		return invert_select_left<6, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT7_DISJOINT:
		return invert_select_left<7, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR:
		return invert_select_left<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD:
		return invert_select_left<1, false, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION:
		return invert_select_left_keep_function<1, DATALOG_LABEL_WILDCARD>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT:
		return invert_select_left_keep_function<1, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION:
		return invert_select_left_keep_function<2, DATALOG_LABEL_WILDCARD>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT:
		return invert_select_left_keep_function<2, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_KEEP_FUNCTION:
		return invert_select_left_keep_function<3, DATALOG_LABEL_WILDCARD>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES:
		return invert_select_left_delete_function<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER:
		return invert_select_left_delete_answer<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_ANSWER:
		return invert_select_left_delete_answer<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER:
		return invert_select_left_delete_answer<3, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT:
		return invert_select_left_delete_answer<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT:
		return invert_select_left_delete_answer<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD:
		return invert_select_left_delete_answer<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER:
		return invert_select_left_delete_function_answer<1, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER:
		return invert_select_left_delete_function_answer<2, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER:
		return invert_select_left_delete_function_answer<3, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER:
		return invert_select_left_delete_function_answer<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER:
		return invert_select_left_delete_function_answer<2, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER:
		return invert_select_left_delete_function_answer<3, DATALOG_LABEL_WILDCARD, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES:
		return invert_select_left_delete_function<1, PREDICATE_NOT, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT:
		return invert_select_right<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR:
		return invert_select_right<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DISJOINT:
		return invert_select_right<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT3_DISJOINT:
		return invert_select_right<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT4_DISJOINT:
		return invert_select_right<4, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT5_DISJOINT:
		return invert_select_right<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT6_DISJOINT:
		return invert_select_right<6, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT7_DISJOINT:
		return invert_select_right<7, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_HEAD:
		return invert_select_right<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_FUNCTION:
		return invert_select_right_delete_function<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DELETE_ANSWER:
		return invert_select_right_delete_answer<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES:
		return invert_select_function<DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES:
		return invert_select_function<DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);

	case datalog_expression_root::FUNCTION_DELETE_LEFT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COORD:
		return invert_delete_left<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD:
		return invert_delete_left<1, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2:
		return invert_delete_left<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES:
		return invert_delete_left<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES:
		return invert_delete_left<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT4_DISJOINT:
		return invert_delete_left<4, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_DISJOINT:
		return invert_delete_left<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT6_DISJOINT:
		return invert_delete_left<6, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT7_DISJOINT:
		return invert_delete_left<7, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES:
		return invert_delete_left<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES:
		return invert_delete_left<2, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION:
		return invert_delete_left_function<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT:
		return invert_delete_left_function<1, DATALOG_LABEL_WILDCARD, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION:
		return invert_delete_left_function<2, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT:
		return invert_delete_left_function<2, DATALOG_LABEL_WILDCARD, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_HEAD:
		return invert_delete_left_function<1, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_HEAD:
		return invert_delete_left_function<3, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_ANSWER:
		return invert_delete_left_keep_function<1, PREDICATE_ANSWER, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION:
		return invert_delete_left_keep_function<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION:
		return invert_delete_left_keep_function<1, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER:
		return invert_delete_left_answer<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER:
		return invert_delete_left_answer<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER:
		return invert_delete_left_answer<3, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT:
		return invert_delete_left_answer<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT:
		return invert_delete_left_answer<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD:
		return invert_delete_left_answer<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COUNT_ANSWER:
		return invert_delete_left_function_answer<1, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_COUNT_ANSWER:
		return invert_delete_left_function_answer<2, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_COUNT_ANSWER:
		return invert_delete_left_function_answer<3, PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_ANSWER:
		return invert_delete_left_function_answer<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER:
		return invert_delete_left_function_answer<2, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER:
		return invert_delete_left_function_answer<3, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<1, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<1, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<2, DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<2, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<3, DATALOG_LABEL_WILDCARD, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION:
		return invert_delete_left_answer_keep_function<3, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT:
		return invert_delete_right<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2:
		return invert_delete_right<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_DISJOINT:
		return invert_delete_right<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT3_DISJOINT:
		return invert_delete_right<3, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT4_DISJOINT:
		return invert_delete_right<4, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT5_DISJOINT:
		return invert_delete_right<5, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT6_DISJOINT:
		return invert_delete_right<6, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT7_DISJOINT:
		return invert_delete_right<7, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD:
		return invert_delete_right<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION:
		return invert_delete_right_keep_function<1, DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_ANSWER:
		return invert_delete_right_answer<2, true, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT:
		return invert_delete_function<PREDICATE_COUNT, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL:
		return invert_delete_function<PREDICATE_COUNT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_COUNT_ANSWER:
		return invert_delete_function_answer<PREDICATE_COUNT, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_NOT:
	case datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL:
		return invert_delete_function<PREDICATE_NOT, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES:
		return invert_delete_left_function<1, PREDICATE_NOT, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT:
		return invert_delete_left_keep_function<1, PREDICATE_NOT, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES:
		return invert_delete_function<DATALOG_LABEL_WILDCARD, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_HEAD:
		return invert_delete_function<DATALOG_LABEL_WILDCARD, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ANSWER:
		return invert_delete_answer(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ANSWER_HAS_LOC:
		if (!invert_delete_answer(inverse.root, first.root, second.root)) return false;
		if (!has_predicate<PREDICATE_LOC>(inverse.root)) {
			free(inverse.root);
			return false;
		}
		return true;

	case datalog_expression_root::FUNCTION_SELECT_ARG1:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES:
		return invert_select_arg<0, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES:
		return invert_select_arg<0, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG2:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES:
		return invert_select_arg<1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES:
		return invert_select_arg<1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG3:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES:
		return invert_select_arg<2, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES:
		return invert_select_arg<2, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG1:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES:
		return invert_delete_arg<0>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG2:
	case datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES:
		return invert_delete_arg<1>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ARG3:
		return invert_delete_arg<2>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_DELETE_ARGS:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL:
		return invert_delete_args(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2:
		return invert_head_arg_select_arg<0, 1, false>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY:
		return invert_head_arg_select_arg<0, 1, true>(inverse.root, first.root, second.root);
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE:
		if (second.root.type == DATALOG_TUPLE) {
			if (second.root.tuple.elements.length > 0) return false;
		} else if (second.root.type != DATALOG_ANY && second.root.type != DATALOG_NON_EMPTY) return false;

		if (first.root.type == DATALOG_ANY || first.root.type == DATALOG_NON_EMPTY) {
			return init_tuple(inverse.root, POSITION_LEFT, 1);
		} else if (first.root.type != DATALOG_TUPLE && first.root.type != DATALOG_PREDICATE && first.root.type != DATALOG_FUNCTION) {
			return false;
		} else {
			return init(inverse.root, first.root);
		}
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY:
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR:
		if (second.root.type == DATALOG_TUPLE) {
			if (second.root.tuple.elements.length > 0) return false;
		} else if (second.root.type != DATALOG_ANY && second.root.type != DATALOG_NON_EMPTY) return false;

		if (first.root.type == DATALOG_ANY || first.root.type == DATALOG_NON_EMPTY) {
			return init_tuple(inverse.root, POSITION_EXACT, 1);
		} else if (first.root.type == DATALOG_EMPTY) {
			inverse.root.type = DATALOG_EMPTY;
			inverse.root.reference_count = 1;
			return true;
		} else if (first.root.type != DATALOG_TUPLE || first.root.tuple.elements.length > 0) {
			return false;
		} else {
			return init_tuple(inverse.root, POSITION_EXACT, 1);
		}
	case datalog_expression_root::FUNCTION_KEEP_NULL:
		if (second.root.type != DATALOG_ANY && second.root.type != DATALOG_EMPTY)
			return false;
		inverse.root.type = DATALOG_EMPTY;
		inverse.root.reference_count = 1;
		return true;
	case datalog_expression_root::FUNCTION_EMPTY_ARG2:
		if (!intersect(inverse.root, &first.root, &second.root))
			return false;
		if (inverse.root.type != DATALOG_PREDICATE
		 || (inverse.root.pred.args[1] != NULL && !can_be_empty(*inverse.root.pred.args[1]))) {
			free(inverse.root);
			return false;
		}
		if (inverse.root.pred.args[1] != NULL) {
			free(*inverse.root.pred.args[1]);
			if (inverse.root.pred.args[1]->reference_count == 0)
				free(inverse.root.pred.args[1]);
			inverse.root.pred.args[1] = NULL;
		}
		return true;
	case datalog_expression_root::FUNCTION_EMPTY_ARGS:
		if (!intersect(inverse.root, &first.root, &second.root)) return false;
		if (inverse.root.type != DATALOG_PREDICATE) return false;
		for (unsigned int i = 0; i < array_length(inverse.root.pred.args); i++) {
			if (inverse.root.pred.args[i] != NULL && !can_be_empty(*inverse.root.pred.args[i])) {
				free(inverse.root);
				return false;
			}
			if (inverse.root.pred.args[i] != NULL) {
				free(*inverse.root.pred.args[i]);
				if (inverse.root.pred.args[i]->reference_count == 0)
					free(inverse.root.pred.args[i]);
				inverse.root.pred.args[i] = NULL;
			}
		}
		return inverse.root.pred.function != DATALOG_LABEL_WILDCARD
			|| inverse.root.pred.exclude(&DATALOG_LABEL_EMPTY, 1);
	case datalog_expression_root::FUNCTION_ARG2_ZERO_ARITY:
		if (!intersect(inverse.root, &first.root, &second.root)) return false;
		if (inverse.root.type == DATALOG_ANY || inverse.root.type == DATALOG_NON_EMPTY) {
			if (!new_expression(inverse.root.pred.args[1])) return false;
			datalog_expression& arg = *inverse.root.pred.args[1];
			arg.type = DATALOG_PREDICATE;
			arg.reference_count = 1;
			arg.pred.function = DATALOG_LABEL_WILDCARD;
			arg.pred.excluded_count = 0;
			for (unsigned int i = 0; i < array_length(arg.pred.args); i++)
				arg.pred.args[i] = NULL;
			for (unsigned int i = 0; i < array_length(inverse.root.pred.args); i++)
				if (i != 1) inverse.root.pred.args[i] = &DATALOG_ANY_TREE;
			DATALOG_ANY_TREE.reference_count += array_length(inverse.root.pred.args) - 1;

			inverse.root.pred.function = DATALOG_LABEL_WILDCARD;
			if (!init_excluded(inverse.root.pred.excluded, &DATALOG_LABEL_EMPTY, 1))
				return false;
			inverse.root.pred.excluded_count = 1;
			inverse.root.type = DATALOG_PREDICATE;
			inverse.root.reference_count = 1;
		} else if (inverse.root.type == DATALOG_PREDICATE) {
			if (inverse.root.pred.function == DATALOG_LABEL_WILDCARD
			 && !inverse.root.pred.exclude(&DATALOG_LABEL_EMPTY, 1))
				return false;
			else if (inverse.root.pred.args[1] == NULL) return true;

			datalog_expression* arg;
			if (inverse.root.pred.args[1]->reference_count == 1) {
				arg = inverse.root.pred.args[1];
			} else {
				if (!init(arg, *inverse.root.pred.args[1])) exit(EXIT_FAILURE);
				free(*inverse.root.pred.args[1]);
				inverse.root.pred.args[1] = arg;
			}

			if (arg->type == DATALOG_ANY) {
				arg->type = DATALOG_PREDICATE;
				arg->pred.function = DATALOG_LABEL_WILDCARD;
				arg->pred.excluded_count = 0;
				for (unsigned int i = 0; i < array_length(arg->pred.args); i++)
					arg->pred.args[i] = NULL;
			} else if (arg->type == DATALOG_NON_EMPTY) {
				arg->type = DATALOG_PREDICATE;
				arg->pred.function = DATALOG_LABEL_WILDCARD;
				if (!init_excluded(arg->pred.excluded, &DATALOG_LABEL_EMPTY, 1)) exit(EXIT_FAILURE);
				arg->pred.excluded_count = 1;
				for (unsigned int i = 0; i < array_length(arg->pred.args); i++)
					arg->pred.args[i] = NULL;
			} else if (arg->type == DATALOG_PREDICATE) {
				for (unsigned int i = 0; i < array_length(arg->pred.args); i++) {
					if (arg->pred.args[i] != NULL) {
						if (!can_be_empty(*arg->pred.args[i])) {
							free(inverse.root); return false;
						}
						free(*arg->pred.args[i]);
						if (arg->pred.args[i]->reference_count == 0)
							free(arg->pred.args[i]);
						arg->pred.args[i] = NULL;
					}
				}
			} else {
				return false;
			}
		} else {
			return false;
		}
		return true;
	case datalog_expression_root::FUNCTION_LOC:
		if (second.root.type != DATALOG_ANY && second.root.type != DATALOG_EMPTY)
			return false;
		return set_predicate_answer<PREDICATE_LOC, DIRECTION_BACKWARD>(inverse.root, first.root);
	case datalog_expression_root::FUNCTION_TWO_PREDICATES:
		return invert_tuple_length<2>(inverse.root, first.root, second.root);

	case datalog_expression_root::FUNCTION_FLIP_PREDICATE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE:
		exp = second.root;
		if (!flip_predicate(exp) || !intersect(inverse.root, &first.root, &exp)) {
			free(exp); return false;
		}
		free(exp);
		return true;

	case datalog_expression_root::FUNCTION_EMPTY:
		break;
	}
	fprintf(stderr, "invert ERROR: Unrecognized transformation function.\n");
	return false;
}

bool invert(
	datalog_expression_root*& inverse,
	unsigned int& inverse_count,
	datalog_expression_root::function function,
	const datalog_expression_root& first,
	const datalog_expression_root& second)
{
	inverse = (datalog_expression_root*) malloc(sizeof(datalog_expression_root));
	if (inverse == NULL) {
		fprintf(stderr, "invert ERROR: Out of memory.\n");
		return false;
	} else if (!invert(*inverse, function, first, second)) {
		free(inverse);
		return false;
	}
	inverse_count = 1;

	unsigned int head = get_head(inverse->root);
	if (head != 1 && head != DATALOG_LABEL_EMPTY && head != DATALOG_LABEL_WILDCARD) {
		free(*inverse); free(inverse);
		return false;
	}
	return true;
}

inline bool any_number(const datalog_expression_root& src) {
	return src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY;
}

/* NOTE: this function assumes src is not DATALOG_ANY or DATALOG_NON_EMPTY */
inline bool get_number(const datalog_expression_root& src, int& value) {
	if (src.root.type != DATALOG_INTEGER)
		return false;
	value = src.root.integer;
	return true;
}

inline bool set_number(datalog_expression_root& exp, const datalog_expression_root& set, int value) {
	if (set.root.type != DATALOG_ANY && set.root.type != DATALOG_NON_EMPTY
	 && (set.root.type != DATALOG_INTEGER || set.root.integer != value))
		return false;
	exp.root.type = DATALOG_INTEGER;
	exp.root.integer = value;
	exp.root.reference_count = 1;
	exp.concord = set.concord;
	exp.index = set.index;
	exp.inf = set.inf;
	return true;
}

inline bool any_string(const datalog_expression_root& src) {
	return src.root.type == DATALOG_ANY || src.root.type == DATALOG_NON_EMPTY
		|| (src.root.type == DATALOG_STRING && src.root.str.length == 1 && src.root.str[0] == DATALOG_LABEL_WILDCARD);
}

/* NOTE: this function assumes src is not DATALOG_ANY or DATALOG_NON_EMPTY or a wildcard string */
inline bool get_string(const datalog_expression_root& src, sequence& value) {
	if (src.root.type != DATALOG_STRING)
		return false;
	return init(value, src.root.str);
}

inline bool set_string(datalog_expression_root& exp, const datalog_expression_root& set, const sequence& value) {
	if (set.root.type == DATALOG_STRING) {
		bool is_wildcard = (set.root.str.length == 1 && set.root.str[0] == DATALOG_LABEL_WILDCARD);
		if (!is_wildcard && set.root.str != value) return false;
	} else if (set.root.type != DATALOG_ANY && set.root.type != DATALOG_NON_EMPTY) {
		return false;
	}
	exp.root.type = DATALOG_STRING;
	exp.root.reference_count = 1;
	exp.concord = set.concord;
	exp.index = set.index;
	exp.inf = set.inf;
	return init(exp.root.str, value);
}

inline void get_selected(
		const datalog_expression_root::function& f,
		int& num_conjuncts, bool* args,
		tuple_position& position, bool& function)
{
	function = false;
	switch (f.type) {
	case datalog_expression_root::FUNCTION_IDENTITY:
	case datalog_expression_root::FUNCTION_IDENTITY_COORD:
	case datalog_expression_root::FUNCTION_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SINGULAR:
	case datalog_expression_root::FUNCTION_PLURAL:
	case datalog_expression_root::FUNCTION_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_CONCORD_NON_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL:
	case datalog_expression_root::FUNCTION_CONCORD_NON_PLURAL_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_CONCORD_ALL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_CONCORD_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_KEEP_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_PLURAL:
	case datalog_expression_root::FUNCTION_KEEP_UNCOUNTABLE:
	case datalog_expression_root::FUNCTION_INFINITIVE:
	case datalog_expression_root::FUNCTION_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PRESENT_PARTICIPLE:
	case datalog_expression_root::FUNCTION_KEEP_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_FLIP_PREDICATE_KEEP_PAST_PARTICIPLE:
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2:
	case datalog_expression_root::FUNCTION_HEAD_ARG1_SELECT_ARG2_ONLY:
	case datalog_expression_root::FUNCTION_EMPTY_ARG2:
	case datalog_expression_root::FUNCTION_EMPTY_ARGS:
	case datalog_expression_root::FUNCTION_ARG2_ZERO_ARITY:
	case datalog_expression_root::FUNCTION_LOC:
	case datalog_expression_root::FUNCTION_TWO_PREDICATES:
		num_conjuncts = INT_MAX; break;
	case datalog_expression_root::FUNCTION_SELECT_ARG1:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG1_ONLY_DELETE_FEATURES:
		num_conjuncts = 0; args[0] = true; break;
	case datalog_expression_root::FUNCTION_DELETE_ARG1:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_PLURAL:
	case datalog_expression_root::FUNCTION_DELETE_ARG1_FEATURES:
		num_conjuncts = 1; position = POSITION_LEFT;
		for (unsigned int i = 1; i < datalog_predicate::ARG_COUNT; i++)
			args[i] = true;
		break;
	case datalog_expression_root::FUNCTION_SELECT_ARG2:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG2_ONLY_DELETE_FEATURES:
		num_conjuncts = 0; args[1] = true; break;
	case datalog_expression_root::FUNCTION_DELETE_ARG2:
	case datalog_expression_root::FUNCTION_DELETE_ARG2_FEATURES:
		num_conjuncts = 1; position = POSITION_LEFT;
		for (unsigned int i = 0; i < datalog_predicate::ARG_COUNT; i++)
			if (i != 1) args[i] = true;
		break;
	case datalog_expression_root::FUNCTION_SELECT_ARG3:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY:
	case datalog_expression_root::FUNCTION_SELECT_ARG3_ONLY_DELETE_FEATURES:
		num_conjuncts = 0; args[2] = true; break;
	case datalog_expression_root::FUNCTION_DELETE_ARG3:
		num_conjuncts = 1; position = POSITION_LEFT;
		for (unsigned int i = 0; i < datalog_predicate::ARG_COUNT; i++)
			if (i != 2) args[i] = true;
		break;
	case datalog_expression_root::FUNCTION_DELETE_ARGS:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_ARGS_KEEP_PLURAL:
		num_conjuncts = 1; position = POSITION_LEFT; break;

	case datalog_expression_root::FUNCTION_NULL:
	case datalog_expression_root::FUNCTION_KEEP_FEATURES:
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE:
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY:
	case datalog_expression_root::FUNCTION_EMPTY_TUPLE_ONLY_KEEP_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_KEEP_NULL:
		num_conjuncts = 0; break;

	case datalog_expression_root::FUNCTION_SELECT_LEFT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_HEAD_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_ANSWER_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FUNCTION_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_NOT_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_DELETE_FEATURES:
		num_conjuncts = 1; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT_KEEP_FUNCTION_DISJOINT:
		num_conjuncts = 1; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT2:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DISJOINT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FUNCTION_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_DELETE_FEATURES:
		num_conjuncts = 2; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_LEFT2_KEEP_FUNCTION_DISJOINT:
		num_conjuncts = 2; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_ANSWER_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_DELETE_FUNCTION_ANSWER:
		num_conjuncts = 3; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT3_KEEP_FUNCTION:
		num_conjuncts = 3; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT4_DISJOINT:
		num_conjuncts = 4; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DISJOINT:
	case datalog_expression_root::FUNCTION_SELECT_LEFT5_DELETE_ANSWER_DISJOINT:
		num_conjuncts = 5; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT6_DISJOINT:
		num_conjuncts = 6; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_LEFT7_DISJOINT:
		num_conjuncts = 7; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT_DELETE_FUNCTION:
		num_conjuncts = 1; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_SINGULAR:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_SELECT_RIGHT2_DISJOINT:
		num_conjuncts = 2; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT3_DISJOINT:
		num_conjuncts = 3; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT4_DISJOINT:
		num_conjuncts = 4; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT5_DISJOINT:
		num_conjuncts = 5; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT6_DISJOINT:
		num_conjuncts = 6; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_RIGHT7_DISJOINT:
		num_conjuncts = 7; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_FEATURES:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD:
	case datalog_expression_root::FUNCTION_SELECT_FUNCTION_DELETE_HEAD_FEATURES:
		num_conjuncts = 0; function = true; break;

	case datalog_expression_root::FUNCTION_DELETE_LEFT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COORD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FUNCTION_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_NOT_HEAD_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES:
		num_conjuncts = -1; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_FEATURES_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_ANSWER_HEAD_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_KEEP_NOT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT_HEAD_FEATURES_KEEP_NOT:
		num_conjuncts = -1; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT2:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_COORD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_DISJOINT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_HEAD_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_FUNCTION_ANSWER:
		num_conjuncts = -2; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT2_ANSWER_HEAD_KEEP_FUNCTION:
		num_conjuncts = -2; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_DISJOINT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_FUNCTION_ANSWER:
		num_conjuncts = -3; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_KEEP_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_LEFT3_ANSWER_HEAD_KEEP_FUNCTION:
		num_conjuncts = -3; position = POSITION_LEFT; function = true; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT4_DISJOINT:
		num_conjuncts = -4; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_DISJOINT:
	case datalog_expression_root::FUNCTION_DELETE_LEFT5_ANSWER_DISJOINT:
		num_conjuncts = -5; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT6_DISJOINT:
		num_conjuncts = -6; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_LEFT7_DISJOINT:
		num_conjuncts = -7; position = POSITION_LEFT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT:
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD:
		num_conjuncts = -1; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT_HEAD_KEEP_FUNCTION:
		num_conjuncts = -1; position = POSITION_RIGHT; function = true; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2:
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_RIGHT2_DISJOINT:
		num_conjuncts = -2; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT3_DISJOINT:
		num_conjuncts = -3; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT4_DISJOINT:
		num_conjuncts = -4; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT5_DISJOINT:
		num_conjuncts = -5; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT6_DISJOINT:
		num_conjuncts = -6; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_RIGHT7_DISJOINT:
		num_conjuncts = -7; position = POSITION_RIGHT; break;
	case datalog_expression_root::FUNCTION_DELETE_COUNT:
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_COUNT_HEAD_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_DELETE_COUNT_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_NOT:
	case datalog_expression_root::FUNCTION_DELETE_NOT_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_NOT_INFINITIVE:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_SINGULAR:
	case datalog_expression_root::FUNCTION_DELETE_NOT_CONCORD_PLURAL:
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION:
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_FEATURES:
	case datalog_expression_root::FUNCTION_DELETE_FUNCTION_HEAD:
	case datalog_expression_root::FUNCTION_DELETE_ANSWER:
	case datalog_expression_root::FUNCTION_DELETE_ANSWER_HAS_LOC:
		num_conjuncts = 0; break;
	case datalog_expression_root::FUNCTION_EMPTY:
		fprintf(stderr, "get_selected ERROR: Invalid semantic transformation function types.\n");
		exit(EXIT_FAILURE);
	}
}

enum class separability {
	SEPARABLE,
	UNSEPARABLE,
	ALL_UNSEPARABLE
};

inline separability is_separable(
		const transformation<datalog_expression_root>& transformation,
		int& num_conjuncts, tuple_position& position,
		bool (&args)[datalog_predicate::ARG_COUNT], bool& function)
{
	if (transformation.function_count != 1)
		return separability::ALL_UNSEPARABLE;

	int new_num_conjuncts = 0;
	tuple_position new_position = POSITION_EXACT;
	bool new_function = false;
	bool new_args[datalog_predicate::ARG_COUNT];
	for (unsigned int k = 0; k < array_length(new_args); k++)
		new_args[k] = false;
	get_selected(transformation.functions[0], new_num_conjuncts, new_args, new_position, new_function);

	bool separable_i = true;
	if (function && new_function) {
		separable_i = false;
	} else if (num_conjuncts + new_num_conjuncts > 0) {
		separable_i = false;
	}

	function |= new_function;
	if (position == POSITION_EXACT) {
		position = new_position;
	} else if (new_position != POSITION_EXACT && position != new_position) {
		return separability::ALL_UNSEPARABLE;
	}

	if (new_num_conjuncts == INT_MAX) {
		return separability::ALL_UNSEPARABLE;
	} else if (num_conjuncts == 0) {
		num_conjuncts = new_num_conjuncts;
	} else if (num_conjuncts > 0) {
		if (new_num_conjuncts >= 0) {
			num_conjuncts = max(num_conjuncts, new_num_conjuncts);
		} else {
			return separability::ALL_UNSEPARABLE;
		}
	} else {
		if (new_num_conjuncts <= 0) {
			num_conjuncts = min(num_conjuncts, new_num_conjuncts);
		} else {
			return separability::ALL_UNSEPARABLE;
		}
	}

	for (unsigned int k = 0; k < array_length(args); k++) {
		if (args[k] && new_args[k]) return separability::ALL_UNSEPARABLE;
		args[k] |= new_args[k];
	}

	return separable_i ? separability::SEPARABLE : separability::UNSEPARABLE;
}

void is_separable(
		const transformation<datalog_expression_root>* functions,
		unsigned int rule_length, bool* separable)
{
	int num_conjuncts = 0; bool function = false;
	tuple_position position = POSITION_EXACT;
	bool args[datalog_predicate::ARG_COUNT];
	for (unsigned int k = 0; k < array_length(args); k++)
		args[k] = false;
	for (unsigned int i = 0; i < rule_length; i++) {
		separability result = is_separable(functions[i], num_conjuncts, position, args, function);
		switch (result) {
		case separability::SEPARABLE:
			separable[i] = true; break;
		case separability::UNSEPARABLE:
			separable[i] = false; break;
		case separability::ALL_UNSEPARABLE:
			for (unsigned int j = i; j < rule_length; j++)
				separable[j] = false;
			return;
		}
	}
}


/**
 * Functions for type-checking Datalog expressions.
 */

constexpr unsigned int DATALOG_TYPE_ENTITY = 1;
constexpr unsigned int DATALOG_TYPE_BOOLEAN = 2;
constexpr unsigned int DATALOG_TYPE_INTEGER = 3;
constexpr unsigned int DATALOG_TYPE_STRING = 4;
constexpr unsigned int DATALOG_TYPE_COUNTABLE = 5;
constexpr unsigned int DATALOG_TYPE_ANY = UINT_MAX;

inline bool parse_datalog_type(unsigned int& type, const string& name) {
	if (name == "e") {
		type = DATALOG_TYPE_ENTITY; return true;
	} else if (name == "t") {
		type = DATALOG_TYPE_BOOLEAN; return true;
	} else if (name == "i") {
		type = DATALOG_TYPE_INTEGER; return true;
	} else if (name == "str") {
		type = DATALOG_TYPE_STRING; return true;
	} else if (name == "countable") {
		type = DATALOG_TYPE_COUNTABLE; return true;
	} else {
		fprintf(stderr, "parse_datalog_type ERROR: Unrecognized typename.\n");
		return false;
	}
}

template<typename Stream>
inline bool print_datalog_type(unsigned int type, Stream& stream) {
	switch (type) {
	case DATALOG_TYPE_ENTITY:
		return print('e', stream);
	case DATALOG_TYPE_BOOLEAN:
		return print('t', stream);
	case DATALOG_TYPE_INTEGER:
		return print('i', stream);
	case DATALOG_TYPE_STRING:
		return print("str", stream);
	case DATALOG_TYPE_COUNTABLE:
		return print("countable", stream);
	case DATALOG_TYPE_ANY:
		return print('*', stream);
	}
	fprintf(stderr, "print_datalog_type ERROR: Unrecognized datalog type.\n");
	return false;
}

struct predicate_signature {
	unsigned int return_type;
	unsigned int arg_types[datalog_predicate::ARG_COUNT];
};

struct datalog_ontology {
	array<array<predicate_signature>> types;
	array<array<unsigned int>> supertypes;
	hash_map<string, unsigned int> type_names;

	datalog_ontology() : types(256), supertypes(256), type_names(32) {
		if (!type_names.put("e", DATALOG_TYPE_ENTITY)
		 || !type_names.put("t", DATALOG_TYPE_BOOLEAN)
		 || !type_names.put("i", DATALOG_TYPE_INTEGER)
		 || !type_names.put("str", DATALOG_TYPE_STRING)
		 || !type_names.put("countable", DATALOG_TYPE_COUNTABLE))
			exit(EXIT_FAILURE);
	}

	~datalog_ontology() { free(); }

	bool add_supertype(unsigned int type, unsigned int supertype) {
		if (type <= supertype) {
			fprintf(stderr, "datalog_ontology.add_supertype ERROR:"
					" Supertypes must appear before each subtype.\n");
			return false;
		} else if (!ensure_capacity(type))
			return false;
		return supertypes[type].add(supertype);
	}

	inline bool add_signature(unsigned int predicate, const predicate_signature& signature) {
		return ensure_capacity(predicate) && types[predicate].add(signature);
	}

	inline bool add_signature_if_empty(unsigned int predicate, const predicate_signature& signature) {
		return types[predicate].length != 0
			|| types[predicate].add(signature);
	}

	bool initialize() {
		if (!add_supertype(DATALOG_TYPE_INTEGER, DATALOG_TYPE_ENTITY)
		 || !add_supertype(DATALOG_TYPE_STRING, DATALOG_TYPE_ENTITY)
		 || !add_supertype(DATALOG_TYPE_COUNTABLE, DATALOG_TYPE_ENTITY))
			return false;

		/* if needed, add default types for the higher-order functions */
		if (!ensure_capacity(NUM_PREDICATES)
		 || !add_signature_if_empty(PREDICATE_ANSWER,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_NOT,		{DATALOG_TYPE_BOOLEAN, {0, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_COUNT,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_COUNTABLE, DATALOG_TYPE_INTEGER, 0}})
		 || !add_signature_if_empty(PREDICATE_SUM,		{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_INTEGER, DATALOG_TYPE_INTEGER, 0}})
		 || !add_signature_if_empty(PREDICATE_HIGHEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_LOWEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_LONGEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_SHORTEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_LARGEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_SMALLEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, 0, 0}})
		 || !add_signature(			PREDICATE_LARGEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_INTEGER, 0, 0}})
		 || !add_signature(			PREDICATE_SMALLEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_INTEGER, 0, 0}})
		 || !add_signature_if_empty(PREDICATE_MOST,		{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, DATALOG_TYPE_COUNTABLE, 0}})
		 || !add_signature_if_empty(PREDICATE_FEWEST,	{DATALOG_TYPE_BOOLEAN, {DATALOG_TYPE_ENTITY, DATALOG_TYPE_COUNTABLE, 0}}))
			return false;

		/* compute list of supertypes for every type */
		for (unsigned int i = 0; i < supertypes.length; i++) {
			unsigned int old_length = supertypes[i].length;
			for (unsigned int j = 0; j < old_length; j++) {
				unsigned int parent = supertypes[i][j];
				if (!supertypes[i].append(supertypes[parent].data, supertypes[parent].length))
					return false;
			}
			if (!supertypes[i].add(i)) return false;

			if (supertypes[i].length > 1) {
				insertion_sort(supertypes[i]);
				unique(supertypes[i]);
			}
		}
		return true;
	}

private:
	bool ensure_capacity(unsigned int new_type) {
		if (!supertypes.ensure_capacity(new_type + 1)
		 || !types.ensure_capacity(new_type + 1)) return false;
		for (unsigned int i = supertypes.length; i < supertypes.capacity; i++) {
			if (!array_init(supertypes[i], 4)) return false;
			supertypes.length++;
		}
		for (unsigned int i = types.length; i < types.capacity; i++) {
			if (!array_init(types[i], 2)) return false;
			types.length++;
		}
		return true;
	}

	void free() {
		for (array<unsigned int>& a : supertypes)
			core::free(a);
		for (array<predicate_signature>& a : types)
			core::free(a);
		for (auto entry : type_names)
			core::free(entry.key);
	}
};

/* forward declarations */

template<bool Complete>
bool build_context(
		const datalog_tuple& tuple,
		const datalog_ontology& T,
		array<unsigned int>& context);

template<bool Complete>
bool type_check(
		const datalog_tuple& tuple,
		const datalog_ontology& T,
		const array<unsigned int>& context);


/* returns true if 'second_type' is a supertype of 'first_type' */
inline bool is_supertype(const datalog_ontology& T,
		unsigned int first_type, unsigned int second_type)
{
	if (second_type == DATALOG_TYPE_ANY) return true;
	if (first_type == DATALOG_TYPE_ANY) return second_type == DATALOG_TYPE_ANY;
	unsigned int index = linear_search(T.supertypes[first_type].data,
			second_type, 0, T.supertypes[first_type].length);
	return (index < T.supertypes[first_type].length
		 && T.supertypes[first_type][index] == second_type);
}

bool type_check(
		const datalog_literal& constant,
		const datalog_ontology& T,
		unsigned int expected_type)
{
	if (constant.label == DATALOG_LABEL_WILDCARD || constant.label >= T.types.length || T.types[constant.label].length == 0)
		return true;
	bool match = false;
	for (unsigned int j = 0; j < T.types[constant.label].length; j++) {
		const predicate_signature& signature = T.types[constant.label][j];

		bool is_constant_type = true;
		for (unsigned int k = 0; k < array_length(signature.arg_types); k++) {
			if (signature.arg_types[k] != 0) {
				is_constant_type = false;
				break;
			}
		}
		if (!is_constant_type) continue;

		if (is_supertype(T, signature.return_type, expected_type)
		 || is_supertype(T, expected_type, signature.return_type)) {
			match = true;
			break;
		}
	}
	return match;
}

inline void build_context_any(
		unsigned int variable,
		const datalog_ontology& T,
		array<unsigned int>& context)
{
	if (!ensure_variable_map_capacity(context, variable)) {
		exit(EXIT_FAILURE);
	} else if (context[variable] == 0)
		context[variable] = DATALOG_TYPE_ANY;
}

inline bool build_context(
		unsigned int variable,
		unsigned int type,
		const datalog_ontology& T,
		array<unsigned int>& context)
{
	if (!ensure_variable_map_capacity(context, variable)) {
		exit(EXIT_FAILURE);
	} else if (context[variable] == 0) {
		context[variable] = type;
	} else if (is_supertype(T, type, context[variable])) {
		context[variable] = type;
	} else if (!is_supertype(T, context[variable], type)) {
		return false;
	}
	return true;
}

template<bool Complete, bool Flippable>
bool build_context(
		const datalog_predicate& pred,
		const datalog_ontology& T,
		array<unsigned int>& context)
{
	if (pred.function == PREDICATE_CONST) {
		if (pred.args[0] == NULL)
			return true; /* 'delete_args' allows this to be empty during parsing */
		else if (pred.args[1] == NULL)
			return false;
		else if (!Complete && pred.args[0]->type == DATALOG_ANY)
			return true;
		else if (pred.args[0]->type != DATALOG_VARIABLE)
			return false;

		unsigned int arg_type;
		unsigned int variable = pred.args[0]->variable;
		if (pred.args[1]->type == DATALOG_PREDICATE) {
			unsigned int predicate = pred.args[1]->pred.function;
			if (predicate == DATALOG_LABEL_WILDCARD || predicate >= T.types.length || T.types[predicate].length != 1) {
				build_context_any(variable, T, context);
				return true; /* defer to type checking */
			}
			arg_type = T.types[predicate][0].return_type;
		} else if (pred.args[1]->type == DATALOG_CONSTANT) {
			unsigned int predicate = pred.args[1]->constant.label;
			if (predicate == DATALOG_LABEL_WILDCARD || predicate >= T.types.length || T.types[predicate].length != 1) {
				build_context_any(variable, T, context);
				return true; /* defer to type checking */
			}
			arg_type = T.types[predicate][0].return_type;
		} else if (pred.args[1]->type == DATALOG_INTEGER) {
			arg_type = DATALOG_TYPE_INTEGER;
		} else if (pred.args[1]->type == DATALOG_STRING) {
			arg_type = DATALOG_TYPE_STRING;
		} else {
			return false;
		}
		return build_context(variable, arg_type, T, context);
	} else if (pred.function == DATALOG_LABEL_WILDCARD
			|| pred.function >= T.types.length || T.types[pred.function].length != 1) {
		for (unsigned int i = 0; i < array_length(pred.args); i++) {
			if (pred.args[i] == NULL || pred.args[i]->type != DATALOG_VARIABLE) continue;
			build_context_any(pred.args[i]->variable, T, context);
		}
		return true;
	}

	const predicate_signature& signature = T.types[pred.function][0];
	if (Complete && signature.return_type != DATALOG_TYPE_BOOLEAN)
		return false;
	for (unsigned int i = 0; i < array_length(pred.args); i++) {
		if (pred.args[i] == NULL || pred.args[i]->type == DATALOG_ANY || pred.args[i]->type == DATALOG_NON_EMPTY) {
			/* predicate arguments can be empty during parsing */
			continue;
		} else if (pred.args[i]->type == DATALOG_VARIABLE) {
			if (Flippable && i < 2)
				build_context_any(pred.args[i]->variable, T, context);
			else if (!build_context(pred.args[i]->variable, signature.arg_types[i], T, context))
				return false;
		} else if (pred.args[i]->type == DATALOG_INTEGER) {
			if (!is_supertype(T, DATALOG_TYPE_INTEGER, signature.arg_types[i])
			 && !is_supertype(T, signature.arg_types[i], DATALOG_TYPE_INTEGER)) {
				return false;
			}
		} else if (pred.args[i]->type == DATALOG_STRING) {
			if (!is_supertype(T, DATALOG_TYPE_STRING, signature.arg_types[i])
			 && !is_supertype(T, signature.arg_types[i], DATALOG_TYPE_STRING)) {
				return false;
			}
		} else if (pred.args[i]->type == DATALOG_CONSTANT) {
			if (!type_check(pred.args[i]->constant, T, signature.arg_types[i]))
				return false;
		} else {
			return false;
		}
	}

	return true;
}

template<bool Complete>
bool build_context(
		const datalog_function& func,
		const datalog_ontology& T,
		array<unsigned int>& context)
{
	if (func.function != DATALOG_LABEL_WILDCARD) {
		/* the ontology has information about this higher-order function */
		if (T.types[func.function].length > 1) {
			for (unsigned int i = 0; i < array_length(func.vars); i++) {
				if (func.vars[i] == 0) continue;
				build_context_any(func.vars[i], T, context);
			}
		} else {
			const predicate_signature& signature = T.types[func.function][0];
			for (unsigned int i = 0; i < array_length(func.vars); i++) {
				if (func.vars[i] != 0
				 && !build_context(func.vars[i], signature.arg_types[i], T, context))
					return false;
			}
		}
	}

	if (func.arg->type == DATALOG_PREDICATE) {
		return build_context<Complete, false>(func.arg->pred, T, context);
	} else if (func.arg->type == DATALOG_TUPLE) {
		return build_context<Complete, false>(func.arg->tuple, T, context);
	} else if (func.arg->type == DATALOG_FUNCTION) {
		return build_context<Complete>(func.arg->func, T, context);
	} else if (func.arg->type == DATALOG_ANY || func.arg->type == DATALOG_NON_EMPTY || func.arg->type == DATALOG_EMPTY) {
		return true;
	} else {
		fprintf(stderr, "build_context ERROR: Found a higher-order function with an "
				"argument that is not a predicate, tuple, or another higher-order function.\n");
		return false;
	}
}

template<bool Complete, bool Flippable>
bool build_context(
		const datalog_tuple& tuple,
		const datalog_ontology& T,
		array<unsigned int>& context)
{
	for (unsigned int i = 0; i < tuple.elements.length; i++) {
		const datalog_expression* element = tuple.elements[i];
		switch (element->type) {
		case DATALOG_PREDICATE:
			if ((i == 0 && !build_context<Complete, Flippable>(element->pred, T, context))
			 || (i == 1 && !build_context<Complete, false>(element->pred, T, context))) return false;
			break;
		case DATALOG_FUNCTION:
			if (!build_context<Complete>(element->func, T, context)) return false;
			break;
		case DATALOG_ANY:
		case DATALOG_NON_EMPTY:
			continue;
		default:
			fprintf(stderr, "build_context ERROR: Unrecognized tuple element type.\n");
			return false;
		}
	}
	return true;
}

template<bool Complete, bool Flippable>
bool type_check(
		const datalog_predicate& pred,
		const datalog_ontology& T,
		const array<unsigned int>& context,
		unsigned int expected_type)
{
	if (pred.function == PREDICATE_CONST) {
		/* 'select_args' allows this to be empty during parsing */
		if (pred.args[0] == NULL) return Complete ? false : true;
		if (!is_supertype(T, expected_type, DATALOG_TYPE_BOOLEAN)
		 && !is_supertype(T, DATALOG_TYPE_BOOLEAN, expected_type))
			return false;

		unsigned int variable = pred.args[0]->variable;
		if (pred.args[1] == NULL) {
			return true;
		} else if (pred.args[1]->type == DATALOG_PREDICATE) {
			unsigned int predicate = pred.args[1]->pred.function;
			if (predicate == DATALOG_LABEL_WILDCARD || predicate >= T.types.length || T.types[predicate].length <= 1)
				return true;
			return type_check<Complete, false>(pred.args[1]->pred, T, context, context[variable]);
		} else if (pred.args[1]->type == DATALOG_CONSTANT) {
			unsigned int predicate = pred.args[1]->constant.label;
			if (predicate == DATALOG_LABEL_WILDCARD || predicate >= T.types.length || T.types[predicate].length <= 1)
				return true;
			return type_check(pred.args[1]->constant, T, context[variable]);
		} else {
			return true;
		}
	} else if (pred.function == DATALOG_LABEL_WILDCARD
			|| pred.function >= T.types.length || T.types[pred.function].length == 0) {
		return true;
	}

	for (unsigned int i = 0; i < T.types[pred.function].length; i++) {
		const predicate_signature& signature = T.types[pred.function][i];

		if (!is_supertype(T, expected_type, signature.return_type)
		 && !is_supertype(T, signature.return_type, expected_type))
			continue;

		bool match = true;
		for (unsigned int i = 0; i < array_length(pred.args); i++) {
			unsigned int arg_type;
			if (pred.args[i] == NULL) {
				if (!Complete) {
					continue;
				} else if (signature.arg_types[i] == 0) {
					continue;
				} else {
					match = false;
					break;
				}
			} else if (pred.args[i]->type == DATALOG_ANY || pred.args[i]->type == DATALOG_NON_EMPTY) {
				if (Complete) {
					match = false;
					break;
				} else continue;
			} else if (pred.args[i]->type == DATALOG_CONSTANT) {
				if (type_check(pred.args[i]->constant, T, signature.arg_types[i])) {
					continue;
				} else {
					match = false;
					break;
				}
			} else if (pred.args[i]->type == DATALOG_INTEGER) {
				arg_type = DATALOG_TYPE_INTEGER;
			} else if (pred.args[i]->type == DATALOG_STRING) {
				arg_type = DATALOG_TYPE_STRING;
			} else if (pred.args[i]->type == DATALOG_VARIABLE) {
				if (pred.args[i]->variable >= context.capacity || context[pred.args[i]->variable] == 0) {
					/* this variable is not in the context */
					return false;
				}
				arg_type = context[pred.args[i]->variable];
			} else {
				return false;
			}

			if (!is_supertype(T, arg_type, signature.arg_types[i])
			 && !is_supertype(T, signature.arg_types[i], arg_type)) {
				if (!Flippable || i >= 2 || pred.args[i]->type != DATALOG_VARIABLE) {
					match = false;
					break;
				} else if (!is_supertype(T, arg_type, signature.arg_types[1 - i])
						 && !is_supertype(T, signature.arg_types[1 - i], arg_type)) {
					match = false;
					break;
				}
			}
		}
		if (match) return true;
	}

	/* none of the type signatures for this predicate match */
	return false;
}

template<bool Complete>
bool type_check(
		const datalog_function& func,
		const datalog_ontology& T,
		const array<unsigned int>& context)
{
	if (func.function != DATALOG_LABEL_WILDCARD && T.types[func.function].length > 1) {
		/* we didn't type check during context construction, so we do it now */
		bool type_correct = false;
		for (unsigned int i = 0; i < T.types[func.function].length; i++) {
			const predicate_signature& signature = T.types[func.function][i];
			if (signature.return_type != DATALOG_TYPE_BOOLEAN) continue;

			bool match = true;
			for (unsigned int i = 0; i < array_length(func.vars); i++) {
				if (func.vars[i] == 0) {
					if (signature.arg_types[i] == 0) {
						continue;
					} else {
						match = false;
						break;
					}
				}

				if (func.vars[i] >= context.capacity || context[func.vars[i]] == 0) {
					/* this variable is not in the context */
					return false;
				} else if (!is_supertype(T, context[func.vars[i]], signature.arg_types[i])
						&& !is_supertype(T, signature.arg_types[i], context[func.vars[i]])) {
					match = false;
					break;
				}
			}

			if (match) {
				type_correct = true;
				break;
			}
		}

		if (!type_correct) return false;
	}

	if (func.arg->type == DATALOG_PREDICATE) {
		return type_check<Complete, false>(func.arg->pred, T, context, DATALOG_TYPE_BOOLEAN);
	} else if (func.arg->type == DATALOG_TUPLE) {
		return type_check<Complete, false>(func.arg->tuple, T, context);
	} else if (func.arg->type == DATALOG_FUNCTION) {
		return type_check<Complete>(func.arg->func, T, context);
	} else if (func.arg->type == DATALOG_ANY || func.arg->type == DATALOG_NON_EMPTY || func.arg->type == DATALOG_EMPTY) {
		return true;
	} else {
		fprintf(stderr, "type_check ERROR: Found a higher-order function with an "
				"argument that is not a predicate, tuple, or another higher-order function.\n");
		return false;
	}
}

template<bool Complete, bool Flippable>
bool type_check(
		const datalog_tuple& tuple,
		const datalog_ontology& T,
		const array<unsigned int>& context)
{
	for (unsigned int i = 0; i < tuple.elements.length; i++) {
		const datalog_expression* element = tuple.elements[i];
		switch (element->type) {
		case DATALOG_PREDICATE:
			if ((i == 0 && !type_check<Complete, Flippable>(element->pred, T, context, DATALOG_TYPE_BOOLEAN))
			 || (i == 1 && !type_check<Complete, false>(element->pred, T, context, DATALOG_TYPE_BOOLEAN))) return false;
			break;
		case DATALOG_FUNCTION:
			if (!type_check<Complete>(element->func, T, context)) return false;
			break;
		case DATALOG_ANY:
		case DATALOG_NON_EMPTY:
			continue;
		default:
			fprintf(stderr, "type_check ERROR: Unrecognized tuple element type.\n");
			return false;
		}
	}
	return true;
}

template<bool Complete>
bool type_check(const datalog_ontology& T, const datalog_expression_root& e) {
	array<unsigned int> context = array<unsigned int>(8);
	memset(context.data, 0, sizeof(unsigned int) * context.capacity);
	if (e.root.type == DATALOG_ANY || e.root.type == DATALOG_NON_EMPTY
	 || e.root.type == DATALOG_EMPTY || e.root.type == DATALOG_CONSTANT
	 || e.root.type == DATALOG_INTEGER || e.root.type == DATALOG_STRING) {
		return true;
	} else if (e.root.type == DATALOG_TUPLE) {
		return build_context<Complete, !Complete>(e.root.tuple, T, context)
			&& type_check<Complete, !Complete>(e.root.tuple, T, context);
	} else if (e.root.type == DATALOG_FUNCTION) {
		return build_context<Complete>(e.root.func, T, context)
			&& type_check<Complete>(e.root.func, T, context);
	} else if (e.root.type == DATALOG_PREDICATE) {
		return build_context<Complete, !Complete>(e.root.pred, T, context)
			&& type_check<Complete, !Complete>(e.root.pred, T, context, Complete ? DATALOG_TYPE_BOOLEAN : DATALOG_TYPE_ANY);
	} else {
		return false;
	}
}


/**
 * Functionality for reading an ontology from a stream.
 */

enum ontology_token_type {
	ONTOLOGY_TOKEN_IDENTIFIER,
	ONTOLOGY_TOKEN_COLON,
	ONTOLOGY_TOKEN_SUBTYPE,
	ONTOLOGY_TOKEN_ARROW
};

typedef lexical_token<ontology_token_type> ontology_token;

template<typename Stream>
inline bool print(ontology_token_type type, Stream& stream) {
	switch (type) {
	case ONTOLOGY_TOKEN_IDENTIFIER:
		return print("IDENTIFIER", stream);
	case ONTOLOGY_TOKEN_COLON:
		return print(':', stream);
	case ONTOLOGY_TOKEN_SUBTYPE:
		return print("<:", stream);
	case ONTOLOGY_TOKEN_ARROW:
		return print("->", stream);
	}
	fprintf(stderr, "print ERROR: Unknown ontology_token_type.\n");
	return false;
}

enum ontology_lexer_state {
	ONTOLOGY_LEXER_START,
	ONTOLOGY_LEXER_IDENTIFIER,
	ONTOLOGY_LEXER_QUOTE
};

bool ontology_lex(array<ontology_token>& tokens, FILE* input) {
	position start = position(1, 1);
	position current = position(1, 1);
	ontology_lexer_state state = ONTOLOGY_LEXER_START;
	array<char> token = array<char>(1024);

	int next = fgetc(input);
	bool new_line = false;
	while (next != -1) {
		switch (state) {
		case ONTOLOGY_LEXER_QUOTE:
			if (next == '\'') {
				if (!emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER)) return false;
				state = ONTOLOGY_LEXER_START;
				token.clear();
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case ONTOLOGY_LEXER_IDENTIFIER:
			if (next == ':') {
				if (!emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER)
				 || !emit_token(tokens, start, start + 1, ONTOLOGY_TOKEN_COLON))
					return false;
				state = ONTOLOGY_LEXER_START;
				token.clear();
			} else if (next == '-') {
				next = fgetc(input);
				if (next != '>') {
					read_error("Expected '>'", current);
					return false;
				} else if (!emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER)
						|| !emit_token(tokens, current, current + 2, ONTOLOGY_TOKEN_ARROW))
					return false;
				current.column++;
				state = ONTOLOGY_LEXER_START;
				token.clear();
			} else if (next == '<') {
				next = fgetc(input);
				if (next != ':') {
					read_error("Expected ':'", current);
					return false;
				} else if (!emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER)
						|| !emit_token(tokens, current, current + 2, ONTOLOGY_TOKEN_SUBTYPE))
					return false;
				current.column++;
				state = ONTOLOGY_LEXER_START;
				token.clear();
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				if (!emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER))
					return false;
				state = ONTOLOGY_LEXER_START;
				token.clear();
				new_line = (next == '\n');
			} else if (next == '\'') {
				read_error("Unexpected quote after identifier", current);
				return false;
			} else {
				if (!token.add(next)) return false;
			}
			break;

		case ONTOLOGY_LEXER_START:
			if (next == '\'') {
				state = ONTOLOGY_LEXER_QUOTE;
				start = current;
			} else if (next == ':') {
				if (!emit_token(tokens, current, current + 1, ONTOLOGY_TOKEN_COLON))
					return false;
			} else if (next == '-') {
				next = fgetc(input);
				if (next != '>') {
					read_error("Expected '>'", current);
					return false;
				} if (!emit_token(tokens, current, current + 2, ONTOLOGY_TOKEN_ARROW))
					return false;
				current.column++;
			} else if (next == '<') {
				next = fgetc(input);
				if (next != ':') {
					read_error("Expected ':'", current);
					return false;
				} if (!emit_token(tokens, current, current + 2, ONTOLOGY_TOKEN_SUBTYPE))
					return false;
				current.column++;
			} else if (next == ' ' || next == '\t' || next == '\n' || next == '\r') {
				new_line = (next == '\n');
			} else {
				if (!token.add(next)) return false;
				state = ONTOLOGY_LEXER_IDENTIFIER;
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

	if (state == ONTOLOGY_LEXER_QUOTE) {
		read_error("Expected closing quote", current);
		return false;
	} else if (state == ONTOLOGY_LEXER_IDENTIFIER) {
		return emit_token(tokens, token, start, current, ONTOLOGY_TOKEN_IDENTIFIER);
	}
	return true;
}

bool ontology_interpret_statement(
		const array<ontology_token>& tokens,
		unsigned int& index, datalog_ontology& T,
		hash_map<string, unsigned int>& predicate_names)
{
	if (!expect_token(tokens, index, ONTOLOGY_TOKEN_IDENTIFIER, "identifier at beginning of type declaration"))
		return false;
	const string& identifier = tokens[index].text;
	index++;

	if (index == tokens.length) {
		fprintf(stderr, "ERROR: Unexpected end of input. Expected a colon or subtype operator.\n");
		return false;
	} else if (tokens[index].type == ONTOLOGY_TOKEN_SUBTYPE) {
		index++;

		/* this is a subtype declaration */
		unsigned int subtype;
		if (!expect_token(tokens, index, ONTOLOGY_TOKEN_IDENTIFIER, "supertype")
		 || !get_token(identifier, subtype, T.type_names))
			return false;

		bool contains;
		unsigned int supertype = T.type_names.get(tokens[index].text, contains);
		if (!contains) {
			read_error("Undefined type", tokens[index].start);
			return false;
		} else if (!T.add_supertype(subtype, supertype))
			return false;
		index++;

	} else if (tokens[index].type == ONTOLOGY_TOKEN_COLON) {
		index++;

		array<unsigned int> types = array<unsigned int>(8);
		while (true) {
			if (!expect_token(tokens, index, ONTOLOGY_TOKEN_IDENTIFIER, "type in type specification"))
				return false;

			bool contains;
			unsigned int type_id = T.type_names.get(tokens[index].text, contains);
			if (!contains) {
				read_error("Undefined type", tokens[index].start);
				return false;
			} else if (!types.add(type_id))
				return false;
			index++;

			if (index == tokens.length || tokens[index].type != ONTOLOGY_TOKEN_ARROW)
				break;
			index++;
		}

		if (types.length > datalog_predicate::ARG_COUNT + 1) {
			read_error("Too many arguments in type declaration", tokens[index - 1].end);
			return false;
		} else {
			predicate_signature signature;
			signature.return_type = types.last();
			for (unsigned int i = 0; i < types.length - 1; i++)
				signature.arg_types[i] = types[i];
			for (unsigned int i = types.length - 1; i < array_length(signature.arg_types); i++)
				signature.arg_types[i] = 0;

			unsigned int predicate;
			if (!get_token(identifier, predicate, predicate_names)
			 || !T.add_signature(predicate, signature))
				return false;
		}
	}

	return true;
}

bool ontology_interpret(
		const array<ontology_token>& tokens, datalog_ontology& T,
		hash_map<string, unsigned int>& predicate_names)
{
	unsigned int index = 0;
	while (index < tokens.length) {
		if (!ontology_interpret_statement(tokens, index, T, predicate_names))
			return false;
	}
	return true;
}


/**
 * Code to perform morphological parsing and generation.
 */

inline bool intersect_concord_index(grammatical_number& intersection,
		grammatical_number concord, grammatical_number index)
{
	if (concord == NUMBER_SINGULAR) {
		if (index != NUMBER_SINGULAR && index != NUMBER_NON_PLURAL
		 && index != NUMBER_ALL && index != NUMBER_ANY)
			return false;
		intersection = NUMBER_SINGULAR;
	} else if (concord == NUMBER_PLURAL) {
		if (index != NUMBER_PLURAL && index != NUMBER_NON_SINGULAR
		 && index != NUMBER_ALL && index != NUMBER_ANY)
			return false;
		intersection = NUMBER_PLURAL;
	} else if (concord == NUMBER_UNCOUNTABLE) {
		if (index != NUMBER_UNCOUNTABLE && index != NUMBER_SINGULAR
		 && index != NUMBER_NON_SINGULAR && index != NUMBER_NON_PLURAL
		 && index != NUMBER_ALL && index != NUMBER_ANY)
			return false;
		intersection = NUMBER_UNCOUNTABLE;
	} else if (concord == NUMBER_NON_SINGULAR) {
		if (index == NUMBER_PLURAL) {
			intersection = NUMBER_PLURAL;
		} else if (index == NUMBER_UNCOUNTABLE || index == NUMBER_NON_PLURAL || index == NUMBER_SINGULAR) {
			intersection = NUMBER_UNCOUNTABLE;
		} else if (index == NUMBER_NON_SINGULAR || index == NUMBER_ALL || index == NUMBER_ANY) {
			intersection = NUMBER_NON_SINGULAR;
		} else {
			return false;
		}
	} else if (concord == NUMBER_NON_PLURAL) {
		if (index == NUMBER_SINGULAR) {
			intersection = NUMBER_SINGULAR;
		} else if (index == NUMBER_UNCOUNTABLE || index == NUMBER_NON_SINGULAR) {
			intersection = NUMBER_UNCOUNTABLE;
		} else if (index == NUMBER_NON_PLURAL || index == NUMBER_ALL || index == NUMBER_ANY) {
			intersection = NUMBER_NON_PLURAL;
		} else {
			return false;
		}
	} else if (concord == NUMBER_ALL || concord == NUMBER_ANY) {
		if (index == NUMBER_NONE) return false;
		intersection = index;
	} else {
		/* concord is NUMBER_NONE */
		return false;
	}
	return true;
}

template<typename Morphology, typename PartOfSpeechType>
bool morphology_is_valid(
		const Morphology& morphology_parser,
		const sequence& terminal, PartOfSpeechType pos,
		const datalog_expression_root& logical_form)
{
	if (pos == POS_OTHER) {
		return true;
	} else if (pos == POS_ADJECTIVE || pos == POS_ADVERB) {
		if (logical_form.concord == NUMBER_SINGULAR || logical_form.concord == NUMBER_PLURAL)
			return false;

		/* we don't model adjective morphology, for now */
		/* adjective compounds are head-final (TODO: need a database of exceptions) */
		unsigned int head_index = terminal.length - 1;
		const fixed_array<token>& result = morphology_parser.parse(terminal[head_index]);
		for (unsigned int i = 0; i < result.length; i++) {
			if (result[i].get_part_of_speech() == pos)
				return true;
		}
		return false;
	}

	unsigned int head_index;
	if (pos == POS_NOUN) {
		/* noun compounds are head-final (TODO: need a database of exceptions) */
		head_index = terminal.length - 1;
	} else if (pos == POS_VERB) {
		/* verb compounds (phrasal verbs) are head-final (TODO: need a database of exceptions) */
		head_index = 0;
	} else {
		fprintf(stderr, "morphology_is_valid ERROR: Unrecognized part of speech.\n");
		return false;
	}

	/* check if concord and index have a non-empty intersection (treating the numbers as set-valued) */
	grammatical_number intersection = NUMBER_NONE;
	if (pos == POS_NOUN) {
		if (!intersect_concord_index(intersection, logical_form.concord, logical_form.index))
			return false;
	} else if (pos == POS_VERB) {
		/* if the verb has multiple words, ensure none are auxiliary */
		if (terminal.length > 1) {
			for (unsigned int i = 0; i + 1 < terminal.length; i++)
				if (morphology_parser.is_auxiliary_root(terminal[i])) return false;
		}
	}

	if (pos == POS_VERB) {
		inflection inf = logical_form.inf;
		if (inf == INFLECTION_NONE)
			inf = INFLECTION_OTHER_VERB;
		if (morphology_parser.inflect({terminal[head_index], NUMBER_ANY, inf}).length > 0)
			return true;
		if (has_intersection(logical_form.index, NUMBER_SINGULAR)) {
			if (morphology_parser.inflect({terminal[head_index], NUMBER_SINGULAR, inf}).length > 0)
				return true;
		} if (has_intersection(logical_form.index, NUMBER_PLURAL)) {
			if (morphology_parser.inflect({terminal[head_index], NUMBER_PLURAL, inf}).length > 0)
				return true;
		}
	} else if (pos == POS_NOUN) {
		if (has_intersection(intersection, NUMBER_SINGULAR)) {
			if (morphology_parser.inflect({terminal[head_index], NUMBER_SINGULAR, INFLECTION_NOUN}).length > 0)
				return true;
		} if (has_intersection(intersection, NUMBER_PLURAL)) {
			if (morphology_parser.inflect({terminal[head_index], NUMBER_PLURAL, INFLECTION_NOUN}).length > 0)
				return true;
		} if (has_intersection(intersection, NUMBER_UNCOUNTABLE)) {
			/* simply return true, since this could be a proper noun */
			return true;
		}
	}
	return false;
}

template<bool First, typename Morphology, typename PartOfSpeechType, typename EmitRootFunction>
bool morphology_parse(
		const Morphology& morphology_parser, const sequence& words, PartOfSpeechType pos,
		const datalog_expression_root& logical_form, EmitRootFunction emit_root)
{
	if (pos == POS_OTHER) {
		return emit_root(words, logical_form);
	} else if (pos == POS_ADJECTIVE || pos == POS_ADVERB) {
		if (logical_form.concord == NUMBER_SINGULAR || logical_form.concord == NUMBER_PLURAL)
			return false;

		/* we don't model adjective morphology, for now */
		/* adjective compounds are head-final (TODO: need a database of exceptions) */
		unsigned int head_index = words.length - 1;
		const fixed_array<token>& result = morphology_parser.parse(words[head_index]);
		for (unsigned int i = 0; i < result.length; i++) {
			if (result[i].get_part_of_speech() == pos)
				return emit_root(words, logical_form);
		}
		return true;
	}

	unsigned int head_index;
	if (pos == POS_NOUN) {
		/* noun compounds are head-final (TODO: need a database of exceptions) */
		head_index = words.length - 1;
	} else if (pos == POS_VERB) {
		/* verb compounds (phrasal verbs) are head-initial (TODO: need a database of exceptions) */
		head_index = 0;
	} else {
		fprintf(stderr, "morphology_parse ERROR: Unrecognized part of speech.\n");
		return false;
	}

	/* check if concord and index have a non-empty intersection (treating the numbers as set-valued) */
	grammatical_number intersection = NUMBER_NONE;
	if (pos == POS_NOUN) {
		if (!intersect_concord_index(intersection, logical_form.concord, logical_form.index))
			return true;
	} else if (pos == POS_VERB) {
		/* if the verb has multiple words, ensure none are auxiliary */
		if (words.length > 1) {
			for (unsigned int i = 0; i + 1 < words.length; i++)
				if (morphology_parser.is_auxiliary_verb(words[i])) return true;
		}
	}

	sequence root(NULL, 0); root = words;
	bool emitted_proper_noun = false;
	datalog_expression_root marked_logical_form = logical_form;
	const fixed_array<token>& result = morphology_parser.parse(words[head_index]);
	for (unsigned int i = 0; i < result.length; i++) {
		if (result[i].get_part_of_speech() != pos) continue;

		if (pos == POS_VERB) {
			if (!intersect(marked_logical_form.inf, logical_form.inf, result[i].inf)
			 || !has_intersection(logical_form.index, result[i].number))
				continue;
			if (marked_logical_form.inf == INFLECTION_OTHER_VERB)
				marked_logical_form.inf = INFLECTION_NONE;
		} else if (pos == POS_NOUN) {
			if (result[i].number == NUMBER_SINGULAR) {
				if (intersection != NUMBER_SINGULAR && intersection != NUMBER_NON_PLURAL
				 && intersection != NUMBER_ALL && intersection != NUMBER_ANY)
					continue;
			} else if (result[i].number == NUMBER_PLURAL) {
				if (intersection != NUMBER_PLURAL && intersection != NUMBER_NON_SINGULAR
				 && intersection != NUMBER_ALL && intersection != NUMBER_ANY)
					continue;
			} else { /* the noun must be uncountable */
				if (intersection != NUMBER_UNCOUNTABLE
				 && intersection != NUMBER_NON_SINGULAR && intersection != NUMBER_NON_PLURAL
				 && intersection != NUMBER_ALL && intersection != NUMBER_ANY)
					continue;
			}

			marked_logical_form.inf = INFLECTION_NONE;
			if (logical_form.concord == NUMBER_ANY) {
				marked_logical_form.concord = result[i].number;
			} else {
				marked_logical_form.concord = logical_form.concord;
			}
		}

		root[head_index] = result[i].id;
		emitted_proper_noun = emitted_proper_noun || (root[head_index] == words[head_index]
				&& (logical_form.concord != NUMBER_ANY || marked_logical_form.concord == NUMBER_UNCOUNTABLE));
		if (!emit_root(root, marked_logical_form)) {
			free(marked_logical_form);
			free(root); return false;
		}
	}
	free(root);

	/* the noun might be proper */
	if (!emitted_proper_noun && pos == POS_NOUN && (intersection == NUMBER_UNCOUNTABLE
		 || intersection == NUMBER_NON_SINGULAR || intersection == NUMBER_NON_PLURAL
		 || intersection == NUMBER_ALL || intersection == NUMBER_ANY))
	{
		marked_logical_form.inf = INFLECTION_NONE;
		if (logical_form.concord == NUMBER_ANY) {
			marked_logical_form.concord = NUMBER_UNCOUNTABLE;
		} else {
			marked_logical_form.concord = logical_form.concord;
		}

		if (!emit_root(words, marked_logical_form)) {
			free(marked_logical_form);
			return false;
		}
	}

	free(marked_logical_form);
	return true;
}

template<typename PartOfSpeechType>
inline bool morphology_get_inflections(
		const sequence& tokens, PartOfSpeechType pos,
		hash_set<unsigned int>& inflections)
{
	unsigned int head_index;
	if (pos == POS_NOUN) {
		/* noun compounds are head-final (TODO: need a database of exceptions) */
		head_index = tokens.length - 1;
	} else if (pos == POS_VERB) {
		/* verb compounds (phrasal verbs) are head-initial (TODO: need a database of exceptions) */
		head_index = 0;
	} else if (pos == POS_OTHER || pos == POS_ADJECTIVE || pos == POS_ADVERB) {
		head_index = tokens.length; /* there is no head (we don't use the root for adjectives or adverbs in morphology_parse just yet) */
	} else {
		fprintf(stderr, "morphology_get_inflections ERROR: Unrecognized part of speech.\n");
		return false;
	}

	/* first add all uninflected tokens */
	if (!inflections.check_size(inflections.size + tokens.length))
		return false;
	for (unsigned int i = 0; i < tokens.length; i++)
		if (i != head_index) inflections.add(tokens[i]);

	/* this could be a proper noun */
	if (pos == POS_NOUN && !inflections.add(tokens[head_index]))
		return false;

	/* add the inflected forms of the head token */
	return head_index >= tokens.length
		|| morphology_inflect(tokens[head_index], pos, inflections);
}

//inline bool morphology_inflect(sequence& terminal, part_of_speech pos,
//		const datalog_expression_root& logical_form)
//{
//	if (pos == POS_OTHER) {
//		return true;
//	} else if (pos == POS_ADJECTIVE) {
//		/* we don't model adjective morphology, for now */
//		/* adjective compounds are head-final (TODO: need a database of exceptions) */
//		unsigned int head_index = terminal.length - 1;
//		const fixed_array<unsigned int>& result = morphology_inflect(
//				{terminal[head_index], NUMBER_ANY, INFLECTION_ADJECTIVE});
//		if (result.length == 0)
//			return false;
//		terminal[head_index] = result.elements[0];
//		return true;
//	} else if (pos == POS_ADVERB) {
//		/* we don't model adjective morphology, for now */
//		/* adjective compounds are head-final (TODO: need a database of exceptions) */
//		unsigned int head_index = terminal.length - 1;
//		const fixed_array<unsigned int>& result = morphology_inflect(
//				{terminal[head_index], NUMBER_ANY, INFLECTION_ADVERB});
//		if (result.length == 0)
//			return false;
//		terminal[head_index] = result.elements[0];
//		return true;
//	}
//
//	unsigned int head_index;
//	if (pos == POS_NOUN) {
//		/* noun compounds are head-final (TODO: need a database of exceptions) */
//		head_index = terminal.length - 1;
//	} else if (pos == POS_VERB) {
//		/* verb compounds (phrasal verbs) are head-final (TODO: need a database of exceptions) */
//		head_index = 0;
//	} else {
//		fprintf(stderr, "morphology_inflect ERROR: Unrecognized part of speech.\n");
//		return false;
//	}
//
//	/* check if concord and index have a non-empty intersection (treating the numbers as set-valued) */
//	grammatical_number intersection = NUMBER_NONE;
//	if (pos == POS_NOUN) {
//		if (!intersect_concord_index(intersection, logical_form.concord, logical_form.index))
//			return false;
//	} else if (pos == POS_VERB) {
//		/* if the verb has multiple words, ensure none are auxiliary */
//		if (terminal.length > 1) {
//			for (unsigned int i = 0; i + 1 < terminal.length; i++)
//				if (morphology_is_auxiliary_root(terminal[i])) return false;
//		}
//	}
//
//	array<unsigned int> inflected = array<unsigned int>(16);
//	if (pos == POS_VERB) {
//		inflection inf = logical_form.inf;
//		if (inf == INFLECTION_NONE)
//			inf = INFLECTION_OTHER_VERB;
//		const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_ANY, inf});
//		if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		if (has_intersection(logical_form.index, NUMBER_SINGULAR)) {
//			const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_SINGULAR, inf});
//			if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		} if (has_intersection(logical_form.index, NUMBER_PLURAL)) {
//			const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_PLURAL, inf});
//			if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		}
//	} else if (pos == POS_NOUN) {
//		if (has_intersection(intersection, NUMBER_SINGULAR)) {
//			const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_SINGULAR, INFLECTION_NOUN});
//			if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		} if (has_intersection(intersection, NUMBER_PLURAL)) {
//			const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_PLURAL, INFLECTION_NOUN});
//			if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		} if (has_intersection(intersection, NUMBER_UNCOUNTABLE)) {
//			const fixed_array<unsigned int>& result = morphology_inflect({terminal[head_index], NUMBER_UNCOUNTABLE, INFLECTION_NOUN});
//			if (!inflected.append(result.elements, min(1u, result.length))) return false;
//		}
//	}
//
//	if (inflected.length == 0)
//		return false;
//	insertion_sort(inflected);
//	unique(inflected);
//	terminal[head_index] = sample_uniform(inflected);
//	return true;
//}

template<typename Distribution>
inline bool yield(const rule<datalog_expression_root>& terminal,
		const Distribution& rule_distribution, const datalog_expression_root& logical_form,
		token*& sentence, unsigned int& length, unsigned int& capacity)
{
	part_of_speech pos = rule_distribution.get_part_of_speech();
	if (!ensure_capacity(sentence, capacity, length + terminal.length))
		return false;
	for (unsigned int i = 0; i < terminal.length; i++) {
		sentence[length + i].id = terminal.nonterminals[i];
		sentence[length + i].number = NUMBER_NONE;
		sentence[length + i].inf = INFLECTION_NONE;
	}

	if (pos == POS_NOUN) {
		unsigned int head_index = terminal.length - 1;
		if (!intersect_concord_index(
				sentence[length + head_index].number,
				logical_form.concord, logical_form.index))
		{
			fprintf(stderr, "yield ERROR: Invalid grammatical number for noun.\n");
			return false;
		}
		sentence[length + head_index].inf = INFLECTION_NOUN;
	} else if (pos == POS_VERB) {
		unsigned int head_index = 0;
		if (logical_form.inf == INFLECTION_NONE)
			sentence[length + head_index].inf = INFLECTION_OTHER_VERB;
		else sentence[length + head_index].inf = logical_form.inf;
		sentence[length + head_index].number = logical_form.index;
	} else if (pos == POS_ADJECTIVE) {
		unsigned int head_index = terminal.length - 1;
		sentence[length + head_index].inf = INFLECTION_ADJECTIVE;
	} else if (pos == POS_ADVERB) {
		unsigned int head_index = terminal.length - 1;
		sentence[length + head_index].inf = INFLECTION_ADVERB;
	} else if (pos != POS_OTHER) {
		fprintf(stderr, "yield ERROR: Unrecognized part of speech.\n");
		return false;
	}

	length += terminal.length;
	return true;
}


#endif /* DATALOG_H_ */
