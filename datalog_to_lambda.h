/**
 * datalog_to_lambda.h
 *
 *  Created on: Jul 6, 2016
 *      Author: asaparov
 */

#ifndef DATALOG_TO_LAMBDA_H_
#define DATALOG_TO_LAMBDA_H_

#include "datalog.h"
#include "lambda.h"

#include <core/map.h>
#include <core/utility.h>
#include <limits.h>

using namespace core;

template<typename T>
inline bool init(T*& object) {
	object = (T*) malloc(sizeof(T));
	if (object == NULL) {
		fprintf(stderr, "init ERROR: Unable to allocate heap memory.\n");
		return false;
	} else if (!init(*object)) {
		fprintf(stderr, "init ERROR: Unable to initialize object on heap.\n");
		free(object);
		return false;
	}
	return true;
}

struct example {
	array<unsigned int> sentence;
	term logical_form;

	static inline void free(example& ex) {
		core::free(ex.sentence);
		core::free(ex.logical_form);
	}
};

inline bool init(example& ex) {
	if (!array_init(ex.sentence, 16))
		return false;
	ex.logical_form.type = TERM_EMPTY;
	return true;
}

struct variable_map {
	unsigned int* map;
	unsigned int capacity;
	unsigned int size;

	variable_map(unsigned int initial_capacity = 16) : capacity(initial_capacity), size(0) {
		map = (unsigned int*) calloc(initial_capacity, sizeof(unsigned int));
		if (map == NULL) {
			fprintf(stderr, "variable_map ERROR: Out of memory.\n");
			exit(EXIT_FAILURE);
		}
	}

	~variable_map() {
		free(map);
	}
};

inline bool ensure_capacity(variable_map& variables, unsigned int capacity) {
	if (capacity <= variables.capacity)
		return true;

	unsigned int old_capacity = variables.capacity;
	if (!ensure_capacity(variables.map, variables.capacity, capacity)) {
		fprintf(stderr, "set_variable ERROR: Unable to expand variable map.\n");
		return false;
	}

	for (unsigned int i = old_capacity; i < variables.capacity; i++)
		variables.map[i] = 0;
	return true;
}

inline bool new_variable(variable_map& variables,
	unsigned int src, unsigned int value)
{
	if (!ensure_capacity(variables, src + 1))
		return false;
	if (variables.map[src] == UINT_MAX && value != UINT_MAX) {
		fprintf(stderr, "new_variable ERROR: Variable was deleted.\n");
		return false;
	}
	variables.size++;
	variables.map[src] = value;
	return true;
}

inline bool set_variable(variable_map& variables,
	unsigned int src, unsigned int& dst)
{
	if (src < variables.capacity) {
		if (variables.map[src] == 0) {
			dst = variables.size + 1;
			variables.map[src] = dst;
			variables.size++;
			return true;
		} else if (variables.map[src] == UINT_MAX) {
			fprintf(stderr, "set_variable ERROR: Variable was deleted.\n");
			return false;
		} else {
			dst = variables.map[src];
			return true;
		}
	} else {
		dst = variables.size + 1;
		if (!new_variable(variables, src, dst))
			return false;
		return true;
	}
}

inline bool delete_variable(variable_map& variables, unsigned int src) {
	return new_variable(variables, src, UINT_MAX);
}

/* forward declarations */

bool to_lambda_expression(
	const datalog_expression& src,
	expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables);

bool to_lambda_term(
	const datalog_expression& src, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables);

bool to_lambda_function(
	const datalog_predicate& pred, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	unsigned int arity = 0;
	while (arity < array_length(pred.args) && pred.args[arity] != NULL) arity++;
	
	if (arity == 1) {
		if (!init(dst.unary)) return false;
		dst.unary->func = pred.function;
		dst.type = TERM_UNARY_FUNCTION;
		return to_lambda_term(*pred.args[0], dst.unary->arg, names, variables);
	} else if (arity == 2) {
		if (!init(dst.binary)) return false;
		dst.binary->func = pred.function;
		dst.type = TERM_BINARY_FUNCTION;
		return to_lambda_term(*pred.args[0], dst.binary->arg0, names, variables)
			&& to_lambda_term(*pred.args[1], dst.binary->arg1, names, variables);
	} else {
		fprintf(stderr, "ERROR: Functions must be either unary or binary.\n");
		return false;
	}
}

bool to_lambda_term(
	const datalog_expression& src, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	if (src.type == DATALOG_FUNCTION) {
		fprintf(stderr, "ERROR: Higher-order predicates cannot be used as terms.\n");
		return false;
	} else if (src.type == DATALOG_PREDICATE) {
		return to_lambda_function(src.pred, dst, names, variables);
	} else if (src.type == DATALOG_VARIABLE) {
		dst.type = TERM_VARIABLE;
		return set_variable(variables, src.variable, dst.var);
	} else if (src.type == DATALOG_CONSTANT) {
		dst.type = TERM_CONSTANT;
		dst.constant = src.constant.label;
		return true;
	} else {
		fprintf(stderr, "ERROR: Unexpected term type.\n");
		return false;
	}
}

enum objective {
	OBJECTIVE_IDENTITY,
	OBJECTIVE_HEIGHT,
	OBJECTIVE_LENGTH
};

template<term_type T, objective O>
bool to_argminmax(
	const datalog_function& func, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	auto*& minmax = term_field<T>(dst);
	if (!init(minmax))
		return false;

	/* argmax defines a new variable, so make sure the
	   inner expression refers to the correct variable */
	unsigned int old_variable = variables.map[func.vars[0]];
	variables.size++;
	variables.map[func.vars[0]] = variables.size;
	minmax->var = variables.size;

	if (O != OBJECTIVE_IDENTITY) {
		if (!init(minmax->objective.unary)) {
			free(*minmax); free(minmax);
			return false;
		}
		if (O == OBJECTIVE_HEIGHT) {
			minmax->objective.unary->func = PREDICATE_HEIGHT;
		} else if (O == OBJECTIVE_LENGTH) {
			minmax->objective.unary->func = PREDICATE_LENGTH;
		} else {
			fprintf(stderr, "to_argminmax ERROR: Unrecognized objective.\n");
		}
		minmax->objective.unary->arg.var = minmax->var;
		minmax->objective.unary->arg.type = TERM_VARIABLE;
		minmax->objective.type = TERM_UNARY_FUNCTION;
	} else {
		minmax->objective.var = minmax->var;
		minmax->objective.type = TERM_VARIABLE;
	}
	if (!to_lambda_expression(*func.arg, minmax->condition, names, variables)) {
		free(*minmax); free(minmax);
		return false;
	}

	variables.map[func.vars[0]] = old_variable;
	return true;
}

template<term_type T, objective O>
bool to_argminmax(
	const datalog_function& func, expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables,
	const char* predicate_name)
{
	dst.eq = NULL;
	if (func.vars[0] == 0 || func.arg == NULL || !init(dst.eq)
	 || !set_variable(variables, func.vars[0], dst.eq->arg0.var)
	 || !to_argminmax<T, O>(func, dst.eq->arg1, names, variables)) {
		if (dst.eq != NULL) { free(*dst.eq); free(dst.eq); }
		fprintf(stderr, "ERROR: The '%s' predicate requires"
			" two arguments: the first must be a variable reference.\n", predicate_name);
		return false;
	}
	dst.eq->arg0.type = TERM_VARIABLE;
	dst.eq->arg1.type = TERM_ARGMAX;
	dst.type = EXPRESSION_EQUALS;
	return true;
}

template<term_type T, objective O>
bool to_argminmax(
	const datalog_function& func, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables,
	const char* predicate_name)
{
	if (func.vars[0] == 0 || func.arg == NULL
	 || !ensure_capacity(variables, func.vars[0] + 1)
	 || !to_argminmax<T, O>(func, dst, names, variables)) {
		fprintf(stderr, "ERROR: The '%s' predicate requires"
			" two arguments: the first must be a variable reference.\n", predicate_name);
		return false;
	}
	dst.type = TERM_ARGMAX;
	return true;
}

template<term_type T>
bool to_argminmax_count(
	const datalog_function& func, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	auto*& minmax = term_field<T>(dst);
	if (!init(minmax)) return false;

	/* argmin/argmax defines a new variable, so make sure
	   the inner expression refers to the correct variable */
	unsigned int old_variable = variables.map[func.vars[0]];
	variables.size++;
	variables.map[func.vars[0]] = variables.size;
	minmax->var = variables.size;

	if (!init(minmax->objective.count)
	 || !set_variable(variables, func.vars[1], minmax->objective.count->var)) {
		free(*minmax); free(minmax);
		return false;
	}
	minmax->condition.type = EXPRESSION_EMPTY;
	minmax->objective.type = TERM_COUNT;
	if (!to_lambda_expression(*func.arg, minmax->objective.count->condition, names, variables)) {
		free(*minmax); free(minmax);
		return false;
	}

	variables.map[func.vars[0]] = old_variable;
	return true;
}

template<term_type T>
bool to_argminmax_count(
	const datalog_function& func, term& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables,
	const char* predicate_name)
{
	if (func.vars[0] == 0 || func.vars[1] == 0 || func.arg == NULL
	 || !ensure_capacity(variables, func.vars[0] + 1)) {
		fprintf(stderr, "ERROR: The '%s' function requires"
			" three arguments: the first and second must be"
			" variable references.\n", predicate_name);
		return false;
	}
	if (!to_argminmax_count<T>(func, dst, names, variables))
		return false;
	dst.type = T;
	return true;
}

template<term_type T>
bool to_argminmax_count(
	const datalog_function& func,
	expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables,
	const char* predicate_name)
{
	if (func.vars[0] == 0 || func.vars[1] == 0
	 || func.arg == NULL || !init(dst.eq)) {
		fprintf(stderr, "ERROR: The '%s' function requires"
			" three arguments: the first and second must be"
			" variable references.\n", predicate_name);
		return false;
	}
	dst.eq->arg0.type = TERM_VARIABLE;
	dst.type = EXPRESSION_EQUALS;

	if (!set_variable(variables, func.vars[0], dst.eq->arg0.var)
	 || !to_argminmax_count<T>(func, dst.eq->arg1, names, variables)) {
		free(*dst.eq); free(dst.eq);
		return false;
	}
	dst.eq->arg1.type = T;
	return true;
}

bool to_lambda_conjunction(
	const array<datalog_expression*>& src,
	expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	if (src.length == 0) {
		fprintf(stderr, "to_lambda_conjunction ERROR: Tuple is empty.\n");
		return false;
	} else if (src.length == 1) {
		return to_lambda_expression(*src[0], dst, names, variables);
	} else {
		if (!init(dst.conj))
			return false;
		dst.type = EXPRESSION_CONJ;
		if (!dst.conj->operands.ensure_capacity(src.length)) return false;
		for (unsigned int i = 0; i < src.length; i++) {
			if (!to_lambda_expression(*src[i], dst.conj->operands[i], names, variables))
				return false;
			dst.conj->operands.length++;
		}
		return true;
	}
}

bool to_lambda_predicate(
	const datalog_predicate& pred,
	expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	unsigned int arity = 0;
	while (arity < array_length(pred.args) && pred.args[arity] != NULL) arity++;

	if (arity == 1) {
		if (!init(dst.unary)) return false;
		dst.unary->predicate = pred.function;
		dst.type = EXPRESSION_UNARY;
		return to_lambda_term(*pred.args[0], dst.unary->arg, names, variables);
	} else if (arity == 2) {
		if (!init(dst.binary)) return false;
		dst.binary->predicate = pred.function;
		dst.type = EXPRESSION_BINARY;
		return to_lambda_term(*pred.args[0], dst.binary->arg0, names, variables)
			&& to_lambda_term(*pred.args[1], dst.binary->arg1, names, variables);
	} else {
		fprintf(stderr, "ERROR: Predicates must be either unary or binary.\n");
		return false;
	}
}

bool to_lambda_expression(
	const datalog_expression& src,
	expression& dst,
	const hash_map<string, unsigned int>& names,
	variable_map& variables)
{
	switch (src.type) {
	case DATALOG_FUNCTION:
		if (src.func.function == PREDICATE_HIGHEST) {
			/* highest(x, f(x))   -> x = argmax x such that f(x) of height(x) */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_HEIGHT>(src.func, dst, names, variables, "highest");

		} else if (src.func.function == PREDICATE_LOWEST) {
			/* lowest(x, f(x))    -> x = argmin x such that f(x) of height(x) */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_HEIGHT>(src.func, dst, names, variables, "lowest");

		} else if (src.func.function == PREDICATE_LONGEST) {
			/* longest(x, f(x))   -> x = argmax x such that f(x) of length(x) */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_LENGTH>(src.func, dst, names, variables, "longest");

		} else if (src.func.function == PREDICATE_SHORTEST) {
			/* shortest(x, f(x))  -> x = argmin x such that f(x) of length(x) */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_LENGTH>(src.func, dst, names, variables, "shortest");

		} else if (src.func.function == PREDICATE_LARGEST) {
			/* largest(x, f(x))   -> x = argmax x such that f(x) of x */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_IDENTITY>(src.func, dst, names, variables, "largest");

		} else if (src.func.function == PREDICATE_SMALLEST) {
			/* smallest(x, f(x))  -> x = argmin x such that f(x) of x */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_IDENTITY>(src.func, dst, names, variables, "smallest");

		} else if (src.func.function == PREDICATE_MOST) {
			/* most(x, y, f(x))   -> x = argmax x of #{y : f(x, y)} */
			return to_argminmax_count<TERM_ARGMAX>(src.func, dst, names, variables, "most");

		} else if (src.func.function == PREDICATE_FEWEST) {
			/* fewest(x, y, f(x)) -> x = argmin x of #{y : f(x, y)} */
			return to_argminmax_count<TERM_ARGMIN>(src.func, dst, names, variables, "fewest");

		} else if (src.func.function == PREDICATE_NOT) {
			if (src.func.arg == NULL || !init(dst.neg)) return false;
			dst.type = EXPRESSION_NEGATION;
			return to_lambda_expression(*src.func.arg, dst.neg->exp, names, variables);

		} else {
			fprintf(stderr, "ERROR: Encountered disallowed predicates.\n");
			return false;
		}
		
	case DATALOG_PREDICATE:
		return to_lambda_predicate(src.pred, dst, names, variables);

	case DATALOG_TUPLE:
		return to_lambda_conjunction(src.tuple.elements, dst, names, variables);

	default:
		fprintf(stderr, "ERROR: Unexpected expression type.\n");
		return false;
	}
}

bool to_lambda(const datalog_expression& src,
	term& dst, const hash_map<string, unsigned int>& names)
{
	if (src.type != DATALOG_FUNCTION
	 || src.func.function != PREDICATE_ANSWER
	 || src.func.vars[0] == 0 || src.func.arg == NULL) {
		fprintf(stderr, "ERROR: The logical form must begin with"
			" an 'answer' predicate with two arguments.\n");
		return false;
	}

	variable_map variables;
	unsigned int var = src.func.vars[0];
	if (src.func.arg->type == DATALOG_TUPLE) {
		/* the expression is a vanilla lambda expression */
		const datalog_tuple& conj = src.func.arg->tuple;
		if (!init(dst.lambda) || !set_variable(variables, var, dst.lambda->variable))
			return false;
		dst.type = TERM_LAMBDA;
		return to_lambda_conjunction(conj.elements, dst.lambda->exp, names, variables);

	} else if (src.func.arg->type == DATALOG_PREDICATE) {
		/* the expression is a predicate instance */
		const datalog_predicate& pred = src.func.arg->pred;
		if (!init(dst.lambda) || !set_variable(variables, var, dst.lambda->variable))
			return false;
		dst.type = TERM_LAMBDA;
		return to_lambda_predicate(pred, dst.lambda->exp, names, variables);

	} else if (src.func.arg->type == DATALOG_FUNCTION) {
		/* the expression is a higher-order function invocation */
		const datalog_function& func = src.func.arg->func;

		switch (func.function) {
		case PREDICATE_COUNT:
			/* count(x, f(x), y) -> y = #{x : f(x)} */
			if (func.vars[0] == 0 || func.vars[1] != var
			 || func.arg == NULL || !init(dst.count)) {
				fprintf(stderr, "ERROR: The 'count' function requires "
					"three arguments: the first and third must be variable references.\n");
				return false;
			}
			dst.type = TERM_COUNT;
			return set_variable(variables, func.vars[0], dst.count->var)
				&& delete_variable(variables, var)
				&& to_lambda_expression(*func.arg, dst.count->condition, names, variables);

		case PREDICATE_SUM:
			/* sum(x, f(x), y)   -> y = sum of x such that f(x) */
			if (func.vars[0] == 0 || func.vars[1] != var
			 || func.arg == NULL || !init(dst.sum)) {
				fprintf(stderr, "ERROR: The 'sum' function requires "
					"three arguments: the first and third must be variable references.\n");
				return false;
			}
			if (!set_variable(variables, func.vars[0], dst.sum->var))
				return false;
			dst.sum->summand.var = dst.sum->var;
			dst.sum->summand.type = TERM_VARIABLE;
			dst.type = TERM_SUM;
			return delete_variable(variables, var)
				&& to_lambda_expression(*func.arg, dst.sum->condition, names, variables);

		case PREDICATE_HIGHEST:
			/* highest(x, f(x))  -> x = argmax x such that f(x) of height(x) */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_HEIGHT>(func, dst, names, variables, "highest");

		case PREDICATE_LOWEST:
			/* lowest(x, f(x))   -> x = argmin x such that f(x) of height(x) */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_HEIGHT>(func, dst, names, variables, "lowest");

		case PREDICATE_LONGEST:
			/* longest(x, f(x))  -> x = argmax x such that f(x) of length(x) */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_LENGTH>(func, dst, names, variables, "longest");

		case PREDICATE_SHORTEST:
			/* shortest(x, f(x)) -> x = argmin x such that f(x) of length(x) */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_LENGTH>(func, dst, names, variables, "shortest");

		case PREDICATE_LARGEST:
			/* largest(x, f(x))  -> x = argmax x such that f(x) of x */
			return to_argminmax<TERM_ARGMAX, OBJECTIVE_IDENTITY>(func, dst, names, variables, "largest");

		case PREDICATE_SMALLEST:
			/* smallest(x, f(x)) -> x = argmin x such that f(x) of x */
			return to_argminmax<TERM_ARGMIN, OBJECTIVE_IDENTITY>(func, dst, names, variables, "smallest");

		case PREDICATE_MOST:
			/* most(x, y, f(x))  -> x = argmax x of #{y : f(x, y)} */
			return to_argminmax_count<TERM_ARGMAX>(func, dst, names, variables, "most");

		case PREDICATE_FEWEST:
			/* most(x, y, f(x))  -> x = argmin x of #{y : f(x, y)} */
			return to_argminmax_count<TERM_ARGMIN>(func, dst, names, variables, "fewest");

		default:
			fprintf(stderr, "ERROR: Encountered disallowed predicate.\n");
			return false;
		}
	}
	return true;
}

bool to_lambda_example(const datalog_expression& src,
	example& dst, const hash_map<string, unsigned int>& names)
{
	if (!init(dst)) return false;
	if (src.type != DATALOG_PREDICATE
	 || src.pred.function != PREDICATE_PARSE
	 || src.pred.args[0] == NULL || src.pred.args[1] == NULL
	 || src.pred.args[0]->type != DATALOG_LIST
	 || src.pred.args[1]->type != DATALOG_PREDICATE) {
		fprintf(stderr, "ERROR: Each training example must begin "
			"with a 'parse' predicate with two arguments: a list "
			"containing the words of the sentence, and an "
			"expression beginning with an 'answer' function.\n");
		return false;
	}

	const datalog_list& sentence = src.pred.args[0]->list;

	if (sentence.elements.length <= 1) {
		fprintf(stderr, "ERROR: Each sentence must contain at "
			"least one word in addition to the terminating question '?'.\n");
		return false;
	} else if (!dst.sentence.ensure_capacity(sentence.elements.length)) {
		return false;
	}
	for (unsigned int i = 0; i < sentence.elements.length; i++) {
		const datalog_expression* token = sentence.elements[i];
		if (token->type == DATALOG_VARIABLE) {
			dst.sentence[i] = token->variable;
		} else if (token->type == DATALOG_CONSTANT) {
			dst.sentence[i] = token->constant.label;
		} else {
			fprintf(stderr, "ERROR: Every token in the sentence must be a literal.\n");
			return false;
		}
	}
	dst.sentence.length = sentence.elements.length;

	return to_lambda(*src.pred.args[1], dst.logical_form, names);
}

#endif /* DATALOG_TO_LAMBDA_H_ */
