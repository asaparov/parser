/**
 * lambda.h
 *
 *  Created on: Jun 30, 2016
 *	  Author: asaparov
 */

#ifndef LAMBDA_H_
#define LAMBDA_H_

#include <core/array.h>

#include <algorithm>

using namespace core;

/* forward declarations */

struct conjunction;
struct disjunction;
struct lambda_expression;
struct existential;
struct negation;
struct unary_predicate;
struct binary_predicate;
struct equals;
struct argmax;
struct argmin;
struct unary_function;
struct binary_function;
struct count_function;
struct sum_function;

struct term;
struct expression;

enum expression_type {
	EXPRESSION_EMPTY,
	EXPRESSION_CONJ,
	EXPRESSION_DISJ,
	EXPRESSION_EXISTS,
	EXPRESSION_NEGATION,
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_EQUALS
};

struct expression {
	expression_type type;
	union {
		conjunction* conj;
		disjunction* disj;
		existential* exists;
		negation* neg;
		unary_predicate* unary;
		binary_predicate* binary;
		equals* eq;
	};

	static inline void move(const expression& src, expression& dst) {
		dst.type = src.type;
		switch (src.type) {
		case EXPRESSION_CONJ:
			dst.conj = src.conj; break;
		case EXPRESSION_DISJ:
			dst.disj = src.disj; break;
		case EXPRESSION_EXISTS:
			dst.exists = src.exists; break;
		case EXPRESSION_NEGATION:
			dst.neg = src.neg; break;
		case EXPRESSION_UNARY:
			dst.unary = src.unary; break;
		case EXPRESSION_BINARY:
			dst.binary = src.binary; break;
		case EXPRESSION_EQUALS:
			dst.eq = src.eq; break;
		default: return;
		}
	}

	static inline void free(expression& exp) {
		switch (exp.type) {
		case EXPRESSION_EMPTY:
			return;
		case EXPRESSION_CONJ:
			core::free(*exp.conj);
			core::free(exp.conj);
			return;
		case EXPRESSION_DISJ:
			core::free(*exp.disj);
			core::free(exp.disj);
			return;
		case EXPRESSION_EXISTS:
			core::free(*exp.exists);
			core::free(exp.exists);
			return;
		case EXPRESSION_NEGATION:
			core::free(*exp.neg);
			core::free(exp.neg);
			return;
		case EXPRESSION_UNARY:
			core::free(*exp.unary);
			core::free(exp.unary);
			return;
		case EXPRESSION_BINARY:
			core::free(*exp.binary);
			core::free(exp.binary);
			return;
		case EXPRESSION_EQUALS:
			core::free(*exp.eq);
			core::free(exp.eq);
			return;
		}
	}
};

enum term_type {
	TERM_EMPTY,
	TERM_ARGMAX,
	TERM_ARGMIN,
	TERM_UNARY_FUNCTION,
	TERM_BINARY_FUNCTION,
	TERM_VARIABLE,
	TERM_CONSTANT,
	TERM_INTEGER,
	TERM_LAMBDA,
	TERM_COUNT,
	TERM_SUM
};

struct term {
	term_type type;
	union {
		unsigned int var;
		unsigned int constant;
		int integer;
		argmax* max;
		argmin* min;
		unary_function* unary;
		binary_function* binary;
		lambda_expression* lambda;
		count_function* count;
		sum_function* sum;
	};

	static inline void free(term& t) {
		switch (t.type) {
		case TERM_EMPTY:
			return;
		case TERM_ARGMAX:
			core::free(*t.max);
			core::free(t.max);
			return;
		case TERM_ARGMIN:
			core::free(*t.min);
			core::free(t.min);
			return;
		case TERM_UNARY_FUNCTION:
			core::free(*t.unary);
			core::free(t.unary);
			return;
		case TERM_BINARY_FUNCTION:
			core::free(*t.binary);
			core::free(t.binary);
			return;
		case TERM_VARIABLE:
		case TERM_CONSTANT:
		case TERM_INTEGER:
			return;
		case TERM_LAMBDA:
			core::free(*t.lambda);
			core::free(t.lambda);
			return;
		case TERM_COUNT:
			core::free(*t.count);
			core::free(t.count);
			return;
		case TERM_SUM:
			core::free(*t.sum);
			core::free(t.sum);
			return;
		}
	}
};

struct conjunction {
	array<expression> operands;

	static inline void free(conjunction& conj) {
		for (expression& e : conj.operands)
			core::free(e);
		core::free(conj.operands);
	}
};

struct disjunction {
	array<expression> operands;

	static inline void free(disjunction& disj) {
		for (expression& e : disj.operands)
			core::free(e);
		core::free(disj.operands);
	}
};

struct lambda_expression {
	unsigned int variable;
	expression exp;
	

	static inline void free(lambda_expression& lambda) {
		core::free(lambda.exp);
	}
};

struct existential {
	unsigned int variable;
	expression exp;

	static inline void free(existential& exists) {
		core::free(exists.exp);
	}
};

struct negation {
	expression exp;

	static inline void free(negation& neg) {
		core::free(neg.exp);
	}
};

struct unary_predicate {
	unsigned int predicate;
	term arg;

	static inline void free(unary_predicate& unary) {
		core::free(unary.arg);
	}
};

struct binary_predicate {
	unsigned int predicate;
	term arg0, arg1;

	static inline void free(binary_predicate& binary) {
		core::free(binary.arg0);
		core::free(binary.arg1);
	}
};

struct equals {
	term arg0;
	term arg1;

	static inline void free(equals& eq) {
		core::free(eq.arg0);
		core::free(eq.arg1);
	}
};

struct argmax {
	unsigned int var;
	expression condition;
	term objective;

	static inline void free(argmax& max) {
		core::free(max.condition);
		core::free(max.objective);
	}
};

struct argmin {
	unsigned int var;
	expression condition;
	term objective;

	static inline void free(argmin& min) {
		core::free(min.condition);
		core::free(min.objective);
	}
};

struct unary_function {
	unsigned int func;
	term arg;

	static inline void free(unary_function& func) {
		core::free(func.arg);
	}
};

struct binary_function {
	unsigned int func;
	term arg0;
	term arg1;

	static inline void free(binary_function& func) {
		core::free(func.arg0);
		core::free(func.arg1);
	}
};

struct count_function {
	unsigned int var;
	expression condition;

	static inline void free(count_function& count) {
		core::free(count.condition);
	}
};

struct sum_function {
	unsigned int var;
	expression condition;
	term summand;

	static inline void free(sum_function& sum) {
		core::free(sum.condition);
		core::free(sum.summand);
	}
};

inline bool init(conjunction& conj) {
	return array_init(conj.operands, 8);
}

inline bool init(disjunction& disj) {
	return array_init(disj.operands, 4);
}

inline bool init(lambda_expression& lambda) {
	lambda.exp.type = EXPRESSION_EMPTY;
	return true;
}

inline bool init(existential& exists) {
	exists.exp.type = EXPRESSION_EMPTY;
	return true;
}

inline bool init(negation& neg) {
	neg.exp.type = EXPRESSION_EMPTY;
	return true;
}

inline bool init(unary_predicate& unary) {
	unary.arg.type = TERM_EMPTY;
	return true;
}

inline bool init(binary_predicate& binary) {
	binary.arg0.type = TERM_EMPTY;
	binary.arg1.type = TERM_EMPTY;
	return true;
}

inline bool init(equals& eq) {
	eq.arg0.type = TERM_EMPTY;
	eq.arg1.type = TERM_EMPTY;
	return true;
}

inline bool init(argmax& max) {
	max.condition.type = EXPRESSION_EMPTY;
	max.objective.type = TERM_EMPTY;
	return true;
}

inline bool init(argmin& min) {
	min.condition.type = EXPRESSION_EMPTY;
	min.objective.type = TERM_EMPTY;
	return true;
}

inline bool init(unary_function& func) {
	func.arg.type = TERM_EMPTY;
	return true;
}

inline bool init(binary_function& func) {
	func.arg0.type = TERM_EMPTY;
	func.arg1.type = TERM_EMPTY;
	return true;
}

inline bool init(count_function& count) {
	count.condition.type = EXPRESSION_EMPTY;
	return true;
}

inline bool init(sum_function& sum) {
	sum.condition.type = EXPRESSION_EMPTY;
	sum.summand.type = TERM_EMPTY;
	return true;
}


/**
 * Some useful type traits for lambda terms.
 */

template<term_type T>
struct term_types { };

template<>
struct term_types<TERM_ARGMIN> {
	typedef argmin type;
};

template<>
struct term_types<TERM_ARGMAX> {
	typedef argmax type;
};

template<term_type T>
typename term_types<T>::type*& term_field(term& t) {
	fprintf(stderr, "get_optimum ERROR: Unspecialized call.\n");
	exit(EXIT_FAILURE);
}

template<>
argmin*& term_field<TERM_ARGMIN>(term& t) {
	return t.min;
}

template<>
argmax*& term_field<TERM_ARGMAX>(term& t) {
	return t.max;
}


/**
 * Functions for printing lambda calculus expressions.
 */

struct lambda_printer {
	hash_map<unsigned int, string> variables;
	const string** names;

	lambda_printer(const string** names) : variables(16), names(names) { }

	~lambda_printer() {
		for (auto entry : variables)
			free(entry.value);
	}
};

template<typename Stream>
bool print_variable(unsigned int var, Stream& out, lambda_printer& printer) {
	if (!printer.variables.check_size()) return false;

	bool contains;
	unsigned int bucket;
	string& name = printer.variables.get(var, contains, bucket);
	if (!contains) {
		unsigned int new_var = printer.variables.table.size + 1;
		int length = snprintf(NULL, 0, "$%u", new_var);

		if (!init(name, length + 1)
		 || snprintf(name.data, length + 1, "$%u", new_var) != length)
			return false;
		name.length--; /* remove the null terminating character */
		printer.variables.table.keys[bucket] = var;
		printer.variables.table.size++;
	}
	return print(name, out);
}

template<typename Stream>
bool print(conjunction& conj, Stream& out, lambda_printer& printer)
{
	if (conj.operands.length == 0)
		return print("(and <empty>)", out);

	if (!print("(and ", out)
	 || !print(conj.operands[0], out, printer))
		return false;
	for (unsigned int i = 1; i < conj.operands.length; i++) {
		if (!print(' ', out)
		 || !print(conj.operands[i], out, printer))
			return false;
	}
	return print(')', out);
}

template<typename Stream>
bool print(disjunction& disj, Stream& out, lambda_printer& printer)
{
	if (disj.operands.length == 0)
		return print("(or <empty>)", out);

	if (!print("(or ", out)
		|| !print(disj.operands[0], out, printer))
		return false;
	for (unsigned int i = 1; i < disj.operands.length; i++) {
		if (!print(' ', out)
			|| !print(disj.operands[i], out, printer))
			return false;
	}
	return print(')', out);
}

template<typename Stream>
bool print(existential& ex, Stream& out, lambda_printer& printer) {
	return print("(exists ", out)
		&& print_variable(ex.variable, out, printer)
		&& print(' ', out)
		&& print(ex.exp, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(negation& neg, Stream& out, lambda_printer& printer) {
	return print("(not ", out)
		&& print(neg.exp, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(unary_predicate& unary, Stream& out, lambda_printer& printer) {
	return print('(', out)
		&& print(*printer.names[unary.predicate], out)
		&& print(' ', out)
		&& print(unary.arg, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(binary_predicate& binary, Stream& out, lambda_printer& printer) {
	return print('(', out)
		&& print(*printer.names[binary.predicate], out)
		&& print(' ', out)
		&& print(binary.arg0, out, printer)
		&& print(' ', out)
		&& print(binary.arg1, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(equals& eq, Stream& out, lambda_printer& printer) {
	return print("(equals ", out)
		&& print(eq.arg0, out, printer)
		&& print(' ', out)
		&& print(eq.arg1, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(expression& e, Stream& out, lambda_printer& printer) {
	switch (e.type) {
	case EXPRESSION_EMPTY:
		return print("<empty>", out);
	case EXPRESSION_CONJ:
		return print(*e.conj, out, printer);
	case EXPRESSION_DISJ:
		return print(*e.disj, out, printer);
	case EXPRESSION_EXISTS:
		return print(*e.exists, out, printer);
	case EXPRESSION_NEGATION:
		return print(*e.neg, out, printer);
	case EXPRESSION_UNARY:
		return print(*e.unary, out, printer);
	case EXPRESSION_BINARY:
		return print(*e.binary, out, printer);
	case EXPRESSION_EQUALS:
		return print(*e.eq, out, printer);
	default:
		fprintf(stderr, "print ERROR: Unrecognized expression type.\n");
		return false;
	}
}

template<typename Stream>
bool print(argmax& max, Stream& out, lambda_printer& printer) {
	return print("(argmax ", out)
		&& print_variable(max.var, out, printer)
		&& print(' ', out)
		&& print(max.condition, out, printer)
		&& print(' ', out)
		&& print(max.objective, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(argmin& min, Stream& out, lambda_printer& printer) {
	return print("(argmin ", out)
		&& print_variable(min.var, out, printer)
		&& print(' ', out)
		&& print(min.condition, out, printer)
		&& print(' ', out)
		&& print(min.objective, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(unary_function& unary, Stream& out, lambda_printer& printer) {
	return print('(', out)
		&& print(*printer.names[unary.func], out)
		&& print(' ', out)
		&& print(unary.arg, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(binary_function& binary, Stream& out, lambda_printer& printer) {
	return print('(', out)
		&& print(*printer.names[binary.func], out)
		&& print(' ', out)
		&& print(binary.arg0, out, printer)
		&& print(' ', out)
		&& print(binary.arg1, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(lambda_expression& lambda, Stream& out, lambda_printer& printer) {
	return print("(lambda ", out)
		&& print_variable(lambda.variable, out, printer)
		&& print(' ', out)
		&& print(lambda.exp, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(count_function& count, Stream& out, lambda_printer& printer) {
	return print("(count ", out)
		&& print_variable(count.var, out, printer)
		&& print(' ', out)
		&& print(count.condition, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print(sum_function& sum, Stream& out, lambda_printer& printer) {
	return print("(sum ", out)
		&& print_variable(sum.var, out, printer)
		&& print(' ', out)
		&& print(sum.condition, out, printer)
		&& print(' ', out)
		&& print(sum.summand, out, printer)
		&& print(')', out);
}

template<typename Stream>
bool print_constant(const string& constant, Stream& out) {
	unsigned int index = constant.index_of(' ');
	if (index < constant.length) {
		return print('\'', out)
			&& print(constant, out)
			&& print('\'', out);
	} else return print(constant, out);
}

template<typename Stream>
bool print(term& t, Stream& out, lambda_printer& printer) {
	switch (t.type) {
	case TERM_EMPTY:
		return print("<empty>", out);
	case TERM_ARGMAX:
		return print(*t.max, out, printer);
	case TERM_ARGMIN:
		return print(*t.min, out, printer);
	case TERM_UNARY_FUNCTION:
		return print(*t.unary, out, printer);
	case TERM_BINARY_FUNCTION:
		return print(*t.binary, out, printer);
	case TERM_VARIABLE:
		return print_variable(t.var, out, printer);
	case TERM_CONSTANT:
		return print_constant(*printer.names[t.constant], out);
	case TERM_INTEGER:
		return print(t.integer, out);
	case TERM_LAMBDA:
		return print(*t.lambda, out, printer);
	case TERM_COUNT:
		return print(*t.count, out, printer);
	case TERM_SUM:
		return print(*t.sum, out, printer);
	default:
		fprintf(stderr, "print ERROR: Unrecognized term type.\n");
		return false;
	}
}


/**
 * Functionality for determining variable scope and
 * inserting missing existential quantifiers.
 */

struct scope {
	array<unsigned int> in_scope;
	array<unsigned int> disallowed;

	static inline void move(const scope& src, scope& dst) {
		core::move(src.in_scope, dst.in_scope);
		core::move(src.disallowed, dst.disallowed);
	}

	static inline void free(scope& s) {
		core::free(s.in_scope);
		core::free(s.disallowed);
	}
};

inline bool init(scope& s, unsigned int initial_capacity) {
	if (!array_init(s.in_scope, initial_capacity)) {
		return false;
	} else if (!array_init(s.disallowed, initial_capacity)) {
		free(s.in_scope);
		return false;
	}
	return true;
}

struct context {
	hash_map<const expression*, scope*> expressions;
	hash_map<const term*, scope*> terms;

	context() : expressions(16), terms(16) { }

	~context() {
		for (auto entry : expressions) {
			free(*entry.value);
			free(entry.value);
		}
		for (auto entry : terms) {
			free(*entry.value);
			free(entry.value);
		}
	}
};

/* forward declarations */

bool declare_variables(
	term& t, scope& current_scope, context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared);
bool declare_variables(
	expression& e, scope& current_scope, context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared);

template<typename T>
bool compute_scope(scope& current_scope, context& ctx, const T& child) {
	/* get scope information of the child */
	scope* child_scope = compute_scope(child, ctx);
	if (child_scope == NULL)
		return false;

	/* check that no disallowed variable is in scope */
	if (!has_intersection(current_scope.in_scope, child_scope->disallowed)
	 || !has_intersection(current_scope.disallowed, child_scope->in_scope)) {
		fprintf(stderr, "ERROR: Found disallowed variable in scope.\n");
		return false;
	}

	/* merge the child scope into the parent scope */
	array<unsigned int> scratch(16);
	swap(current_scope.in_scope, scratch);
	if (!set_union(current_scope.in_scope, scratch, child_scope->in_scope))
		return false;
	scratch.clear();

	swap(current_scope.disallowed, scratch);
	if (!set_union(current_scope.disallowed, scratch, child_scope->disallowed))
		return false;

#if !defined(NDEBUG)
	if (std::adjacent_find(
		current_scope.in_scope.begin(), current_scope.in_scope.end()) != current_scope.in_scope.end())
	{
		fprintf(stderr, "compute_scope ERROR: in_scope array is not sorted.\n");
		return false;
	} else if (std::adjacent_find(
		current_scope.disallowed.begin(), current_scope.disallowed.end()) != current_scope.disallowed.end())
	{
		fprintf(stderr, "compute_scope ERROR: disallowed array is not sorted.\n");
		return false;
	}
#endif
	return true;
}

bool compute_scope(scope& current_scope,
	context& ctx, const array<expression>& children)
{
	for (const expression& child : children)
		if (!compute_scope(current_scope, ctx, child))
			return false;
	return true;
}

template<typename Arg, typename... Args>
bool compute_scope(scope& current_scope,
	context& ctx, const Arg& first, const Args&... args)
{
	if (!compute_scope(current_scope, ctx, first) || !compute_scope(current_scope, ctx, args...))
		return false;
	return true;
}

template<typename T>
scope* get_scope(const T& node, hash_map<const T*, scope*>& table) {
	bool contains;
	unsigned int index;
	if (!table.check_size())
		return NULL;
	scope*& current_scope = table.get(&node, contains, index);
	if (!contains) {
		current_scope = (scope*) malloc(sizeof(scope));
		if (current_scope == NULL) return NULL;
		if (!init(*current_scope, 8)) {
			free(current_scope);
			return NULL;
		}
		table.table.keys[index] = &node;
		table.table.size++;
	}
	return current_scope;
}

bool disallow_variable(scope& s, unsigned int var) {
	unsigned int index = s.in_scope.index_of(var);
	if (index < s.in_scope.length) {
		s.in_scope.remove(index);
		insertion_sort(s.in_scope);
	}
	if (!s.disallowed.add(var)) return false;
	insertion_sort(s.disallowed);
	return true;
}

scope* compute_scope(const term& t, context& ctx)
{
	scope* current_scope = get_scope(t, ctx.terms);
	if (current_scope == NULL) return NULL;

	switch (t.type) {
	case TERM_EMPTY:
		return current_scope;
	case TERM_ARGMAX:
		return (compute_scope(*current_scope, ctx, t.max->condition, t.max->objective)
			&& disallow_variable(*current_scope, t.max->var)) ? current_scope : NULL;
	case TERM_ARGMIN:
		return (compute_scope(*current_scope, ctx, t.min->condition, t.min->objective)
			&& disallow_variable(*current_scope, t.min->var)) ? current_scope : NULL;
		return current_scope;
	case TERM_UNARY_FUNCTION:
		return compute_scope(*current_scope, ctx, t.unary->arg) ? current_scope : NULL;
	case TERM_BINARY_FUNCTION:
		return compute_scope(*current_scope, ctx, t.binary->arg0, t.binary->arg1) ? current_scope : NULL;
	case TERM_VARIABLE:
		return current_scope->in_scope.add(t.var) ? current_scope : NULL;
	case TERM_CONSTANT:
	case TERM_INTEGER:
		return current_scope;
	case TERM_LAMBDA:
		return (compute_scope(*current_scope, ctx, t.lambda->exp)
			&& disallow_variable(*current_scope, t.lambda->variable)) ? current_scope : NULL;
	case TERM_COUNT:
		return (compute_scope(*current_scope, ctx, t.count->condition)
			&& disallow_variable(*current_scope, t.count->var)) ? current_scope : NULL;
	case TERM_SUM:
		return (compute_scope(*current_scope, ctx, t.sum->condition, t.sum->summand)
			&& disallow_variable(*current_scope, t.sum->var)) ? current_scope : NULL;
	default:
		fprintf(stderr, "compute_scope ERROR: Unrecognized term type.\n");
		return NULL;
	}
}

scope* compute_scope(const expression& e, context& ctx)
{
	scope* current_scope = get_scope(e, ctx.expressions);
	if (current_scope == NULL) return NULL;

	switch (e.type) {
	case EXPRESSION_EMPTY:
		return current_scope;
	case EXPRESSION_CONJ:
		return compute_scope(*current_scope, ctx, e.conj->operands) ? current_scope : NULL;
	case EXPRESSION_DISJ:
		return compute_scope(*current_scope, ctx, e.disj->operands) ? current_scope : NULL;
	case EXPRESSION_EXISTS:
		return (compute_scope(*current_scope, ctx, e.exists->exp)
			&& disallow_variable(*current_scope, e.exists->variable)) ? current_scope : NULL;
	case EXPRESSION_NEGATION:
		return compute_scope(*current_scope, ctx, e.neg->exp) ? current_scope : NULL;
	case EXPRESSION_UNARY:
		return compute_scope(*current_scope, ctx, e.unary->arg) ? current_scope : NULL;
	case EXPRESSION_BINARY:
		return compute_scope(*current_scope, ctx, e.binary->arg0, e.binary->arg1) ? current_scope : NULL;
	case EXPRESSION_EQUALS:
		return compute_scope(*current_scope, ctx, e.eq->arg0, e.eq->arg1) ? current_scope : NULL;
	default:
		fprintf(stderr, "compute_scope ERROR: Unrecognized expression type.\n");
		return NULL;
	}
}

scope* get_scope(const term& t, const context& ctx) {
	bool contains;
	scope* s = ctx.terms.get(&t, contains);
	if (!contains) return NULL;
	else return s;
}

scope* get_scope(const expression& e, const context& ctx) {
	bool contains;
	scope* s = ctx.expressions.get(&e, contains);
	if (!contains) return NULL;
	else return s;
}

bool declare_variables(expression& e, array<unsigned int>& declare) {
	if (declare.length == 0) return true;
	insertion_sort(declare);
	unique(declare);

	expression* prev = &e;
	for (unsigned int var : declare) {
		existential* declaration = (existential*) malloc(sizeof(existential));
		if (declaration == NULL) {
			return false;
		} else if (!init(*declaration)) {
			free(declaration);
			return false;
		}

		declaration->variable = var;
		move(*prev, declaration->exp);
		e.type = EXPRESSION_EXISTS;
		e.exists = declaration;
		prev = &e;
	}
	return true;
}

template<typename MinMax>
bool declare_variables_minmax(
	MinMax& minmax,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* objective_scope = get_scope(minmax.objective, ctx);
	scope* condition_scope = get_scope(minmax.condition, ctx);
	if (condition_scope == NULL || objective_scope == NULL) return false;

	/* these variables must be declared higher up in the tree */
	if (!set_intersect(undeclareable, condition_scope->in_scope, objective_scope->in_scope))
		return false;

	/* ensure the optimizing variable is not declared higher up */
	unsigned int index = undeclareable.index_of(minmax.var);
	if (index < undeclareable.length)
		undeclareable.remove(index);

	array<unsigned int> difference(8), declare(8);
	if (!set_subtract(difference, condition_scope->in_scope, undeclareable))
		return false;
	swap(difference, condition_scope->in_scope);
	if (!declared.add(minmax.var)
	 || !declare_variables(minmax.condition, *condition_scope, ctx, declare, declared)
	 || !declare_variables(minmax.condition, declare))
		return false;

	difference.clear();
	if (!set_subtract(difference, objective_scope->in_scope, undeclareable))
		return false;
	swap(difference, objective_scope->in_scope);
	return declare_variables(minmax.objective, *objective_scope, ctx, undeclareable, declared);
}

template<typename Unary>
bool declare_variables_unary(
	Unary& unary,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* arg_scope = get_scope(unary.arg, ctx);
	if (arg_scope == NULL) return false;

	return declare_variables(unary.arg, *arg_scope, ctx, undeclareable, declared);
}

template<typename Binary>
bool declare_variables_binary(
	Binary& binary,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* arg0_scope = get_scope(binary.arg0, ctx);
	scope* arg1_scope = get_scope(binary.arg1, ctx);
	if (arg0_scope == NULL || arg1_scope == NULL) return false;

	/* these variables must be declared higher up in the tree */
	if (!set_intersect(undeclareable, arg0_scope->in_scope, arg1_scope->in_scope))
		return false;

	array<unsigned int> difference(8);
	if (!set_subtract(difference, arg0_scope->in_scope, undeclareable))
		return false;
	swap(difference, arg0_scope->in_scope);
	if (!declare_variables(binary.arg0, *arg0_scope, ctx, undeclareable, declared))
		return false;

	difference.clear();
	if (!set_subtract(difference, arg1_scope->in_scope, undeclareable))
		return false;
	swap(difference, arg1_scope->in_scope);
	return declare_variables(binary.arg1, *arg1_scope, ctx, undeclareable, declared);
}

bool declare_variables(
	unsigned int& variable,
	expression& exp,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* arg_scope = get_scope(exp, ctx);
	if (arg_scope == NULL) return false;

	array<unsigned int> declare(8);
	if (!declared.add(variable)
	 || !declare_variables(exp, *arg_scope, ctx, declare, declared))
		return false;
	declared.pop();

	return declare_variables(exp, declare);
}

bool declare_variables(
	sum_function& sum,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* condition_scope = get_scope(sum.condition, ctx);
	scope* summand_scope = get_scope(sum.summand, ctx);
	if (condition_scope == NULL || summand_scope == NULL) return false;

	/* these variables must be declared higher up in the tree */
	if (!set_intersect(undeclareable, condition_scope->in_scope, summand_scope->in_scope))
		return false;

	array<unsigned int> difference(8), declare(8);
	if (!set_subtract(difference, condition_scope->in_scope, undeclareable))
		return false;
	swap(difference, condition_scope->in_scope);
	if (!declare_variables(sum.condition, *condition_scope, ctx, declare, declared)
	 || !declare_variables(sum.condition, declare))
		return false;

	difference.clear();
	if (!set_subtract(difference, summand_scope->in_scope, undeclareable))
		return false;
	swap(difference, summand_scope->in_scope);
	return declare_variables(sum.summand, *summand_scope, ctx, undeclareable, declared);
}

bool declare_variables(
	array<expression>& children,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	if (children.length == 0)
		return true;

	scope** children_scope = (scope**) alloca(sizeof(scope*) * children.length);
	if (children_scope == NULL) return false;
	for (unsigned int i = 0; i < children.length; i++) {
		children_scope[i] = get_scope(children[i], ctx);
		if (children_scope[i] == NULL) return false;
	}

	/* check the conjuncts for overlapping variables */
	for (unsigned int var : current_scope.in_scope) {
		if (declared.contains(var)) continue;

		bool found = false;
		for (unsigned int i = 0; i < children.length; i++) {
			if (children_scope[i]->in_scope.contains(var)) {
				if (found == true) {
					/* this variable appears in at least two conjuncts */
					if (!undeclareable.add(var)) return false;
					break;
				} else found = true;
			}
		}
	}

	array<unsigned int> difference(8);
	for (unsigned int i = 0; i < children.length; i++) {
		array<unsigned int>& child_scope = children_scope[i]->in_scope;
		if (!set_subtract(difference, child_scope, undeclareable))
			return false;
		swap(difference, child_scope);
		if (!declare_variables(children[i], *children_scope[i], ctx, undeclareable, declared))
			return false;
		difference.clear();
	}

	return true;
}

bool declare_variables(
	negation& neg,
	scope& current_scope,
	context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	scope* child_scope = get_scope(neg.exp, ctx);
	if (child_scope == NULL) return false;

	array<unsigned int> declare(8);
	return declare_variables(neg.exp, *child_scope, ctx, declare, declared)
		&& declare_variables(neg.exp, declare);
}

bool declare_variables(
	term& t, scope& current_scope, context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	switch (t.type) {
	case TERM_EMPTY:			return true;
	case TERM_CONSTANT:			return true;
	case TERM_INTEGER:			return true;
	case TERM_ARGMAX:			return declare_variables_minmax(*t.max, current_scope, ctx, undeclareable, declared);
	case TERM_ARGMIN:			return declare_variables_minmax(*t.min, current_scope, ctx, undeclareable, declared);
	case TERM_UNARY_FUNCTION:	return declare_variables_unary(*t.unary, current_scope, ctx, undeclareable, declared);
	case TERM_BINARY_FUNCTION:	return declare_variables_binary(*t.binary, current_scope, ctx, undeclareable, declared);
	case TERM_SUM:				return declare_variables(*t.sum, current_scope, ctx, undeclareable, declared);
	case TERM_COUNT:
		return declare_variables(t.count->var, t.count->condition, current_scope, ctx, undeclareable, declared);
	case TERM_LAMBDA:
		return declare_variables(t.lambda->variable, t.lambda->exp, current_scope, ctx, undeclareable, declared);
	case TERM_VARIABLE:
		if (!declared.contains(t.var))
			return undeclareable.add(t.var);
		else return true;
	default:
		fprintf(stderr, "declare_variables ERROR: Unrecognized term type.\n");
		return false;
	}
}

bool declare_variables(
	expression& e, scope& current_scope, context& ctx,
	array<unsigned int>& undeclareable,
	array<unsigned int>& declared)
{
	switch (e.type) {
	case EXPRESSION_EMPTY:		return true;
	case EXPRESSION_CONJ:		return declare_variables(e.conj->operands, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_DISJ:		return declare_variables(e.disj->operands, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_NEGATION:	return declare_variables(*e.neg, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_UNARY:		return declare_variables_unary(*e.unary, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_BINARY:		return declare_variables_binary(*e.binary, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_EQUALS:		return declare_variables_binary(*e.eq, current_scope, ctx, undeclareable, declared);
	case EXPRESSION_EXISTS:
		return declare_variables(e.exists->variable, e.exists->exp, current_scope, ctx, undeclareable, declared);
	default:
		fprintf(stderr, "declare_variables ERROR: Unrecognized expression type.\n");
		return false;
	}
}

bool declare_variables(term& t, context& ctx)
{
	scope* current_scope = get_scope(t, ctx);
	if (current_scope == NULL) return false;

	array<unsigned int> undeclareable(16), declared(16);
	return declare_variables(t, *current_scope, ctx, undeclareable, declared);
}

#endif /* LAMBDA_H_ */
