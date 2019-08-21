/**
 * datalog_hdp.h
 *
 *  Created on: Mar 3, 2017
 *      Author: asaparov
 */

#ifndef DATALOG_HDP_H_
#define DATALOG_HDP_H_

#include <hdp/mcmc.h>

#include "datalog.h"

constexpr unsigned int ARG_POSITION_UNARY = 1;
constexpr unsigned int ARG_POSITION_FIRST = 2;
constexpr unsigned int ARG_POSITION_SECOND = 3;
constexpr unsigned int ARG_POSITION_FIRST_PARENT = 4;
constexpr unsigned int ARG_POSITION_SECOND_PARENT = 5;
constexpr unsigned int ARG_POSITION_INTEGER = 6;
constexpr unsigned int NUM_ARG_POSITIONS = 6;
constexpr unsigned int ARG_POSITION_FIRST_OR_SECOND = 7;
constexpr unsigned int ARG_POSITION_FIRST_OR_SECOND_PARENT = 8;

template<unsigned int ArgPosition>
inline unsigned int parent_arg_position() {
	fprintf(stderr, "parent_arg_position ERROR: This function is only "
			"defined for ARG_POSITION_FIRST and ARG_POSITION_SECOND.\n");
	exit(EXIT_FAILURE);
}

template<>
inline unsigned int parent_arg_position<ARG_POSITION_FIRST>() {
	return ARG_POSITION_FIRST_PARENT;
}

template<>
inline unsigned int parent_arg_position<ARG_POSITION_SECOND>() {
	return ARG_POSITION_SECOND_PARENT;
}

template<>
inline unsigned int parent_arg_position<ARG_POSITION_FIRST_OR_SECOND>() {
	return ARG_POSITION_FIRST_OR_SECOND_PARENT;
}

struct datalog_term {
	unsigned int predicate; /* this is the label of the term */

	/* if arg_position is greater than NUM_ARG_POSITIONS, then the term
	   contains two arguments, the first is the variable given by the current
	   vertex, and the second of which is a function whose label is this field */
	unsigned int arg_position;

	constexpr datalog_term(unsigned int predicate, unsigned int arg_position) :
		predicate(predicate), arg_position(arg_position) { }

	inline void zero() {
		predicate = 0;
	}

	inline void any() {
		predicate = DATALOG_LABEL_WILDCARD;
		arg_position = DATALOG_LABEL_WILDCARD;
	}

	static inline unsigned int hash(const datalog_term& edge) {
		return default_hash(edge);
	}

	static inline bool is_empty(const datalog_term& edge) {
		return edge.predicate == 0;
	}

	static inline void move(const datalog_term& src, datalog_term& dst) {
		dst.predicate = src.predicate;
		dst.arg_position = src.arg_position;
	}

	static inline void swap(datalog_term& first, datalog_term& second) {
		core::swap(first.predicate, second.predicate);
		core::swap(first.arg_position, second.arg_position);
	}

	static inline void free(datalog_term& edge) { }
};

inline bool init(datalog_term& term, const datalog_term& src) {
	term.predicate = src.predicate;
	term.arg_position = src.arg_position;
	return true;
}

inline bool operator == (const datalog_term& first, const datalog_term& second) {
	return first.predicate == second.predicate
		&& first.arg_position == second.arg_position;
}

inline bool operator != (const datalog_term& first, const datalog_term& second) {
	return first.predicate != second.predicate
		|| first.arg_position != second.arg_position;
}

inline bool operator < (const datalog_term& first, const datalog_term& second) {
	if (first.predicate < second.predicate) return true;
	else if (first.predicate > second.predicate) return false;
	else if (first.arg_position < second.arg_position) return true;
	else return false;
}

template<typename Stream, typename Printer>
inline bool print_arg_position(unsigned int position, Stream& out, Printer& printer) {
	if (position == ARG_POSITION_UNARY) {
		return print("(X)", out);
	} else if (position == ARG_POSITION_FIRST) {
		return print("(X,y)", out);
	} else if (position == ARG_POSITION_SECOND) {
		return print("(y,X)", out);
	} else if (position == ARG_POSITION_FIRST_OR_SECOND) {
		return print("(y,X) OR (X,y)", out);
	} else if (position == ARG_POSITION_FIRST_PARENT) {
		return print("(X,p)", out);
	} else if (position == ARG_POSITION_SECOND_PARENT) {
		return print("(p,X)", out);
	} else if (position == ARG_POSITION_FIRST_OR_SECOND_PARENT) {
		return print("(X,p) OR (p,X)", out);
	} else if (position == ARG_POSITION_INTEGER) {
		return print("(X,int)", out);
	} else if (position == DATALOG_LABEL_EMPTY) {
		return true;
	} else {
		return print("(X,", out) && print(position, out, printer) && print("(*))", out);
	}
}

template<typename Stream, typename Printer>
bool print(const datalog_term& edge, Stream& out, Printer& printer) {
	if (!print(edge.predicate, out, printer)) return false;
	return print_arg_position(edge.arg_position, out, printer);
}

template<typename Printer>
struct datalog_term_printer {
	Printer& printer;

	datalog_term_printer(Printer& printer) : printer(printer) { }
};

template<typename Stream, typename Printer>
bool print(unsigned int edge, Stream& out, unsigned int level, datalog_term_printer<Printer>& printer) {
	if (level == 0) {
		return print(edge, out, printer.printer);
	} else if (level == 1) {
		return print_arg_position(edge, out, printer.printer);
	} else {
		fprintf(stderr, "print ERROR: Invalid argument 'level' when printing datalog_term.\n");
		return false;
	}
}

struct datalog_term_set {
	datalog_term edge;
	unsigned int* excluded_predicates;
	unsigned int excluded_predicate_count;
	unsigned int* excluded_functions;
	unsigned int excluded_function_count;

	constexpr datalog_term_set(
			unsigned int predicate, unsigned int* excluded_predicates, unsigned int excluded_predicate_count,
			unsigned int function = 0, unsigned int* excluded_functions = NULL, unsigned int excluded_function_count = 0) :
				edge(predicate, function), excluded_predicates(excluded_predicates),
				excluded_predicate_count(excluded_predicate_count), excluded_functions(excluded_functions),
				excluded_function_count(excluded_function_count)
	{ }

	inline void zero() {
		edge.zero();
	}

	inline void any() {
		edge.any();
		excluded_predicate_count = 0;
		excluded_function_count = 0;
	}

	static inline unsigned int hash(const datalog_term_set& set) {
		unsigned int value = datalog_term::hash(set.edge);
		if (set.excluded_predicate_count > 0)
			value ^= default_hash(set.excluded_predicates, set.excluded_predicate_count);
		if (set.excluded_function_count > 0)
			value ^= default_hash(set.excluded_functions, set.excluded_function_count);
		return value;
	}

	static inline bool is_empty(const datalog_term_set& set) {
		return datalog_term::is_empty(set.edge);
	}

	static inline void move(const datalog_term_set& src, datalog_term_set& dst) {
		core::move(src.edge, dst.edge);
		dst.excluded_predicates = src.excluded_predicates;
		dst.excluded_predicate_count = src.excluded_predicate_count;
		dst.excluded_functions = src.excluded_functions;
		dst.excluded_function_count = src.excluded_function_count;
	}

	static inline void free(datalog_term_set& set) {
		core::free(set.edge);
		if (set.excluded_predicate_count > 0)
			core::free(set.excluded_predicates);
		if (set.excluded_function_count > 0)
			core::free(set.excluded_functions);
	}
};

inline bool init(datalog_term_set& set, const datalog_term_set& src) {
	if (!init(set.edge, src.edge)) return false;
	if (src.excluded_predicate_count > 0) {
		set.excluded_predicates = (unsigned int*) malloc(sizeof(unsigned int) * src.excluded_predicate_count);
		if (set.excluded_predicates == NULL) {
			fprintf(stderr, "init ERROR: Insufficient memory for excluded_predicates in datalog_term_set.\n");
			free(set.edge); return false;
		}
		memcpy(set.excluded_predicates, src.excluded_predicates, sizeof(unsigned int) * src.excluded_predicate_count);
	}
	set.excluded_predicate_count = src.excluded_predicate_count;
	if (src.excluded_function_count > 0) {
		set.excluded_functions = (unsigned int*) malloc(sizeof(unsigned int) * src.excluded_function_count);
		if (set.excluded_functions == NULL) {
			fprintf(stderr, "init ERROR: Insufficient memory for excluded_functions in datalog_term_set.\n");
			free(set.edge); free(set.excluded_predicates);
			return false;
		}
		memcpy(set.excluded_functions, src.excluded_functions, sizeof(unsigned int) * src.excluded_function_count);
	}
	set.excluded_function_count = src.excluded_function_count;
	return true;
}

inline bool operator == (const datalog_term_set& first, const datalog_term_set& second) {
	if (first.edge != second.edge
	 || first.excluded_predicate_count != second.excluded_predicate_count
	 || first.excluded_function_count != second.excluded_function_count)
		return false;
	for (unsigned int i = 0; i < first.excluded_predicate_count; i++)
		if (first.excluded_predicates[i] != second.excluded_predicates[i]) return false;
	for (unsigned int i = 0; i < first.excluded_function_count; i++)
		if (first.excluded_functions[i] != second.excluded_functions[i]) return false;
	return true;
}

struct datalog_term_distribution {
	typedef double value_type;

	static constexpr double ALPHA_MOST = 1000.0;
	static const double LOG_ALPHA_MOST;

	unsigned int predicate_count; /* boolean-valued */
	unsigned int function_count; /* object-valued */
	double total_edges;
	double log_total_edges;

	datalog_term_distribution(const std::array<unsigned int, 2>& params) :
		predicate_count(params[0]), function_count(params[1]),
		total_edges(predicate_count * NUM_ARG_POSITIONS + function_count + ALPHA_MOST - 1),
		log_total_edges(log(total_edges)) { }

	inline double log_probability(const datalog_term& edge) const {
		if (edge.predicate == PREDICATE_MOST)
			return LOG_ALPHA_MOST - log_total_edges;
		else return -log_total_edges;
	}

	inline double log_probability(const array_multiset<datalog_term>& edges) const {
		double sum = 0.0;
		for (const auto& entry : edges.counts)
			sum += log_probability(entry.key) * entry.value;
		return sum;
	}

	inline double probability(const datalog_term& edge) const {
		if (edge.predicate == PREDICATE_MOST)
			return ALPHA_MOST / total_edges;
		else return 1.0 / total_edges;
	}
};

const double datalog_term_distribution::LOG_ALPHA_MOST = log(datalog_term_distribution::ALPHA_MOST);

struct datalog_prior
{
	hdp<datalog_term_distribution, constant<datalog_term>, datalog_term, double> edge_hdp;
	hdp_sampler<datalog_term_distribution, constant<datalog_term>, datalog_term, double> edge_sampler;
	cache<datalog_term_distribution, constant<datalog_term>, datalog_term, double> edge_cache;
	array_map<datalog_term, array<unsigned int>*> edge_root_probabilities;
	array<unsigned int>* unseen_edge_root_probabilities;

	hdp<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> constant_hdp;
	hdp_sampler<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> constant_sampler;
	cache<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> constant_cache;
	array_map<unsigned int, array<unsigned int>*> constant_root_probabilities;
	array<unsigned int>* unseen_constant_root_probabilities;

	std::mutex edge_hdp_lock;
	std::mutex constant_hdp_lock;

	double termination_posterior;
	double empty_variable_posterior;

	static constexpr unsigned int EDGE_HDP_DEPTH = 2;
	static constexpr unsigned int CONSTANT_HDP_DEPTH = 1;

	datalog_prior(
			unsigned int predicate_count, unsigned int function_count, unsigned int constant_count,
			const double* edge_hdp_alpha, const double* constant_hdp_alpha) :
		edge_hdp(std::array<unsigned int, EDGE_HDP_DEPTH>({{predicate_count, function_count}}), edge_hdp_alpha, EDGE_HDP_DEPTH + 1),
		edge_sampler(edge_hdp), edge_cache(edge_sampler), edge_root_probabilities(256), unseen_edge_root_probabilities(NULL),
		constant_hdp(constant_count, constant_hdp_alpha, CONSTANT_HDP_DEPTH + 1), constant_sampler(constant_hdp),
		constant_cache(constant_sampler), constant_root_probabilities(256), unseen_constant_root_probabilities(NULL),
		edge_posterior_cache(1024), constant_posterior_cache(1024)
	{ }

	~datalog_prior() {
		for (auto entry : edge_root_probabilities)
			cleanup_root_probabilities(entry.value, edge_sampler.posterior.length);
		for (auto entry : constant_root_probabilities)
			cleanup_root_probabilities(entry.value, constant_sampler.posterior.length);
		for (auto entry : edge_posterior_cache)
			core::free(entry.key);
		for (auto entry : constant_posterior_cache)
			core::free(entry.key);
		if (unseen_edge_root_probabilities != NULL)
			cleanup_root_probabilities(unseen_edge_root_probabilities, edge_sampler.posterior.length);
		if (unseen_constant_root_probabilities != NULL)
			cleanup_root_probabilities(unseen_constant_root_probabilities, constant_sampler.posterior.length);
	}

	inline unsigned int predicate_count() const {
		return edge_hdp.pi.predicate_count;
	}

	bool train(const datalog_expression_root* const* examples, unsigned int length,
			const datalog_expression_root* const* constants, unsigned int constants_length,
			unsigned int burn_in, unsigned int iterations, unsigned int skip)
	{
		hash_set<datalog_term> edge_observations(256);
		hash_set<unsigned int> constant_observations(256);
		for (unsigned int i = 0; i < length; i++) {
			if (!add_training_example(examples[i]->root, edge_observations, constant_observations))
				return false;
		}

		/* add the constants */
		for (unsigned int i = 0; i < constants_length; i++) {
			if (constants[i]->root.type != DATALOG_PREDICATE || constants[i]->root.pred.args[0] == NULL || constants[i]->root.pred.args[0]->type != DATALOG_CONSTANT) {
				fprintf(stderr, "datalog_prior.add_constants ERROR: Logical form is not a predicate with a constant argument.\n");
				return false;
			}
			const datalog_predicate& pred = constants[i]->root.pred;
			unsigned int constant = (pred.args[0] == NULL) ? DATALOG_LABEL_EMPTY : pred.args[0]->constant.label;
			if (!add(constant_sampler, &pred.function, CONSTANT_HDP_DEPTH, constant, constant_cache)
			 || !constant_observations.add(constant))
				return false;
		}

		/* perform MCMC on the edge semantics model */
		prepare_sampler(edge_sampler, edge_cache);
		prepare_sampler(constant_sampler, constant_cache);
		for (unsigned int i = 0; i < burn_in; i++) {
			sample_hdp<true>(edge_sampler, edge_cache);
			sample_hdp<true>(constant_sampler, constant_cache);
		}
		for (unsigned int i = 0; i < iterations; i++) {
			sample_hdp<true>(edge_sampler, edge_cache);
			sample_hdp<true>(constant_sampler, constant_cache);
			if (i % skip == 0 && (!edge_sampler.add_sample() || !constant_sampler.add_sample()))
				return false;
		}

		/* precompute the root probabilities for the edge hdp */
		for (datalog_term& observation : edge_observations)
			if (!edge_root_probabilities.put(observation, edge_cache.compute_root_probabilities(edge_sampler, observation)))
				return false;
		unseen_edge_root_probabilities = edge_cache.compute_root_probabilities(edge_sampler, {0, 0});
		if (unseen_edge_root_probabilities == NULL) return false;

		/* precompute the root probabilities for the constant hdp */
		for (unsigned int& observation : constant_observations)
			if (!constant_root_probabilities.put(observation, constant_cache.compute_root_probabilities(constant_sampler, observation)))
				return false;
		unseen_constant_root_probabilities = constant_cache.compute_root_probabilities(constant_sampler, 0u);
		if (unseen_constant_root_probabilities == NULL) return false;

		if (edge_root_probabilities.size > 1)
			sort(edge_root_probabilities.keys, edge_root_probabilities.values, edge_root_probabilities.size);
		if (constant_root_probabilities.size > 1)
			sort(constant_root_probabilities.keys, constant_root_probabilities.values, constant_root_probabilities.size);

		datalog_term_set empty_edge = datalog_term_set(DATALOG_LABEL_EMPTY, NULL, 0, DATALOG_LABEL_EMPTY, NULL, 0);
		datalog_term_set any_edge = datalog_term_set(DATALOG_LABEL_WILDCARD, NULL, 0, DATALOG_LABEL_WILDCARD, NULL, 0);
		termination_posterior = max_edge_posterior(empty_edge, any_edge);
		empty_variable_posterior = max_edge_posterior(empty_edge, empty_edge);
		return true;
	}

	template<bool CompleteContext>
	inline double log_probability(const datalog_expression_root& example) {
		double score = 0.0;
		bool any_edge_insertable = false;
		array<datalog_term_set_path> paths(8);
		if (example.root.type == DATALOG_ANY || example.root.type == DATALOG_NON_EMPTY) {
			return 0.0;
		} else if (example.root.type == DATALOG_TUPLE) {
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this tuple */
			if (!compute_paths<CompleteContext, !CompleteContext>(example.root.tuple, paths, score, any_edge_insertable)) exit(EXIT_FAILURE);
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
		} else if (example.root.type == DATALOG_FUNCTION) {
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this function */
			if (!compute_paths<CompleteContext>(example.root.func, paths, score, any_edge_insertable)) exit(EXIT_FAILURE);
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
		} else if (example.root.type == DATALOG_PREDICATE) {
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this one */
			if (!compute_paths<CompleteContext, !CompleteContext, false>(example.root.pred, paths, score, any_edge_insertable)) exit(EXIT_FAILURE);
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
		} else if (example.root.type == DATALOG_CONSTANT || example.root.type == DATALOG_INTEGER || example.root.type == DATALOG_EMPTY) {
			if (CompleteContext)
				return -std::numeric_limits<double>::infinity();
			else return 0.0;
		} else {
			return -std::numeric_limits<double>::infinity();
		}

		static constexpr datalog_term_set empty_edge = datalog_term_set(DATALOG_LABEL_EMPTY, NULL, 0, DATALOG_LABEL_EMPTY, NULL, 0);
		if (CompleteContext) {
			if (paths.length > 0 && paths[0].not_empty)
				score += max_edge_posterior(paths[0].first, empty_edge);
			for (unsigned int i = 0; i < paths.length; i++) {
				const datalog_term_set_path& edge_path = paths[i];
				if (edge_path.not_empty) {
					if (i > 0 && !edge_path.has_first) {
						score = -std::numeric_limits<double>::infinity();
						return score;
					}
					if (edge_path.last.edge.predicate == 0)
						score += empty_variable_posterior;
					else score += max_edge_posterior(empty_edge, edge_path.last);
				}
			}
		} else {
			/* TODO: we commented this to make this prior separable (factorizable) */
			//static constexpr datalog_term_set any_edge = datalog_term_set(DATALOG_LABEL_WILDCARD, NULL, 0, DATALOG_LABEL_WILDCARD, NULL, 0);

			/* TODO: we can tighten this bound further by considering the head variables of every subgraph */
			/*unsigned int first;
			for (first = 0; first < paths.length; first++)
				if (paths[first].not_empty) break;

			if (first < paths.length && paths[first].first.edge.predicate != 0)
				score += max_edge_posterior(paths[first].first, any_edge);
			for (unsigned int i = first; i < paths.length; i++) {
				const datalog_term_set_path& edge_path = paths[i];
				if (edge_path.not_empty) score += termination_posterior;
			}*/
		}
		return score;
	}

private:
	struct datalog_term_path {
		/* while the full path would be represented as array<datalog_path>,
		   we only need the first and last element to compute the required
		   conditional probabilities */
		datalog_term first;
		datalog_term last;
		bool has_first; /* indicates whether 'first' refers to the first or second edge on the path */
	};

	struct datalog_term_set_path {
		/* while the full path would be represented as array<datalog_path>,
		   we only need the first and last element to compute the required
		   conditional probabilities */
		datalog_term_set first;
		datalog_term_set last;
		bool has_first; /* indicates whether 'first' refers to the first or second edge on the path */
		bool not_empty;
	};

	/* TODO: remove this if unneeded, as well as the cache hash_map definitions below */
	struct cache_item {
		double log_probability;
		array<datalog_term_set_path> paths;

		static inline void free(cache_item& item) {
			core::free(item.paths);
		}
	};

	struct edge_cache_key {
		datalog_term_set observation;
		datalog_term_set prev;

		inline bool operator == (const edge_cache_key& other) const {
			return observation == other.observation && prev == other.prev;
		}

		static inline unsigned int hash(const edge_cache_key& key) {
			return 8191 * datalog_term_set::hash(key.prev) + datalog_term_set::hash(key.observation);
		}

		static inline bool is_empty(const edge_cache_key& key) {
			return datalog_term_set::is_empty(key.observation);
		}

		static inline void move(const edge_cache_key& src, edge_cache_key& dst) {
			core::move(src.observation, dst.observation);
			core::move(src.prev, dst.prev);
		}

		static inline void free(edge_cache_key& key) {
			core::free(key.observation);
			core::free(key.prev);
		}
	};

	inline bool init(edge_cache_key& key, const datalog_term_set& observation, const datalog_term_set& prev) {
		if (!::init(key.observation, observation)) return false;
		else if (!::init(key.prev, prev)) {
			core::free(key.observation);
			return false;
		}
		return true;
	}

	struct constant_cache_key {
		datalog_literal observation;
		datalog_literal prev;

		inline bool operator == (const constant_cache_key& other) const {
			return observation == other.observation && prev == other.prev;
		}

		static inline unsigned int hash(const constant_cache_key& key) {
			return 8191 * datalog_literal::hash(key.prev) + datalog_literal::hash(key.observation);
		}

		static inline bool is_empty(const constant_cache_key& key) {
			return datalog_literal::is_empty(key.observation);
		}

		static inline void move(const constant_cache_key& src, constant_cache_key& dst) {
			core::move(src.observation, dst.observation);
			core::move(src.prev, dst.prev);
		}

		static inline void free(constant_cache_key& key) {
			core::free(key.observation);
			core::free(key.prev);
		}
	};

	inline bool init(constant_cache_key& key, const datalog_literal& observation, const datalog_literal& prev) {
		if (!::init(key.observation, observation)) return false;
		else if (!::init(key.prev, prev)) {
			core::free(key.observation);
			return false;
		}
		return true;
	}

	hash_map<edge_cache_key, double> edge_posterior_cache;
	hash_map<constant_cache_key, double> constant_posterior_cache;

	inline bool add_training_example(
			const datalog_expression& expression,
			hash_set<datalog_term>& edge_observations,
			hash_set<unsigned int>& constant_observations)
	{
		array<datalog_term_path> paths(64);
		if (!compute_paths(expression, paths, edge_observations, constant_observations)) {
			return false;
		} else if (paths.length == 0 || paths[0].has_first) {
			fprintf(stderr, "datalog_prior.add_training_example ERROR: Incorrect root vertex in variable graph.\n");
			return false;
		}
		for (unsigned int j = 1; j < paths.length; j++) {
			if (!paths[j].has_first) {
				fprintf(stderr, "datalog_prior.add_training_example ERROR: Variable graph is disconnected.\n");
				return false;
			}
		}

		/* add the first and terminating "empty" edges */
		unsigned int path[] = {DATALOG_LABEL_EMPTY, DATALOG_LABEL_EMPTY};
		if (paths[0].first.predicate != 0) {
			if (!add(edge_sampler, path, EDGE_HDP_DEPTH, paths[0].first, edge_cache)
			 || !edge_observations.add(paths[0].first)) return false;
		}
		datalog_term empty_edge = {DATALOG_LABEL_EMPTY, DATALOG_LABEL_EMPTY};
		for (const datalog_term_path& edge_path : paths) {
			if (edge_path.last.predicate == 0) {
				path[0] = DATALOG_LABEL_EMPTY; path[1] = DATALOG_LABEL_EMPTY;
			} else {
				path[0] = edge_path.last.predicate;
				path[1] = edge_path.last.arg_position;
			}
			if (!add(edge_sampler, path, EDGE_HDP_DEPTH, empty_edge, edge_cache)) return false;
		}
		return edge_observations.add(empty_edge);
	}

	template<unsigned int ArgPosition>
	inline bool add_parent_edge(unsigned int variable,
			unsigned int predicate, array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations)
	{
		if (!paths.ensure_capacity(variable)) return false;
		while (paths.length < variable) {
			paths[paths.length].first.zero();
			paths[paths.length].last.zero();
			paths[paths.length].has_first = false;
			paths.length++;
		}

		if (!paths[variable - 1].has_first) {
			datalog_term new_edge = {predicate, ArgPosition};
			if (paths[variable - 1].first.predicate != 0) {
				unsigned int path[] = { predicate, ArgPosition };
				if (!add(edge_sampler, path, EDGE_HDP_DEPTH, paths[variable - 1].first, edge_cache)
				 || !edge_observations.add(new_edge)) return false;
			}
			paths[variable - 1].first = new_edge;
			if (paths[variable - 1].last.predicate == 0)
				paths[variable - 1].last = new_edge;
			paths[variable - 1].has_first = true;
		} else {
			datalog_term new_edge = {predicate, parent_arg_position<ArgPosition>()};
			if (paths[variable - 1].last.predicate != 0) {
				unsigned int path[] = { paths[variable - 1].last.predicate, paths[variable - 1].last.arg_position };
				if (!add(edge_sampler, path, EDGE_HDP_DEPTH, new_edge, edge_cache)
				 || !edge_observations.add(new_edge)) return false;
			}
			paths[variable - 1].last = new_edge;
		}
		return true;
	}

	inline bool add_child_edge(unsigned int variable,
			unsigned int predicate, array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations, unsigned int position)
	{
		if (!paths.ensure_capacity(variable)) return false;
		while (paths.length < variable) {
			paths[paths.length].first.zero();
			paths[paths.length].last.zero();
			paths[paths.length].has_first = false;
			paths.length++;
		}

		datalog_term new_edge = {predicate, position};
		if (paths[variable - 1].last.predicate != 0) {
			unsigned int path[] = { paths[variable - 1].last.predicate, paths[variable - 1].last.arg_position };
			if (!add(edge_sampler, path, EDGE_HDP_DEPTH, new_edge, edge_cache)
			 || !edge_observations.add(new_edge)) return false;
		}
		paths[variable - 1].last = new_edge;
		if (paths[variable - 1].first.predicate == 0)
			paths[variable - 1].first = new_edge;
		return true;
	}

	bool compute_paths(const datalog_predicate& pred,
			array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations,
			hash_set<unsigned int>& constant_observations)
	{
		if (pred.args[0] == NULL) {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a predicate with no arguments.\n");
			return false;
		} else if (pred.args[0]->type == DATALOG_VARIABLE) {
			unsigned int first = pred.args[0]->variable;
			if (pred.args[1] == NULL) {
				if (pred.args[2] != NULL) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a ternary predicate whose second argument is empty.\n");
					return false;
				}

				/* this is a unary predicate */
				return add_child_edge(first, pred.function, paths, edge_observations, ARG_POSITION_UNARY);
			} else if (pred.args[1]->type == DATALOG_VARIABLE) {
				if (pred.args[2] != NULL) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a ternary predicate whose second argument is not an integer.\n");
					return false;
				}
				unsigned int second = pred.args[1]->variable;
				if (second < first) {
					/* second is a parent of first */
					return add_parent_edge<ARG_POSITION_FIRST>(first, pred.function, paths, edge_observations)
						&& add_child_edge(second, pred.function, paths, edge_observations, ARG_POSITION_SECOND);
				} else if (second > first) {
					/* first is a parent of second */
					return add_parent_edge<ARG_POSITION_SECOND>(second, pred.function, paths, edge_observations)
						&& add_child_edge(first, pred.function, paths, edge_observations, ARG_POSITION_FIRST);
				} else {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a reflexive binary edge.\n");
					return false;
				}
			} else if (pred.args[1]->type == DATALOG_PREDICATE) {
				if (pred.args[2] != NULL) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a ternary predicate whose second argument is not an integer.\n");
					return false;
				} else if (pred.function != PREDICATE_CONST && (pred.function != DATALOG_LABEL_WILDCARD || pred.is_excluded(PREDICATE_CONST))) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a non-const predicate with a predicate argument.\n");
					return false;
				}

				/* make sure the arguments of the predicate are constants */
				datalog_predicate& internal_predicate = pred.args[1]->pred;
				if ((internal_predicate.args[0] != NULL && internal_predicate.args[0]->type != DATALOG_CONSTANT)
				 || (internal_predicate.args[1] != NULL && internal_predicate.args[1]->type != DATALOG_CONSTANT)) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found an internal predicate without constant arguments.\n");
					return false;
				}
				unsigned int constant = (internal_predicate.args[0] == NULL) ? DATALOG_LABEL_EMPTY : internal_predicate.args[0]->constant.label;
				return add_child_edge(first, pred.function, paths, edge_observations, pred.args[1]->pred.function)
					&& add(constant_sampler, &pred.args[1]->pred.function, CONSTANT_HDP_DEPTH, constant, constant_cache)
					&& constant_observations.add(constant);
			} else if (pred.args[1]->type == DATALOG_CONSTANT) {
				if (pred.args[2] != NULL) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a ternary predicate whose second argument is not an integer.\n");
					return false;
				}

				return add_child_edge(first, pred.function, paths, edge_observations, pred.args[1]->constant.label);
			} else if (pred.args[1]->type == DATALOG_INTEGER) {
				if (pred.args[2] != NULL && pred.args[2]->type != DATALOG_CONSTANT) {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a ternary predicate whose third argument is not a constant.\n");
					return false;
				}

				unsigned int constant = (pred.args[2] == NULL) ? DATALOG_LABEL_EMPTY : pred.args[2]->constant.label;
				return add_child_edge(first, pred.function, paths, edge_observations, ARG_POSITION_INTEGER)
					&& add(constant_sampler, &pred.function, CONSTANT_HDP_DEPTH, constant, constant_cache)
					&& constant_observations.add(constant);
			} else {
				fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a predicate"
						" whose second argument is not a variable or a predicate.\n");
				return false;
			}
		} else {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a "
					"predicate whose first argument is not a variable.\n");
			return false;
		}
	}

	inline bool compute_paths(
			const datalog_function& func,
			array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations,
			hash_set<unsigned int>& constant_observations)
	{
		unsigned int head;
		switch (func.function) {
		case PREDICATE_COUNT:
		case PREDICATE_SUM:
			if (func.vars[0] == 0 || func.vars[1] == 0 || func.vars[0] != func.vars[1] + 1) {
				fprintf(stderr, "compute_paths ERROR: Unexpected variables for higher-order function.\n");
				return false;
			}
			head = func.vars[0];
			if (!add_parent_edge<ARG_POSITION_UNARY>(head, func.function, paths, edge_observations)) return false;
			break;

		case PREDICATE_HIGHEST:
		case PREDICATE_LOWEST:
		case PREDICATE_LONGEST:
		case PREDICATE_SHORTEST:
		case PREDICATE_LARGEST:
		case PREDICATE_SMALLEST:
			if (func.vars[0] == 0 || func.vars[1] != 0) {
				fprintf(stderr, "compute_paths ERROR: Unexpected variables for higher-order function.\n");
				return false;
			}
			head = func.vars[0];
			if (head != 0 && !add_child_edge(head, func.function, paths, edge_observations, ARG_POSITION_FIRST)) return false;
			break;

		case PREDICATE_MOST:
		case PREDICATE_FEWEST:
			if (func.vars[0] == 0 || func.vars[1] == 0 || func.vars[0] + 1 != func.vars[1]) {
				fprintf(stderr, "compute_paths ERROR: Unexpected variables for higher-order function.\n");
				return false;
			}
			head = func.vars[0];
			if (!add_child_edge(head, func.function, paths, edge_observations, ARG_POSITION_FIRST))
				return false;
			break;

		case PREDICATE_NOT:
			head = get_head(*func.arg);
			if (head == DATALOG_LABEL_EMPTY || head == DATALOG_LABEL_WILDCARD) {
				fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a negation scope with no definite head variable.\n");
				return false;
			} else if (!add_child_edge(head, func.function, paths, edge_observations, ARG_POSITION_FIRST)) {
				return false;
			}
			break;

		case PREDICATE_ANSWER:
		default:
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Unrecognized datalog_function type.\n");
			return false;
		}

		if (func.arg->type == DATALOG_PREDICATE) {
			if (!compute_paths(func.arg->pred, paths, edge_observations, constant_observations)) return false;
		} else if (func.arg->type == DATALOG_TUPLE) {
			if (!compute_paths(func.arg->tuple, paths, edge_observations, constant_observations)) return false;
		} else if (func.arg->type == DATALOG_FUNCTION) {
			if (!compute_paths(func.arg->func, paths, edge_observations, constant_observations)) return false;
		} else {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a higher-order function"
					" with an argument that is not a predicate, tuple, or another higher-order function.\n");
			return false;
		}

		return add_child_edge(head, DATALOG_LABEL_EMPTY, paths, edge_observations, DATALOG_LABEL_EMPTY);
	}

	bool compute_paths(const datalog_tuple& tuple,
			array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations,
			hash_set<unsigned int>& constant_observations)
	{
		for (unsigned int i = 0; i < tuple.elements.length; i++) {
			switch (tuple.elements[i]->type) {
			case DATALOG_PREDICATE:
				if (!compute_paths(tuple.elements[i]->pred, paths, edge_observations, constant_observations)) return false;
				break;
			case DATALOG_FUNCTION:
				if (!compute_paths(tuple.elements[i]->func, paths, edge_observations, constant_observations)) return false;
				break;
			default:
				fprintf(stderr, "datalog_prior.compute_paths ERROR: Unexpected expression type in tuple.\n");
				return false;
			}
		}
		return true;
	}

	bool compute_paths(const datalog_expression& root,
			array<datalog_term_path>& paths,
			hash_set<datalog_term>& edge_observations,
			hash_set<unsigned int>& constant_observations)
	{
		if (root.type != DATALOG_FUNCTION || root.func.function != PREDICATE_ANSWER
		 || root.func.vars[0] != 1 || root.func.vars[1] != 0) {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Expected an 'answer' function at the root.\n");
			return false;
		} else if (root.func.arg->type == DATALOG_FUNCTION) {
			return compute_paths(root.func.arg->func, paths, edge_observations, constant_observations);
		} else if (root.func.arg->type == DATALOG_TUPLE) {
			return compute_paths(root.func.arg->tuple, paths, edge_observations, constant_observations);
		} else if (root.func.arg->type == DATALOG_PREDICATE) {
			return compute_paths(root.func.arg->pred, paths, edge_observations, constant_observations);
		} else {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: The argument of"
					" the root function should be either a function, tuple, or predicate.\n");
			return false;
		}
	}

	inline bool is_excluded(const datalog_term& edge, const datalog_term_set& set) {
		if (set.edge.predicate == DATALOG_LABEL_WILDCARD) {
			if (index_of(edge.predicate, set.excluded_predicates, set.excluded_predicate_count) < set.excluded_predicate_count)
				return true;
		} else if (set.edge.predicate != edge.predicate) return true;

		if (set.edge.arg_position == DATALOG_LABEL_WILDCARD) {
			if (index_of(edge.arg_position, set.excluded_functions, set.excluded_function_count) < set.excluded_function_count)
				return true;
		} else if (set.edge.arg_position != edge.arg_position) return true;

		return false;
	}

	inline bool is_excluded(unsigned int constant, const datalog_literal& set) {
		if (set.label == DATALOG_LABEL_WILDCARD) {
			if (index_of(constant, set.excluded, set.excluded_count) < set.excluded_count)
				return true;
		} else if (set.label != constant) return true;
		return false;
	}

	inline double max_edge_posterior(const datalog_term& observation, const unsigned int* path,
			const unsigned int* const* excluded, const unsigned int* excluded_count,
			const array<unsigned int>* root_probabilities)
	{
		array<weighted_feature_set<double>> paths = array<weighted_feature_set<double>>(8);
		predict(edge_sampler, observation, path, excluded, excluded_count, paths, root_probabilities);

		double score = max(paths);
		for (weighted_feature_set<double>& path : paths)
			free(path);
		return score;
	}

	inline double max_edge_posterior_helper(
			const datalog_term_set& observation,
			const datalog_term_set& prev)
	{
		if (observation.edge.arg_position == ARG_POSITION_FIRST_OR_SECOND) {
			return max(
					max_edge_posterior_helper(
						datalog_term_set(observation.edge.predicate, observation.excluded_predicates,
						observation.excluded_predicate_count, ARG_POSITION_FIRST), prev),
					max_edge_posterior_helper(
						datalog_term_set(observation.edge.predicate, observation.excluded_predicates,
						observation.excluded_predicate_count, ARG_POSITION_SECOND), prev));
		} else if (observation.edge.arg_position == ARG_POSITION_FIRST_OR_SECOND_PARENT) {
			return max(
					max_edge_posterior_helper(
						datalog_term_set(observation.edge.predicate, observation.excluded_predicates,
						observation.excluded_predicate_count, ARG_POSITION_FIRST_PARENT), prev),
					max_edge_posterior_helper(
						datalog_term_set(observation.edge.predicate, observation.excluded_predicates,
						observation.excluded_predicate_count, ARG_POSITION_SECOND_PARENT), prev));
		} else if (prev.edge.arg_position == ARG_POSITION_FIRST_OR_SECOND) {
			return max(
					max_edge_posterior_helper(observation,
						datalog_term_set(prev.edge.predicate, prev.excluded_predicates,
						prev.excluded_predicate_count, ARG_POSITION_FIRST)),
					max_edge_posterior_helper(observation,
						datalog_term_set(prev.edge.predicate, prev.excluded_predicates,
						prev.excluded_predicate_count, ARG_POSITION_SECOND)));
		} else if (prev.edge.arg_position == ARG_POSITION_FIRST_OR_SECOND_PARENT) {
			return max(
					max_edge_posterior_helper(observation,
						datalog_term_set(prev.edge.predicate, prev.excluded_predicates,
						prev.excluded_predicate_count, ARG_POSITION_FIRST_PARENT)),
					max_edge_posterior_helper(observation,
						datalog_term_set(prev.edge.predicate, prev.excluded_predicates,
						prev.excluded_predicate_count, ARG_POSITION_SECOND_PARENT)));
		} else {
			unsigned int path[] = { prev.edge.predicate, prev.edge.arg_position };
			const unsigned int* excluded[] = { prev.excluded_predicates, prev.excluded_functions };
			unsigned int excluded_counts[] = { prev.excluded_predicate_count, prev.excluded_function_count };

			double posterior;
			if (observation.edge.predicate == DATALOG_LABEL_WILDCARD || observation.edge.arg_position == DATALOG_LABEL_WILDCARD) {
				posterior = max_edge_posterior({0, 0}, path, excluded, excluded_counts, unseen_edge_root_probabilities);
				for (const auto& entry : edge_root_probabilities) {
					if (!is_excluded(entry.key, observation))
						posterior = max(posterior, max_edge_posterior(entry.key, path, excluded, excluded_counts, entry.value));
				}
			} else {
				unsigned int index = edge_root_probabilities.index_of(observation.edge);
				const array<unsigned int>* root_probabilities =
						(index < edge_root_probabilities.size) ? edge_root_probabilities.values[index] : unseen_edge_root_probabilities;
				posterior = max_edge_posterior(observation.edge, path, excluded, excluded_counts, root_probabilities);
			}
			return posterior;
		}
	}

	inline double max_edge_posterior(
			const datalog_term_set& observation,
			const datalog_term_set& prev)
	{
		edge_hdp_lock.lock();
		if (!edge_posterior_cache.check_size()) exit(EXIT_FAILURE);

		bool contains; unsigned int bucket;
		double posterior = edge_posterior_cache.get({observation, prev}, contains, bucket);
		edge_hdp_lock.unlock();
		if (contains) return posterior;

		posterior = max_edge_posterior_helper(observation, prev);

		edge_hdp_lock.lock();
		edge_posterior_cache.get({observation, prev}, contains, bucket);
		if (!contains) {
			if (!init(edge_posterior_cache.table.keys[bucket], observation, prev)) exit(EXIT_FAILURE);
			edge_posterior_cache.values[bucket] = posterior;
			edge_posterior_cache.table.size++;
		}
		edge_hdp_lock.unlock();
		return posterior;
	}

	inline double max_constant_posterior(unsigned int observation, unsigned int prev,
			const unsigned int* excluded, unsigned int excluded_count,
			const array<unsigned int>* root_probabilities)
	{
		array<weighted_feature_set<double>> paths = array<weighted_feature_set<double>>(8);
		predict(constant_sampler, observation, &prev, &excluded, &excluded_count, paths, root_probabilities);

		double score = max(paths);
		for (weighted_feature_set<double>& path : paths)
			free(path);
		return score;
	}

	inline double max_constant_posterior(
			const datalog_literal& observation,
			const datalog_literal& prev)
	{
		constant_hdp_lock.lock();
		if (!constant_posterior_cache.check_size()) exit(EXIT_FAILURE);

		bool contains; unsigned int bucket;
		double& posterior = constant_posterior_cache.get({observation, prev}, contains, bucket);
		constant_hdp_lock.unlock();
		if (contains) return posterior;

		if (observation.label == DATALOG_LABEL_WILDCARD) {
			posterior = max_constant_posterior(0, prev.label, prev.excluded, prev.excluded_count, unseen_constant_root_probabilities);
			for (const auto& entry : constant_root_probabilities) {
				if (!is_excluded(entry.key, observation))
					posterior = max(posterior, max_constant_posterior(entry.key, prev.label, prev.excluded, prev.excluded_count, entry.value));
			}
		} else {
			unsigned int index = constant_root_probabilities.index_of(observation.label);
			const array<unsigned int>* root_probabilities =
					(index < constant_root_probabilities.size) ? constant_root_probabilities.values[index] : unseen_constant_root_probabilities;
			posterior = max_constant_posterior(observation.label, prev.label, prev.excluded, prev.excluded_count, root_probabilities);
		}

		constant_hdp_lock.lock();
		if (!init(constant_posterior_cache.table.keys[bucket], observation, prev)) exit(EXIT_FAILURE);
		constant_posterior_cache.table.size++;
		constant_hdp_lock.unlock();
		return posterior;
	}

	inline bool ensure_variable_capacity(array<datalog_term_set_path>& paths, unsigned int variable) {
		if (!paths.ensure_capacity(variable)) return false;
		while (paths.length < variable) {
			paths[paths.length].first.zero();
			paths[paths.length].last.zero();
			paths[paths.length].has_first = false;
			paths[paths.length].not_empty = false;
			paths.length++;
		}
		return true;
	}

	template<bool CompleteContext, unsigned int ArgPosition>
	bool add_parent_edge(unsigned int variable,
			unsigned int predicate, unsigned int* excluded_predicates, unsigned int excluded_predicate_count,
			array<datalog_term_set_path>& paths, double& log_probability, bool& any_edge_insertable)
	{
		if (!ensure_variable_capacity(paths, variable)) return false;
		if (!paths[variable - 1].not_empty && any_edge_insertable) {
			paths[variable - 1].first.any();
			paths[variable - 1].last.any();
		}

		datalog_term_set new_edge = datalog_term_set(predicate, excluded_predicates, excluded_predicate_count, ArgPosition);
		if (!paths[variable - 1].has_first) {
			/* if the context is incomplete, this edge may not actually be the first */
			if (paths[variable - 1].not_empty) {
				double score = 0.0;
				if (paths[variable - 1].first.edge.predicate != 0) {
					score = max_edge_posterior(paths[variable - 1].first, new_edge);
					if (paths[variable - 1].last.edge.predicate != 0 && !CompleteContext) {
						score = max(score, max_edge_posterior(new_edge, paths[variable - 1].last));
						if (ArgPosition == ARG_POSITION_FIRST || ArgPosition == ARG_POSITION_SECOND || ArgPosition == ARG_POSITION_FIRST_OR_SECOND) {
							datalog_term_set new_parent_edge = datalog_term_set(predicate, excluded_predicates,
									excluded_predicate_count, parent_arg_position<ArgPosition>());
							score = max(score, max_edge_posterior(new_parent_edge, paths[variable - 1].last));
						}
					}
				} else if (paths[variable - 1].last.edge.predicate != 0 && !CompleteContext)
					score = max_edge_posterior(new_edge, paths[variable - 1].last);
				log_probability += score;
			}

			if (CompleteContext) {
				paths[variable - 1].first = new_edge;
				if (paths[variable - 1].last.edge.predicate == 0)
					paths[variable - 1].last = new_edge;
			} else {
				paths[variable - 1].first.any();
				paths[variable - 1].last.any();
			}
			paths[variable - 1].has_first = true;
		} else {
			if (ArgPosition != ARG_POSITION_FIRST && ArgPosition != ARG_POSITION_SECOND && ArgPosition != ARG_POSITION_FIRST_OR_SECOND) {
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			}
			datalog_term_set new_edge = datalog_term_set(predicate, excluded_predicates,
					excluded_predicate_count, parent_arg_position<ArgPosition>());
			if (paths[variable - 1].last.edge.predicate != 0)
				log_probability += max_edge_posterior(new_edge, paths[variable - 1].last);
			paths[variable - 1].last = new_edge;
		}
		paths[variable - 1].not_empty = true;
		return true;
	}

	inline bool add_child_edge(
			unsigned int variable, unsigned int predicate, unsigned int* excluded_predicates, unsigned int excluded_predicate_count,
			array<datalog_term_set_path>& paths, double& log_probability, bool any_edge_insertable,
			unsigned int function, unsigned int* excluded_functions = NULL, unsigned int excluded_function_count = 0)
	{
		if (!ensure_variable_capacity(paths, variable)) return false;
		if (!paths[variable - 1].not_empty && any_edge_insertable) {
			paths[variable - 1].first.any();
			paths[variable - 1].last.any();
		}

		datalog_term_set new_edge = datalog_term_set(
				predicate, excluded_predicates, excluded_predicate_count,
				function, excluded_functions, excluded_function_count);
		if (paths[variable - 1].not_empty && paths[variable - 1].last.edge.predicate != 0)
			log_probability += max_edge_posterior(new_edge, paths[variable - 1].last);
		paths[variable - 1].last = new_edge;
		if (paths[variable - 1].first.edge.predicate == 0)
			paths[variable - 1].first = new_edge;
		paths[variable - 1].not_empty = true;
		return true;
	}

	inline void set_any_edges(array<datalog_term_set_path>& paths, bool& any_edge_insertable) {
		for (datalog_term_set_path& path : paths) {
			if (path.not_empty) {
				path.first.any();
				path.last.any();
			}
		}
		any_edge_insertable = true;
	}

	template<bool CompleteContext, bool Flippable, bool IsBooleanValued = true>
	bool compute_paths(const datalog_predicate& pred,
			array<datalog_term_set_path>& paths,
			double& log_probability, bool& any_edge_insertable)
	{
		if (IsBooleanValued && CompleteContext && pred.function == DATALOG_LABEL_WILDCARD) {
			log_probability = -std::numeric_limits<double>::infinity();
			return true;
		}
		if (pred.args[0] == NULL) {
			if (IsBooleanValued) {
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			}
			/* due to the delete_argx functions, this argument can ultimately become any variable */
			set_any_edges(paths, any_edge_insertable); return true;
		} else if (pred.args[0]->type == DATALOG_VARIABLE) {
			unsigned int first = pred.args[0]->variable;
			if (pred.args[1] == NULL) {
				if (pred.args[2] != NULL) {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				if (IsBooleanValued) {
					/* this is a unary predicate */
					return add_child_edge(first, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable, ARG_POSITION_UNARY);
				} else {
					/* due to the delete_argx functions, this argument can ultimately become any variable */
					set_any_edges(paths, any_edge_insertable); return true;
				}
			} else if (pred.args[1]->type == DATALOG_VARIABLE) {
				if (pred.args[2] != NULL) {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				unsigned int second = pred.args[1]->variable;
				if (second < first) {
					/* second is a parent of first */
					bool not_empty = false; //paths[first - 1].not_empty;
					if (!add_parent_edge<CompleteContext, Flippable ? ARG_POSITION_FIRST_OR_SECOND : ARG_POSITION_FIRST>(
								first, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable))
						return false;
					if (not_empty) return true;
					return add_child_edge(second, pred.function, pred.excluded, pred.excluded_count, paths,
							log_probability, any_edge_insertable, Flippable ? ARG_POSITION_FIRST_OR_SECOND : ARG_POSITION_SECOND);
				} else if (second > first) {
					/* first is a parent of second */
					bool not_empty = false; //paths[second - 1].not_empty;
					if (!add_parent_edge<CompleteContext, Flippable ? ARG_POSITION_FIRST_OR_SECOND : ARG_POSITION_SECOND>(
								second, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable))
						return false;
					if (not_empty) return true;
					return add_child_edge(first, pred.function, pred.excluded, pred.excluded_count, paths,
							log_probability, any_edge_insertable, Flippable ? ARG_POSITION_FIRST_OR_SECOND : ARG_POSITION_FIRST);
				} else {
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a reflexive binary edge.\n");
					return false;
				}
			} else if (pred.args[1]->type == DATALOG_PREDICATE) {
				if (pred.args[2] != NULL
				 || (pred.function != PREDICATE_CONST && (pred.function != DATALOG_LABEL_WILDCARD || pred.is_excluded(PREDICATE_CONST)))) {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				/* make sure the arguments of the predicate are constants */
				datalog_predicate& internal_predicate = pred.args[1]->pred;
				if ((internal_predicate.args[0] != NULL && internal_predicate.args[0]->type != DATALOG_ANY
				  && internal_predicate.args[0]->type != DATALOG_NON_EMPTY && internal_predicate.args[0]->type != DATALOG_CONSTANT)
				 || (internal_predicate.args[1] != NULL && internal_predicate.args[1]->type != DATALOG_ANY
				  && internal_predicate.args[1]->type != DATALOG_NON_EMPTY && internal_predicate.args[1]->type != DATALOG_CONSTANT)) {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				datalog_literal observed_constant;
				if (internal_predicate.args[0] == NULL) {
					observed_constant.label = DATALOG_LABEL_EMPTY; observed_constant.excluded_count = 0;
				} else if (internal_predicate.args[0]->type == DATALOG_ANY || internal_predicate.args[0]->type == DATALOG_NON_EMPTY) {
					observed_constant.label = DATALOG_LABEL_WILDCARD; observed_constant.excluded_count = 0;
				} else {
					observed_constant.label = internal_predicate.args[0]->constant.label;
					observed_constant.excluded = internal_predicate.args[0]->constant.excluded;
					observed_constant.excluded_count = internal_predicate.args[0]->constant.excluded_count;
				}
				log_probability += max_constant_posterior(observed_constant, {pred.args[1]->pred.function, pred.args[1]->pred.excluded, pred.args[1]->pred.excluded_count});
				return add_child_edge(
						first, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable,
						pred.args[1]->pred.function, pred.args[1]->pred.excluded, pred.args[1]->pred.excluded_count);
			} else if (pred.args[1]->type == DATALOG_CONSTANT) {
				if (pred.args[2] != NULL) {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				return add_child_edge(
						first, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable,
						pred.args[1]->constant.label, pred.args[1]->constant.excluded, pred.args[1]->constant.excluded_count);
			} else if (pred.args[1]->type == DATALOG_INTEGER || pred.args[1]->type == DATALOG_ANY || pred.args[1]->type == DATALOG_NON_EMPTY) {
				/* in the current grammar, this argument can only become an integer */
				datalog_literal observed_constant;
				if (pred.args[2] == NULL) {
					if (CompleteContext) {
						observed_constant.label = DATALOG_LABEL_EMPTY; observed_constant.excluded_count = 0;
					} else { /* delete_arg3 allows for the third argument to become any constant */
						observed_constant.label = DATALOG_LABEL_WILDCARD; observed_constant.excluded_count = 0;
					}
				} else if (pred.args[2]->type == DATALOG_ANY || pred.args[2]->type == DATALOG_NON_EMPTY) {
					observed_constant.label = DATALOG_LABEL_WILDCARD; observed_constant.excluded_count = 0;
				} else if (pred.args[2]->type == DATALOG_CONSTANT) {
					observed_constant.label = pred.args[2]->constant.label;
					observed_constant.excluded = pred.args[2]->constant.excluded;
					observed_constant.excluded_count = pred.args[2]->constant.excluded_count;
				} else {
					log_probability = -std::numeric_limits<double>::infinity();
					return true;
				}
				log_probability += max_constant_posterior(observed_constant, {pred.function, pred.excluded, pred.excluded_count});
				return add_child_edge(first, pred.function, pred.excluded, pred.excluded_count, paths, log_probability, any_edge_insertable, ARG_POSITION_INTEGER);
			} else if (pred.args[1]->type == DATALOG_TUPLE) {
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			} else {
				fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a predicate whose "
						"second argument is not a variable, predicate, constant, or integer.\n");
				return false;
			}
		} else if (pred.args[0]->type == DATALOG_CONSTANT) {
			if (IsBooleanValued) {
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			}
			/* we don't know the variable associated with this edge */
			set_any_edges(paths, any_edge_insertable);
			return true;
		} else if (pred.args[0]->type == DATALOG_ANY || pred.args[0]->type == DATALOG_NON_EMPTY) {
			/* the first argument could be any variable */
			set_any_edges(paths, any_edge_insertable);
			return true;
		} else {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a "
					"predicate whose first argument is not a variable.\n");
			return false;
		}
	}

	template<bool CompleteContext>
	inline bool compute_paths(
			const datalog_function& func,
			array<datalog_term_set_path>& paths,
			double& log_probability,
			bool& any_edge_insertable)
	{
		unsigned int head = DATALOG_LABEL_WILDCARD;
		switch (func.function) {
		case PREDICATE_COUNT:
		case PREDICATE_SUM:
			if (func.vars[0] == 0 || func.vars[1] == 0 || func.vars[0] != func.vars[1] + 1) {
				/* some invert functions can cause the two variables here to become non-adjacent, which is disallowed by this prior */
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			}
			head = func.vars[0];
			if (!add_parent_edge<CompleteContext, ARG_POSITION_UNARY>(head, func.function,
					func.excluded, func.excluded_count, paths, log_probability, any_edge_insertable)) return false;
			break;

		case PREDICATE_HIGHEST:
		case PREDICATE_LOWEST:
		case PREDICATE_LONGEST:
		case PREDICATE_SHORTEST:
		case PREDICATE_LARGEST:
		case PREDICATE_SMALLEST:
			if (func.vars[0] == 0 || func.vars[1] != 0) {
				fprintf(stderr, "compute_paths ERROR: Unexpected variables for higher-order function.\n");
				return false;
			}
			head = func.vars[0];
			if (!add_child_edge(head, func.function, func.excluded, func.excluded_count,
					paths, log_probability, any_edge_insertable, ARG_POSITION_FIRST)) return false;
			break;

		case PREDICATE_MOST:
		case PREDICATE_FEWEST:
			if (func.vars[0] == 0 || func.vars[1] == 0 || func.vars[0] + 1 != func.vars[1]) {
				/* some invert functions can cause the two variables here to become non-adjacent, which is disallowed by this prior */
				log_probability = -std::numeric_limits<double>::infinity();
				return true;
			}
			head = func.vars[0];
			if (!add_child_edge(head, func.function, func.excluded, func.excluded_count, paths, log_probability, any_edge_insertable, ARG_POSITION_FIRST))
				return false;
			break;

		case PREDICATE_NOT:
			/* since the head variable is not readily available in this structure, we have to compute it manually */
			if (CompleteContext) {
				head = get_head(*func.arg);
				if (head == DATALOG_LABEL_EMPTY) {
					log_probability = -std::numeric_limits<double>::infinity();
				} else if (head != DATALOG_LABEL_WILDCARD
				 && !add_child_edge(head, func.function, func.excluded, func.excluded_count,
						 paths, log_probability, any_edge_insertable, ARG_POSITION_FIRST))
				{
					return false;
				}
			}
			/* if the expression isn't complete, the head may change via transformations such as delete_left_head_keep_function */
			break;

		case PREDICATE_ANSWER:
		case DATALOG_LABEL_WILDCARD:
			break;

		default:
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Unrecognized datalog_function type.\n");
			return false;
		}

		if (func.arg->type == DATALOG_PREDICATE) {
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this one */
			if (!compute_paths<CompleteContext, false>(func.arg->pred, paths, log_probability, any_edge_insertable)) return false;
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
		} else if (func.arg->type == DATALOG_TUPLE) {
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this tuple */
			if (!compute_paths<CompleteContext, false>(func.arg->tuple, paths, log_probability, any_edge_insertable)) return false;
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
		} else if (func.arg->type == DATALOG_FUNCTION) {
			bool child_edge_insertable = any_edge_insertable;
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable); /* additional terms can be added before or after this function */
			if ((func.function == PREDICATE_ANSWER && !compute_paths<CompleteContext>(func.arg->func, paths, log_probability, any_edge_insertable))
			 || (func.function != PREDICATE_ANSWER && !compute_paths<true>(func.arg->func, paths, log_probability, child_edge_insertable))) return false;
			if (!CompleteContext) set_any_edges(paths, any_edge_insertable);
			if (child_edge_insertable) any_edge_insertable = true;
		} else if (func.arg->type == DATALOG_EMPTY) {
			if (CompleteContext)
				log_probability = -std::numeric_limits<double>::infinity();
		} else if (func.arg->type == DATALOG_ANY || func.arg->type == DATALOG_NON_EMPTY) {
			/* TODO: can any edge really appear in an ANY subexpression? */
			set_any_edges(paths, any_edge_insertable);
		} else {
			fprintf(stderr, "datalog_prior.compute_paths ERROR: Found a higher-order function"
					" with an argument that is not a predicate, tuple, or another higher-order function.\n");
			return false;
		}

		if (!CompleteContext || head == DATALOG_LABEL_WILDCARD || head == DATALOG_LABEL_EMPTY)
			return true;
		return add_child_edge(head, DATALOG_LABEL_EMPTY, NULL, 0, paths,
				log_probability, any_edge_insertable, DATALOG_LABEL_EMPTY);
	}

	template<bool CompleteContext, bool Flippable>
	bool compute_paths(const datalog_tuple& tuple,
			array<datalog_term_set_path>& paths,
			double& log_probability,
			bool& any_edge_insertable)
	{
		if (CompleteContext && tuple.position != POSITION_EXACT) {
			log_probability = -std::numeric_limits<double>::infinity();
		} else {
			/* iterate over the elements in the tuple */
			bool child_edge_insertable = any_edge_insertable;
			for (unsigned int i = 0; i < tuple.elements.length; i++) {
				switch (tuple.elements[i]->type) {
				case DATALOG_PREDICATE:
					if ((i == 0 && !compute_paths<CompleteContext, Flippable>(tuple.elements[i]->pred, paths, log_probability, any_edge_insertable))
					 || (i != 0 && !compute_paths<CompleteContext, false>(tuple.elements[i]->pred, paths, log_probability, any_edge_insertable))) return false;
					break;
				case DATALOG_FUNCTION:
					if (!compute_paths<true>(tuple.elements[i]->func, paths, log_probability, child_edge_insertable)) return false;
					if (child_edge_insertable) any_edge_insertable = true;
					break;
				case DATALOG_ANY:
				case DATALOG_NON_EMPTY:
					/* TODO: can any edge really appear in an ANY subexpression? */
					set_any_edges(paths, any_edge_insertable);
					break;
				default:
					fprintf(stderr, "datalog_prior.compute_paths ERROR: Unexpected expression type in tuple.\n");
					return false;
				}
			}
		}
		return true;
	}
};

#endif /* DATALOG_HDP_H_ */
