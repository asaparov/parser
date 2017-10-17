/**
 * \file inmind_prior.h
 *
 *  <!-- Created on: Aug 9, 2017
 *           Author: asaparov -->
 */

#ifndef INMIND_PRIOR_H_
#define INMIND_PRIOR_H_

#include <hdp/mcmc.h>

#include "datalog.h"

enum struct string_type {
	FIELD = 1,
	CONCEPT = 2,
	INSTANCE = 3,
	VALUE = 4,
	ANY = 5
};

struct string_type_printer { };

template<typename Stream>
inline bool print(string_type type, Stream& out) {
	switch (type) {
	case string_type::FIELD:
		return print("field", out);
	case string_type::CONCEPT:
		return print("concept", out);
	case string_type::INSTANCE:
		return print("instance", out);
	case string_type::VALUE:
		return print("value", out);
	case string_type::ANY:
		return print("*", out);
	}
	fprintf(stderr, "print ERROR: Unrecognized string_type.\n");
	exit(EXIT_FAILURE);
}

template<typename Stream>
inline bool print(unsigned int key, Stream& out, const string_type_printer& printer) {
	return print((string_type) key, out);
}

struct inmind_prior
{
	static constexpr unsigned int ARG_COUNT = datalog_predicate::ARG_COUNT;

	hdp<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>* arg_hdp;
	hdp_sampler<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>* arg_sampler;
	cache<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>* arg_cache;
	array_map<unsigned int, array<unsigned int>*>* arg_root_probabilities;
	array<unsigned int>** unseen_arg_root_probabilities;

	hdp<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> string_hdp;
	hdp_sampler<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> string_sampler;
	cache<uniform_distribution<double>, constant<unsigned int>, unsigned int, double> string_cache;
	array<unsigned int>** string_root_probabilities;

	hash_map<unsigned int, unsigned int> text_observations;

	array<pair<unsigned int, unsigned int>> field_sources, instance_sources, concept_sources;
	array_map<sequence, pair<unsigned int, unsigned int>> field_sizes; /* posterior hyperparameters */
	pair<unsigned int, unsigned int> instance_sizes; /* posterior hyperparameters */

	double field_length_alpha, field_length_beta;
	double instance_length_alpha, instance_length_beta;

	hash_map<pair<datalog_literal, datalog_literal>, double>* arg_posterior_cache;
	hash_map<pair<unsigned int, unsigned int>, double> string_posterior_cache;
	std::mutex* arg_hdp_lock;
	std::mutex string_hdp_lock;

	inmind_prior(
			unsigned int predicate_count, unsigned int token_count,
			const double* arg_hdp_alpha, const double* string_hdp_alpha, double string_prior_alpha,
			double field_length_alpha, double field_length_beta,
			double instance_length_alpha, double instance_length_beta) :
		arg_hdp(NULL), arg_sampler(NULL), arg_cache(NULL), arg_root_probabilities(NULL),
		unseen_arg_root_probabilities(NULL), string_hdp(token_count, string_hdp_alpha, 2),
		string_sampler(string_hdp), string_cache(string_sampler), string_root_probabilities(NULL),
		text_observations(128), field_sources(8), instance_sources(8), concept_sources(8), field_sizes(8), instance_sizes(0, 0),
		field_length_alpha(field_length_alpha), field_length_beta(field_length_beta),
		instance_length_alpha(instance_length_alpha), instance_length_beta(instance_length_beta),
		string_posterior_cache(1024), arg_hdp_lock(NULL)
	{
		arg_hdp = (hdp<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>*)
				malloc(sizeof(hdp<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>) * ARG_COUNT);
		arg_sampler = (hdp_sampler<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>*)
				malloc(sizeof(hdp_sampler<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>) * ARG_COUNT);
		arg_cache = (cache<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>*)
				malloc(sizeof(cache<uniform_distribution<double>, constant<unsigned int>, unsigned int, double>) * ARG_COUNT);
		arg_root_probabilities = (array_map<unsigned int, array<unsigned int>*>*)
				malloc(sizeof(array_map<unsigned int, array<unsigned int>*>) * ARG_COUNT);
		unseen_arg_root_probabilities = (array<unsigned int>**) calloc(ARG_COUNT, sizeof(array<unsigned int>*));

		arg_posterior_cache = (hash_map<pair<datalog_literal, datalog_literal>, double>*)
				malloc(sizeof(hash_map<pair<datalog_literal, datalog_literal>, double>) * ARG_COUNT);
		arg_hdp_lock = new std::mutex[ARG_COUNT];

		if (arg_hdp == NULL || arg_sampler == NULL || arg_cache == NULL
		 || arg_root_probabilities == NULL || arg_posterior_cache == NULL) {
			fprintf(stderr, "inmind_prior ERROR: Out of memory.\n"); exit(EXIT_FAILURE);
		}

		for (unsigned int i = 0; i < ARG_COUNT; i++) {
			if (!init(arg_hdp[i], predicate_count, arg_hdp_alpha, 2)
			 || !init(arg_sampler[i], arg_hdp[i]) || !init(arg_cache[i], arg_sampler[i])
			 || !array_map_init(arg_root_probabilities[i], 256)
			 || !hash_map_init(arg_posterior_cache[i], 256)) {
				exit(EXIT_FAILURE);
			}
		}
	}

	~inmind_prior() { free(); }

	bool add_field_source(unsigned int source, unsigned int arg) {
		return field_sources.add(make_pair(source, arg));
	}

	bool add_instance_source(unsigned int source, unsigned int arg) {
		return instance_sources.add(make_pair(source, arg));
	}

	bool add_concept_source(unsigned int source, unsigned int arg) {
		return concept_sources.add(make_pair(source, arg));
	}

	bool train(const datalog_expression_root* const* examples, unsigned int length,
			const datalog_expression_root* const* constants, unsigned int constants_length,
			unsigned int burn_in, unsigned int iterations, unsigned int skip)
	{
		hash_set<unsigned int>* arg_observations = (hash_set<unsigned int>*) malloc(sizeof(hash_set<unsigned int>) * ARG_COUNT);
		for (unsigned int i = 0; i < ARG_COUNT; i++) {
			if (!hash_set_init(arg_observations[i], 256)) {
				for (unsigned int j = 0; j < i; j++) core::free(arg_observations[j]);
				core::free(arg_observations); return false;
			}
		}

		array_map<sequence, array_histogram<unsigned int>> field_lengths(8);
		array_histogram<unsigned int> instance_lengths(8);
		for (unsigned int i = 0; i < length; i++) {
			if (!add_training_example(examples[i]->root, arg_observations, text_observations, field_lengths, instance_lengths))
				return false;
		}
		if (field_lengths.size > 1)
			sort(field_lengths.keys, field_lengths.values, field_lengths.size, default_sorter());
		if (field_lengths.size > 0) {
			if (!field_sizes.ensure_capacity(field_lengths.size))
				exit(EXIT_FAILURE);
			for (unsigned int i = 0; i < field_lengths.size; i++) {
				move(field_lengths.keys[i], field_sizes.keys[i]);
				field_sizes.values[i].key = 0;
				for (const auto& entry : field_lengths.values[i].counts)
					field_sizes.values[i].key += entry.key * entry.value;
				field_sizes.values[i].value = field_lengths.values[i].sum;
				core::free(field_lengths.values[i]);
			}
		}

		instance_sizes.key = 0;
		for (const auto& entry : instance_lengths.counts)
			instance_sizes.key += entry.key * entry.value;
		instance_sizes.value = instance_lengths.sum;

		/* perform MCMC on the arg semantics model and string model */
		for (unsigned int i = 0; i < ARG_COUNT; i++)
			prepare_sampler(arg_sampler[i], arg_cache[i]);
		prepare_sampler(string_sampler, string_cache);
		for (unsigned int t = 0; t < burn_in; t++) {
			for (unsigned int i = 0; i < ARG_COUNT; i++)
				sample_hdp<true>(arg_sampler[i], arg_cache[i]);
			sample_hdp<true>(string_sampler, string_cache);
		}
		for (unsigned int t = 0; t < iterations; t++) {
			for (unsigned int i = 0; i < ARG_COUNT; i++)
				sample_hdp<true>(arg_sampler[i], arg_cache[i]);
			sample_hdp<true>(string_sampler, string_cache);
			if (t % skip == 0) {
				for (unsigned int i = 0; i < ARG_COUNT; i++) {
					if (!arg_sampler[i].add_sample()) {
						free_observations(arg_observations);
						return false;
					}
				}
				if (!string_sampler.add_sample()) {
					free_observations(arg_observations);
					return false;
				}
			}
		}

		/* precompute the root probabilities for the arg hdp */
		for (unsigned int i = 0; i < ARG_COUNT; i++) {
			for (unsigned int observation : arg_observations[i])
				if (!arg_root_probabilities[i].put(observation, arg_cache[i].compute_root_probabilities(arg_sampler[i], observation)))
					return false;
			unseen_arg_root_probabilities[i] = arg_cache[i].compute_root_probabilities(arg_sampler[i], 0);
			if (unseen_arg_root_probabilities[i] == NULL) {
				free_observations(arg_observations);
				return false;
			}
			if (arg_root_probabilities[i].size > 1)
				sort(arg_root_probabilities[i].keys, arg_root_probabilities[i].values, arg_root_probabilities[i].size);
		}
		free_observations(arg_observations);

		/* precompute the root probabilities for the string hdp */
		string_root_probabilities = (array<unsigned int>**) malloc(sizeof(array<unsigned int>*) * (text_observations.table.size + 1));
		for (unsigned int observation = 0; observation < text_observations.table.size + 1; observation++)
			string_root_probabilities[observation] = string_cache.compute_root_probabilities(string_sampler, observation);
		return true;
	}

	template<bool CompleteContext>
	inline double log_probability(const datalog_expression_root& example) {
		if (example.root.type == DATALOG_ANY || example.root.type == DATALOG_NON_EMPTY) {
			return 0.0;
		} else if (example.root.type == DATALOG_EMPTY || example.root.type == DATALOG_STRING) {
			if (!CompleteContext) return 0.0;
			else return -std::numeric_limits<double>::infinity();
		} else if (example.root.type != DATALOG_PREDICATE) {
			return -std::numeric_limits<double>::infinity();
		}

		double score = 0.0;
		if (CompleteContext) {
			/* only do this for complete logical forms to ensure the prior is separable */
			score = max_arg_posterior(0,
					{example.root.pred.function, example.root.pred.excluded, example.root.pred.excluded_count},
					{DATALOG_EMPTY, NULL, 0});
		}

		sequence* string = NULL;
		return score + log_probability<CompleteContext>(example.root.pred, string, NULL, CompleteContext ? string_type::VALUE : string_type::ANY);
	}

private:
	inline bool add_training_example(
			const datalog_expression& expression,
			hash_set<unsigned int>* arg_observations,
			hash_map<unsigned int, unsigned int>& text_observations,
			array_map<sequence, array_histogram<unsigned int>>& field_lengths,
			array_histogram<unsigned int>& instance_lengths)
	{
		if (expression.type != DATALOG_PREDICATE) {
			fprintf(stderr, "inmind_prior.add_training_example ERROR: Logical form must be a predicate instance.\n");
			return false;
		}

		unsigned int path[] = {DATALOG_EMPTY};
		if (!add(arg_sampler[0], path, 1, expression.pred.function, arg_cache[0])
		 || !arg_observations[0].add(expression.pred.function))
			return false;

		sequence* string = NULL;
		return add_node(expression.pred, arg_observations, text_observations,
				string, NULL, field_lengths, instance_lengths, string_type::VALUE);
	}

	inline bool add_node(
			const datalog_predicate& pred,
			hash_set<unsigned int>* arg_observations,
			hash_map<unsigned int, unsigned int>& text_observations,
			sequence*& string, const sequence* field_name,
			array_map<sequence, array_histogram<unsigned int>>& field_lengths,
			array_histogram<unsigned int>& instance_lengths,
			string_type type)
	{
		unsigned int field_source_arg = ARG_COUNT, instance_source_arg = ARG_COUNT, concept_source_arg = ARG_COUNT;
		for (unsigned int i = 0; i < field_sources.length; i++) {
			if (field_sources[i].key == pred.function) {
				field_source_arg = field_sources[i].value; break;
			}
		} for (unsigned int i = 0; i < instance_sources.length; i++) {
			if (instance_sources[i].key == pred.function) {
				instance_source_arg = instance_sources[i].value; break;
			}
		} for (unsigned int i = 0; i < concept_sources.length; i++) {
			if (concept_sources[i].key == pred.function) {
				concept_source_arg = concept_sources[i].value; break;
			}
		}

		const sequence* child_field_name = (field_source_arg < ARG_COUNT) ? NULL : field_name;
		for (unsigned int i = 0; i < ARG_COUNT; i++) {
			unsigned int observation;
			if (pred.args[i] == NULL) {
				observation = DATALOG_EMPTY;
			} else if (pred.args[i]->type == DATALOG_PREDICATE) {
				observation = pred.args[i]->pred.function;
			} else if (pred.args[i]->type == DATALOG_STRING) {
				observation = pred.args[i]->type;
				string = &pred.args[i]->str;
				if (field_name != NULL
				 && !field_lengths.get(*field_name).add(string->length))
					return false;

				bool contains; unsigned int index;
				unsigned int path[] = { (unsigned int) type };
				if (!text_observations.check_size(text_observations.table.size + string->length))
					return false;
				for (unsigned int i = 0; i < string->length; i++) {
					unsigned int& id = text_observations.get(string->tokens[i], contains, index);
					if (!contains) {
						text_observations.table.keys[index] = string->tokens[i];
						text_observations.table.size++;
						id = text_observations.table.size;
					}
					add(string_sampler, path, 1, id, string_cache);
				}
				switch (type) {
				case string_type::FIELD:
					if (!field_lengths.ensure_capacity(field_lengths.size + 1))
						return false;
					index = field_lengths.index_of(*string);
					if (index == field_lengths.size) {
						if (!init(field_lengths.values[index], 8))
							return false;
						field_lengths.keys[index] = *string;
						field_lengths.size++;
					}
					break;
				case string_type::INSTANCE:
					instance_lengths.add(string->length);
					break;
				case string_type::CONCEPT:
				case string_type::VALUE:
					break;
				case string_type::ANY:
					fprintf(stderr, "inmind_prior.add_node ERROR: This should be unreachable.\n");
					exit(EXIT_FAILURE);
				}
			} else {
				fprintf(stderr, "inmind_prior.add_node ERROR: Unexpected expression type.\n");
				return false;
			}

			if (!add(arg_sampler[i], &pred.function, 1, observation, arg_cache[i])
			 || !arg_observations[i].add(observation))
				return false;

			string_type arg_type = type;
			if (i == field_source_arg) {
				arg_type = string_type::FIELD;
			} if (i == instance_source_arg) {
				arg_type = string_type::INSTANCE;
			} if (i == concept_source_arg) {
				arg_type = string_type::CONCEPT;
			}

			sequence* child_string = NULL;
			if (pred.args[i] != NULL && pred.args[i]->type == DATALOG_PREDICATE
			 && !add_node(pred.args[i]->pred, arg_observations, text_observations,
					 child_string, child_field_name, field_lengths, instance_lengths, arg_type))
				return false;

			if (i == field_source_arg) {
				child_field_name = child_string;
			} else if (string == NULL) {
				string = child_string;
			}
		}
		return true;
	}

	inline double max_string_posterior(
			unsigned int observation, unsigned int parent,
			const array<unsigned int>* root_probabilities)
	{
		array<weighted_feature_set<double>> paths = array<weighted_feature_set<double>>(8);
		predict(string_sampler, observation, &parent, NULL, 0, paths, root_probabilities);

		double score = max(paths);
		for (weighted_feature_set<double>& path : paths)
			core::free(path);
		return score;
	}

	inline double max_string_posterior(unsigned int observation, string_type type)
	{
		string_hdp_lock.lock();
		if (!string_posterior_cache.check_size()) exit(EXIT_FAILURE);

		bool contains; unsigned int bucket;
		double posterior = string_posterior_cache.get({observation, (unsigned int) type}, contains, bucket);
		string_hdp_lock.unlock();
if (debug2 && contains) {
print("\tlog p(\"", stderr);
print(observation, stderr, *debug_terminal_printer); print("\" | ", stderr);
print(type, stderr); fprintf(stderr, ") = %lf\n", posterior);
}
		if (contains) return posterior;

		unsigned int id = text_observations.get(observation, contains);
		if (!contains) id = 0;
		const array<unsigned int>* root_probabilities = string_root_probabilities[id];
		posterior = max_string_posterior(id, (unsigned int) type, root_probabilities);

		string_hdp_lock.lock();
		string_posterior_cache.get({observation, (unsigned int) type}, contains, bucket);
		if (!contains) {
			pair<unsigned int, unsigned int>& new_pair = string_posterior_cache.table.keys[bucket];
			new_pair.key = observation;
			new_pair.value = (unsigned int) type;
			string_posterior_cache.values[bucket] = posterior;
			string_posterior_cache.table.size++;
		}
		string_hdp_lock.unlock();
if (debug2) {
print("\tlog p(\"", stderr);
print(observation, stderr, *debug_terminal_printer); print("\" | ", stderr);
print(type, stderr); fprintf(stderr, ") = %lf\n", posterior);
}
		return posterior;
	}

	inline double max_arg_posterior(unsigned int arg_index,
			unsigned int observation, unsigned int parent,
			const unsigned int* excluded, unsigned int excluded_count,
			const array<unsigned int>* root_probabilities)
	{
		array<weighted_feature_set<double>> paths = array<weighted_feature_set<double>>(8);
		predict(arg_sampler[arg_index], observation, &parent, &excluded, &excluded_count, paths, root_probabilities);

		double score = max(paths);
		for (weighted_feature_set<double>& path : paths)
			core::free(path);
		return score;
	}

	inline double max_arg_posterior(unsigned int arg_index,
			datalog_literal observation, datalog_literal parent)
	{
		arg_hdp_lock[arg_index].lock();
		if (!arg_posterior_cache[arg_index].check_size()) exit(EXIT_FAILURE);

		bool contains; unsigned int bucket;
		double posterior = arg_posterior_cache[arg_index].get({observation, parent}, contains, bucket);
		arg_hdp_lock[arg_index].unlock();
if (debug2 && contains) {
print("\tlog p_", stderr); print(arg_index, stderr); print('(', stderr);
print(observation.label, stderr, *debug_terminal_printer); print(" | ", stderr);
print(parent.label, stderr, *debug_terminal_printer);
fprintf(stderr, ") = %lf\n", posterior);
}
		if (contains) return posterior;

		if (observation.label == DATALOG_LABEL_WILDCARD) {
			posterior = max_arg_posterior(arg_index, 0, parent.label, parent.excluded,
					parent.excluded_count, unseen_arg_root_probabilities[arg_index]);
			for (const auto& entry : arg_root_probabilities[arg_index]) {
				if (!observation.is_excluded(entry.key))
					posterior = max(posterior, max_arg_posterior(arg_index,
							entry.key, parent.label, parent.excluded, parent.excluded_count, entry.value));
			}
		} else {
			unsigned int index = arg_root_probabilities[arg_index].index_of(observation.label);
			const array<unsigned int>* root_probabilities = (index < arg_root_probabilities[arg_index].size) ?
					arg_root_probabilities[arg_index].values[index] : unseen_arg_root_probabilities[arg_index];
			posterior = max_arg_posterior(arg_index, observation.label,
					parent.label, parent.excluded, parent.excluded_count, root_probabilities);
		}

		arg_hdp_lock[arg_index].lock();
		arg_posterior_cache[arg_index].get({observation, parent}, contains, bucket);
		if (!contains) {
			pair<datalog_literal, datalog_literal>& new_pair = arg_posterior_cache[arg_index].table.keys[bucket];
			if (!init(new_pair.key, observation) || !init(new_pair.value, parent))
				exit(EXIT_FAILURE);
			arg_posterior_cache[arg_index].values[bucket] = posterior;
			arg_posterior_cache[arg_index].table.size++;
		}
		arg_hdp_lock[arg_index].unlock();
if (debug2) {
print("\tlog p_", stderr); print(arg_index, stderr); print('(', stderr);
print(observation.label, stderr, *debug_terminal_printer); print(" | ", stderr);
print(parent.label, stderr, *debug_terminal_printer);
fprintf(stderr, ") = %lf\n", posterior);
}
		return posterior;
	}

	template<bool CompleteContext>
	inline double log_probability(const datalog_predicate& pred,
			sequence*& string, const sequence* field_name, string_type type)
	{
		unsigned int field_source_arg = ARG_COUNT, instance_source_arg = ARG_COUNT, concept_source_arg = ARG_COUNT;
		for (unsigned int i = 0; i < field_sources.length; i++) {
			if (field_sources[i].key == pred.function) {
				field_source_arg = field_sources[i].value; break;
			}
		} for (unsigned int i = 0; i < instance_sources.length; i++) {
			if (instance_sources[i].key == pred.function) {
				instance_source_arg = instance_sources[i].value; break;
			}
		} for (unsigned int i = 0; i < concept_sources.length; i++) {
			if (concept_sources[i].key == pred.function) {
				concept_source_arg = concept_sources[i].value; break;
			}
		}

		if (pred.function == DATALOG_LABEL_WILDCARD)
			type = string_type::ANY;

		double score = 0.0;
		const sequence* child_field_name = (field_source_arg < ARG_COUNT) ? NULL : field_name;
		for (unsigned int i = 0; i < ARG_COUNT; i++) {
			unsigned int observation; unsigned int* excluded = NULL; unsigned int excluded_count;
			if (pred.args[i] == NULL) {
				/* we need to take into account functions like 'delete_arg' */
				observation = CompleteContext ? DATALOG_EMPTY : DATALOG_LABEL_WILDCARD;
				excluded_count = 0;
			} else if (pred.args[i]->type == DATALOG_PREDICATE) {
				observation = pred.args[i]->pred.function;
				excluded = pred.args[i]->pred.excluded;
				excluded_count = pred.args[i]->pred.excluded_count;
			} else if (pred.args[i]->type == DATALOG_STRING) {
				observation = pred.args[i]->type;
				excluded_count = 0;
				string = &pred.args[i]->str;
				if (field_name != NULL) {
					unsigned int index = field_sizes.index_of(*field_name);
					double field_length_log_probability;
					if (index < field_sizes.size) {
						double p = field_length_beta + field_sizes.values[index].value;
						field_length_log_probability = log_probability_negative_binomial(
								string->length, field_length_alpha + field_sizes.values[index].key, 1.0 / (1.0 + p));
					} else {
						field_length_log_probability = log_probability_negative_binomial(
								string->length, field_length_alpha, 1.0 / (1.0 + field_length_beta));
					}
					score += field_length_log_probability;
if (debug2) {
fprintf(stderr, "\tlog p(field length = %u | \"", string->length);
print(*field_name, stderr, *debug_terminal_printer);
fprintf(stderr, "\") = %lf\n", field_length_log_probability);
}
				}

				if (type == string_type::INSTANCE) {
					double p = instance_length_beta + instance_sizes.value;
					double instance_length_log_probability = log_probability_negative_binomial(
							string->length, instance_length_alpha + instance_sizes.key, 1.0 / (1.0 + p));
					score += instance_length_log_probability;
if (debug2) {
print("\tlog p(instance = \"", stderr);
print(*string, stderr, *debug_terminal_printer);
fprintf(stderr, "\") = %lf\n", instance_length_log_probability);
}
				} if (type != string_type::ANY) {
					for (unsigned int i = 0; i < string->length; i++)
						score += max_string_posterior(string->tokens[i], type);
				}

			} else if (pred.args[i]->type == DATALOG_ANY || pred.args[i]->type == DATALOG_NON_EMPTY) {
				observation = DATALOG_LABEL_WILDCARD;
				excluded_count = 0;
			} else if (pred.args[i]->type == DATALOG_CONSTANT) {
				return -std::numeric_limits<double>::infinity();
			} else {
				fprintf(stderr, "inmind_prior.log_probability ERROR: Unexpected expression type.\n");
				exit(EXIT_FAILURE);
			}

			score += max_arg_posterior(i, {observation, excluded, excluded_count},
					{pred.function, pred.excluded, pred.excluded_count});

			string_type arg_type = type;
			if (i == field_source_arg) {
				arg_type = string_type::FIELD;
			} if (i == instance_source_arg) {
				arg_type = string_type::INSTANCE;
			} if (i == concept_source_arg) {
				arg_type = string_type::CONCEPT;
			}

			if (pred.args[i] != NULL && pred.args[i]->type == DATALOG_PREDICATE
			 && (CompleteContext || !can_be_empty(*pred.args[i]))) /* 'arg' features can make any argument NULL */
			{
				sequence* child_string = NULL;
				score += log_probability<true>(pred.args[i]->pred, child_string, child_field_name, arg_type);

				if (i != field_source_arg && string == NULL)
					string = child_string;
			}
		}
		return score;
	}

	inline void free_observations(hash_set<unsigned int>* arg_observations) {
		for (unsigned int i = 0; i < ARG_COUNT; i++)
			core::free(arg_observations[i]);
		core::free(arg_observations);
	}

	inline void free() {
		for (unsigned int i = 0; i < datalog_predicate::ARG_COUNT; i++) {
			for (auto entry : arg_root_probabilities[i])
				cleanup_root_probabilities(entry.value, arg_sampler[i].posterior.length);
			for (auto entry : arg_posterior_cache[i])
				core::free(entry.key);
			if (unseen_arg_root_probabilities[i] != NULL)
				cleanup_root_probabilities(unseen_arg_root_probabilities[i], arg_sampler[i].posterior.length);

			core::free(arg_root_probabilities[i]); core::free(arg_posterior_cache[i]);
			core::free(arg_cache[i]); core::free(arg_sampler[i]); core::free(arg_hdp[i]);
		}
		core::free(arg_hdp); core::free(arg_sampler); core::free(arg_cache);
		core::free(arg_root_probabilities);
		core::free(unseen_arg_root_probabilities);
		core::free(arg_posterior_cache);
		delete[] arg_hdp_lock;

		if (string_root_probabilities != NULL) {
			for (unsigned int i = 0; i < text_observations.table.size + 1; i++)
				cleanup_root_probabilities(string_root_probabilities[i], string_sampler.posterior.length);
			core::free(string_root_probabilities);
		}
	}
};

#endif /* INMIND_PRIOR_H_ */
