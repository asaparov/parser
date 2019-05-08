
**TODO:** Document this code. The repository is located at <https://github.com/asaparov/parser>.

This code implements the experiments as described in [this paper](https://asaparov.org/assets/conll_2017.pdf). It uses a generative model of semantic grammar, which is implemented as a separate modular repository [grammar](https://github.com/asaparov/grammar) (the parsing and MCMC sampling algorithm are implemented there).

If you use this code in your research, please cite:
 > Abulhair Saparov, Vijay Saraswat, and Tom M. Mitchell. 2017. A Probabilistic Generative Grammar for Semantic Parsing. In *Proceedings of the 21st Conference on Computational Natural Language Learning (CoNLL 2017)*.

### Dependencies

To use the code, simply download the files and build the desired program with either `make datalog_to_lambda` or `make parser`.

This library depends on [core](https://github.com/asaparov/core), [math](https://github.com/asaparov/math), [hdp](https://github.com/asaparov/hdp), and [grammar](https://github.com/asaparov/grammar). The code makes use of `C++11` and is regularly tested with `gcc 9` but I have previously compiled it with `gcc 4.8`, `clang 4.0`, and `Microsoft Visual C++ 14.0 (2015)`. The code is intended to be platform-independent, so please create an issue if there are any compilation bugs.

### Code structure

This repository implements two logical formalisms: lambda-calculus in `lambda.h` and Datalog in `datalog.h`. A prior distribution Datalog logical forms is defined in `datalog_hdp.h`.

There are two datasets: *GeoQuery* and *Jobs*, located in `geoquery/` and `jobs/`, respectively. Each directory contains training data, test data, domain-specific lexicons, ontologies, and trained grammars (models).

### Programs

There are two programs in this repository.
 - `datalog_to_lambda` converts logical forms in Datalog into lambda calculus. This program is defined in `datalog_to_lambda.cpp` and can be compiled by running `make datalog_to_lambda`.
 - `parser` is a driver program that can train, test, and generate sentences using a generative semantic grammar. The command line usage is `./parser MODE [OPTIONS]...` where `MODE` can be either `sample`, `parse`, or `generate`. `sample` will train a grammar using an input training dataset; `parse` will use a trained grammar to run the parsing algorithm on a collection of sentences; and `generate` will use a trained grammar to generate sentences/questions from a collection of logical forms. The available options are:

 | Command-line option  | Description                                                |
 | ---------------------| ---------------------------------------------------------- |
 | `--train=<filepath>` | The path to the training data. (**required** in all modes) |
 | `--extra=<filepath>` | The path to an extra dataset. In `sample` mode, this is the lexicon. We provide a domain-general lexicon in `lexicon`, as well as lexicons in `geoquery/geoqueries880_lexicon` and `jobs/jobqueries640_lexicon`. In `parse` mode, this a set of additional logical forms that will be used to train the semantic prior (in addition to the logical forms from the training data). |
 | `--kb=<filepath>` | The path to a set of "beliefs" which are also used to train the semantic prior (in addition to the logical forms from the training data). We provide a set of beliefs for GeoQuery in `geoquery/geoqueries880_beliefs`. This option is only used in `parse` mode. |
 | `--test=<filepath>` | The path to the test data. In `parse` mode, the program will attempt to parse each sentence in the test set, comparing the output to the logical form label. In `generate` mode, the algorithm will attempt to generate sentences/questions for every logical form in the test set. (**required** in `parse` and `generate` modes) |
 | `--model=<filepath>` | The path to the model (the file in which the grammar will be stored). In `sample` mode, this is the path to which the trained grammar will be written to. We provide trained models in `geoquery/model_no_lexicon`, `geoquery/model_with_lexicon`, `jobs/model_no_lexicon`, and `jobs/model_with_lexicon`. (**required** in all modes) |
 | `--ontology=<filepath>` | The path to the ontology. We provide ontologies in `geoquery/geoqueries880_types` and `jobs/jobqueries640_types`. |
 | `--grammar=<filepath>` | The path to a file that specifies the nonterminals, "interior" production rules, and hyperparameters of the grammar. The default path is `english.gram`. This option is only used in `sample` mode, since its information will be saved into the model. |
 | `--iterations=<count>` | The number of MCMC iterations with which to train the grammar. This option is only used in `sample` mode. The default is `10`. |
 | `--time-limit=<milliseconds>` | The time limit in milliseconds during parsing of each sentence. The default is `-1` which indicates *no time limit*. This option is only used in `parse` mode. |
 | `--threads=<count>` | The number of threads with which to perform parsing. The default is `1`. This option is only used in `parse` mode. |
 | `--no-morphology` | Disables morphological modeling of words. See README and `morphology.h` in [grammar](https://github.com/asaparov/grammar). |
 | `--test-parseable` | If enabled, the parser will first iterate over each example in the training set. It will check whether the logical form can indeed be derived according to the derivation tree that was learned during sampling. This option is helpful for debugging logical form representations, and is only used in `parse` mode. |

### Experiments

To reproduce the experiments in the paper, the following commands may be run.

To test semantic parser on GeoQuery, **without** lexicon, **without** type-checking:
```
./parser parse --train=geoquery/geoqueries880_train --extra=geoquery/geoqueries880_train_remaining --test=geoquery/geoqueries880_test --model=geoquery/model_no_lexicon --no-morphology
```

To test semantic parser on GeoQuery, **with** lexicon, **without** type-checking:
```
./parser parse --train=geoquery/geoqueries880_train --extra=geoquery/geoqueries880_train_remaining --test=geoquery/geoqueries880_test --kb=geoquery/geoqueries880_beliefs --model=geoquery/model_with_lexicon --no-morphology
```

To test semantic parser on GeoQuery, **without** lexicon, **with** type-checking:
```
./parser parse --train=geoquery/geoqueries880_train --extra=geoquery/geoqueries880_train_remaining --test=geoquery/geoqueries880_test --ontology=geoquery/geoqueries880_types --model=geoquery/model_no_lexicon --no-morphology
```

To test semantic parser on GeoQuery, **with** lexicon, **with** type-checking:
```
./parser parse --train=geoquery/geoqueries880_train --extra=geoquery/geoqueries880_train_remaining --test=geoquery/geoqueries880_test --kb=geoquery/geoqueries880_beliefs --ontology=geoquery/geoqueries880_types --model=geoquery/model_with_lexicon --no-morphology
```

To test semantic parser on Jobs, **without** lexicon, without **type** checking:
```
./parser parse --train=jobs/jobqueries640_train --extra=jobs/jobqueries640_train_remaining --test=jobs/jobqueries640_test --model=jobs/model_no_lexicon --no-morphology
```

To test semantic parser on Jobs, **with** lexicon, **without** type checking:
```
./parser parse --train=jobs/jobqueries640_train --extra=jobs/jobqueries640_train_remaining --test=jobs/jobqueries640_test --model=jobs/model_with_lexicon --no-morphology
```

To test semantic parser on Jobs, **without** lexicon, **with** type checking:
```
./parser parse --train=jobs/jobqueries640_train --extra=jobs/jobqueries640_train_remaining --test=jobs/jobqueries640_test --ontology=jobs/jobqueries640_types --model=jobs/model_no_lexicon --no-morphology
```

To test semantic parser on Jobs, **with** lexicon, **with** type checking:
```
./parser parse --train=jobs/jobqueries640_train --extra=jobs/jobqueries640_train_remaining --test=jobs/jobqueries640_test --ontology=jobs/jobqueries640_types --model=jobs/model_with_lexicon --no-morphology
```

### Training

Although we provide the trained grammar (model files) to run the above commands, the grammar can be trained using the following commands.

To train semantic parser using GeoQuery, **with** lexicon:
```
./parser sample --train=geoquery/geoqueries880_train --extra=geoquery/geoqueries880_lexicon --ontology=geoquery/geoqueries880_types --model=geoquery/model_with_lexicon --no-morphology
```

To train semantic parser using GeoQuery, **without** lexicon:
```
./parser sample --train=geoquery/geoqueries880_train --extra=lexicon --ontology=geoquery/geoqueries880_types --model=geoquery/model_with_lexicon --no-morphology
```

To train semantic parser using Jobs, **with** lexicon:
```
./parser sample --train=jobs/jobqueries640_train --extra=jobs/jobqueries640_lexicon --ontology=jobs/jobqueries640_types --model=jobs/model_with_lexicon --no-morphology
```

To train semantic parser using Jobs, **without** lexicon:
```
./parser sample --train=jobs/jobqueries640_train --extra=lexicon --ontology=jobs/jobqueries640_types --model=jobs/model_with_lexicon --no-morphology
```
