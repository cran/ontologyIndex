# ontologyIndex 2.2

* The `*_branches` functions have all been renamed to `*_descendants` and their `branch_roots` argument to `roots`.
* The `get_descendants` function has had two arguments renamed: `ancestor` is now `roots` and can accept a vector of root terms, returning the set of all terms which descend from them; `remove_ancestor` is now `exclude_roots`, determining whether or not to exclude the given `roots` terms from the result.
* A `check` function which checks whether an ontology is valid has been added.
* XML encoded 'OWL' files can now be read in as `ontology_index`es.

# ontologyIndex 2.1

* A specialisation of `print` has been added for `ontology_index`es.

# ontologyIndex 2.0

* `get_ontology` now only requires a `file` argument.
* `ontology_index` objects now contain an `obsolete` slot - a `logical` vector.
* `prune_branches`, `exclude_branches` and `intersection_with_branches` now use the same signature and are renamed to reflect the possibilit of passing multiple branch roots.
* `get_term_property`, `get_term_parents`, `get_term_ancestors` and `get_term_children` functions with `as_names` argument to get descriptive names added for convenience.
* `swap_out_alt_ids` replaced with `force_compatibility` which optionally throws an error if non-matchable term IDs are passed.
* GO now included: `data(go)` now loads an `ontology_index` of the Gene Ontology (2015-11-27).
