# ontologyIndex 2.0

* `get_ontology` now only requires a `file` argument.
* `ontology_index` objects now contain an `obsolete` slot - a `logical` vector.
* `prune_branches`, `exclude_branches` and `intersection_with_branches` now use the same signature and are renamed to reflect the possibilit of passing multiple branch roots.
* `get_term_property`, `get_term_parents`, `get_term_ancestors` and `get_term_children` functions with `as_names` argument to get descriptive names added for convenience.
* `swap_out_alt_ids` replaced with `force_compatibility` which optionally throws an error if non-matchable term IDs are passed.
* GO now included: `data(go)` now loads an `ontology_index` of the Gene Ontology (2015-11-27).
