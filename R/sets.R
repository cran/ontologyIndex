#' Get property of individual ontological term
#'
#' @template ontology
#' @param property_name Name of property.
#' @param term Character value of term ID.
#' @param as_names Logical value determining whether to return character vector of names (defaults to \code{FALSE}).
#' @export
get_term_property <- function(ontology, property_name, term, as_names=FALSE) {
	if (!property_name %in% names(ontology))
		stop(paste0("Property '", property_name, "' is unknown in ontology"))
	if (!(length(term) == 1))
		stop("Must specify only a single term")
	if (!term %in% ontology[["id"]])
		stop("Term ID is not in ontology")
	result <- if (is.list(ontology[[property_name]]))
		ontology[[property_name]][[term]]
	else
		ontology[[property_name]][term]
	if (as_names) {
		if (all(result %in% ontology[["id"]]))
			ontology[["name"]][result]
		else 
			stop(paste0("Could not convert '", as.character(result), "' to term names"))
	} else {
		result
	}
}

#' Get logical descendancy matrix for set of terms
#' 
#' @return A logical square matrix of with \code{length(terms)} columns and rows. \code{result[row_term,col_term] == TRUE} if \code{row_term} is an ancestor (and not the same as) of \code{col_term}.
#'
#' @template ontology
#' @template terms
#' @param rows Rows for resultant matrix (defaults to \code{terms}).
#' @param cols Cols for resultant matrix (defaults to \code{terms}).
#' @return A logical matrix.
#' @examples 
#' data(hpo)
#' get_term_descendancy_matrix(hpo, c("HP:0001873", "HP:0011877"))
#' @export
#' @importFrom stats setNames
get_term_descendancy_matrix <- function(ontology, terms=NULL, rows=terms, cols=terms) {
	if (class(terms) != "character")
		stop("'terms' must be a character vector of term IDs")
	# 'row is column ancestor'
	if (length(terms) < 2)
		matrix(FALSE, length(terms), length(terms), dimnames=rep(list(terms), 2))
	else
		sapply(
			setNames(cols, cols),
			function(term) setNames(
				rows %in% setdiff(ontology$ancestors[[term]], term),
				rows
			)
		)
}

#' Remove redundant/implied terms from a set of terms
#'
#' @template ontology
#' @template terms
#' @return Character vector of terms
#' @examples
#' data(hpo)
#' minimal_set(hpo, c("HP:0001873", "HP:0001872"))
#' @export
minimal_set <- function(ontology, terms) {
	if (class(terms) != "character")
		stop("'terms' must be a character vector of term IDs")
	redundant <- unlist(
		use.names=FALSE,
		lapply(
			terms,
			function(x) setdiff(ontology$ancestors[[x]], x)
		)
	)
	setdiff(terms,redundant)
}

#' Get set of terms containing all ancestors of terms in a given set
#'
#' @template ontology
#' @template terms
#' @return Character vector of all terms which are an ancestor of at least one term in \code{terms}, including the terms themselves
#' @seealso \code{link{get_descendants}}
#' @examples
#' data(hpo)
#' get_ancestors(hpo, c("HP:0001873", "HP:0011877"))
#' @export
get_ancestors <- function(ontology, terms) {
	if (class(terms) != "character")
		stop("'terms' must be a character vector of term IDs")
	unique(
		unlist(
			use.names=FALSE,
			ontology$ancestors[terms]
		)
	)
}

#' Get frequency of each term in a set of phenotypes
#'
#' @template ontology
#' @template term_sets
#' @param patch_missing Logical indicating whether to include whole ontology even if they're not present in the \code{term_sets} as if they had occurred once
#' @return Numeric vector of information contents, named by corresponding terms. Takes into account ancestors, in the sense that all ancestor terms implied by the phenotypes are considered 'on'
#' @seealso \code{\link{get_term_info_content}}
#' @examples
#' data(hpo)
#' get_term_frequencies(hpo, list("HP:0001873"))
#' @export
get_term_frequencies <- function(
	ontology, 
	term_sets,
	patch_missing=FALSE
) {
	exp(-get_term_info_content(ontology, term_sets, patch_missing=FALSE))
}

#' Get information content of each term in a set of phenotypes
#'
#' @template ontology
#' @template term_sets
#' @param patch_missing Logical indicating whether to include all ontology terms even if they're not present in the \code{term_sets} as if they had occurred once
#' @return Numeric vector of information contents, named by corresponding terms. Takes into account ancestors, in the sense that all ancestor terms implied by the phenotypes are considered 'on'
#' @examples
#' data(hpo)
#' get_term_info_content(hpo, list("HP:0001873"))
#' @export
get_term_info_content <- function(
	ontology, 
	term_sets,
	patch_missing=FALSE
) {
	terms.tab <- table(unlist(use.names=FALSE, lapply(term_sets, function(x) get_ancestors(ontology, x))))
	total.patients <- length(term_sets)
	terms.numeric <- as.numeric(terms.tab)
	names(terms.numeric) <- names(terms.tab)

	result <- log(total.patients) - ifelse(terms.numeric==0, log(total.patients), log(terms.numeric))
	
	if (patch_missing) {
		#include missing terms and give max information content...
		missing.terms <- setdiff(ontology$id, names(result))
		missing.infos <- rep(max(result), length(missing.terms))
		names(missing.infos) <- missing.terms
		result <- c(result, missing.infos) 
	}

	result
}

#' Select terms by propagating relations from a set of terms 
#'
#' An `ontology_index` can contain multiple relations (for example in the case of the Gene Ontology, \code{"is_a"} and \code{"part_of"} could be stored as separate properties in an `ontology_index`). Transitive relations (i.e. relations such that x related to y and y related to z implies x related to z, for example the relation 'is an ancestor of') stored by an `ontology_index` can be propagated using this function. The 'inverse relations' (i.e. x inversely related to y if y related to x) can also be propagated by setting the \code{use_inverse_relations} parameter to \code{TRUE}.
#'
#' @template ontology
#' @param roots Character vector of term IDs from which relations will be propagated.
#' @param relations Character vector given names of transitive relations to be propagated.
#' @param use_inverse_relations Boolean vector indicating whether to propagate inverse relations. If \code{use_inverse_relations} is the same length as \code{relations}, each element determines whether the corresponding relation in \code{relations} is inverted.
#' @template exclude_roots
#' @return Character vector of terms
#' @seealso \code{\link{get_ancestors}}, \code{\link{get_descendants}}
#' @export
propagate_relations <- function(ontology, roots, relations, use_inverse_relations=FALSE, exclude_roots=FALSE) {
	if (class(roots) != "character")
		stop("'roots' must be a character vector of term IDs")
	if (!all(roots %in% ontology$id))
		warning(paste0("Terms '", paste0(roots, collapse=", "), "' not found in ontology"))

	if (length(use_inverse_relations > 1) & length(use_inverse_relations) != length(relations))
		stop("'use_inverse_relations' must either be a single logical value or have the same length as 'relations'")

	if (!all(relations %in% names(ontology)))
		stop(paste0("'", setdiff(relations, names(ontology))[1], "' relation not found in ontology"))

	direct <- unique(setdiff(unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=function(rel, invert) if (invert) ontology$id[sapply(ontology[[rel]], function(x) any(x %in% roots))] else ontology[[rel]][roots], relations, use_inverse_relations)), roots))
	result <- if (length(direct) == 0) roots else c(roots, propagate_relations(ontology=ontology, roots=direct, relations=relations, use_inverse_relations=use_inverse_relations))
	unname(if (exclude_roots) setdiff(result, roots) else result)
}


