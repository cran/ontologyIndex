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
	unique(
		unlist(
			use.names=FALSE,
			ontology$ancestors[terms]
		)
	)
}
