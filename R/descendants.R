#' Get set of terms containing all descendants of terms in a given set
#'
#' @template ontology
#' @template roots
#' @template exclude_roots
#' @return Character vector of terms
#' @seealso \code{link{get_ancestors}}
#' @examples
#' data(hpo)
#' get_descendants(hpo, roots="HP:0001873")
#' @export
get_descendants <- function(ontology, roots, exclude_roots=FALSE) {
	if (class(roots) != "character")
		stop("'terms' must be a character vector of term IDs")
	direct_descs <- unique(setdiff(unlist(use.names=FALSE, ontology[["children"]][roots]), roots))
	result <- if (length(direct_descs) == 0) roots else c(roots, get_descendants(ontology, roots=direct_descs))
	unique(unname(if (exclude_roots) setdiff(result, roots) else result))
}

#' Intersect a set of terms with the descendants of a given set of roots
#' 
#' @template ontology
#' @template roots
#' @template terms
#' @return Character vector of terms
#' @examples 
#' data(hpo)
#' intersection_with_descendants(hpo, c("HP:0001872", "HP:0000707"), c("HP:0001873", "HP:0011877"))
#' @export
#' @seealso \code{\link{exclude_descendants}}, \code{\link{prune_descendants}}
intersection_with_descendants <- function(ontology, roots, terms) { 
	if (class(roots) != "character" | class(terms) != "character")
		stop("'roots' and 'terms' must be character vectors of term IDs")
	as.character(if (length(terms) > 0)
		terms[
			sapply(
				ontology$ancestors[terms], 
				function(ancs) any(ancs %in% roots)
			)
		]
		else character(0)
	)
}

#' Exclude terms descending from any in a given set of root terms
#'
#' Exclude from set \code{terms}, any terms that are either in, or descend from one of, the set \code{roots}.
#'
#' @template ontology
#' @template roots
#' @template terms
#' @return Character vector of terms
#' @export
#' @seealso \code{\link{intersection_with_descendants}}, \code{\link{prune_descendants}}
exclude_descendants <- function(ontology, roots, terms) {
	if (class(roots) != "character" | class(terms) != "character")
		stop("'roots' and 'terms' must be character vectors of term IDs")
	as.character(Filter(
		x=terms,
		f=function(term) !any(ontology$ancestors[[term]] %in% roots)
	))
}

#' Exclude terms descending from a given set of roots but include those roots which were originally implicitly present. 
#'
#' Given two sets of terms, \code{roots} and \code{terms}, construct a set of terms containing those in \code{terms} which do not descend from any term in \code{roots}, and also any terms in \code{roots} which are ancestors of any term in \code{terms}.
#'
#' @template ontology
#' @template roots
#' @template terms
#' @return Character vector of terms
#' @export
#' @seealso \code{\link{exclude_descendants}}, \code{\link{intersection_with_descendants}}
prune_descendants <- function(ontology, roots, terms) {
	if (class(roots) != "character" | class(terms) != "character")
		stop("'roots' and 'terms' must be character vectors of term IDs")

	excluded <- exclude_descendants(ontology, roots, terms)
	result <- if (length(excluded) == length(terms))
		terms
	else
		c(excluded, roots[roots %in% get_ancestors(ontology, terms)])
	as.character(result)
}

