#' Print \code{ontology_index} object.
#'
#' @param x \code{ontology_index} object.
#' @param ... Unused parameters.
#' @return Prints a summary
#' @method print ontology_index
#' @export
print.ontology_index <- function(x, ...) {
	stopifnot(class(x) == "ontology_index")
	obs <- if (is.null(x$obsolete)) rep(FALSE, length(x$id)) else x$obsolete
	cat("Ontology with ", sum(!obs), " terms\n", sep="")
	version <- attr(x, "version", exact=TRUE)
	if (!is.null(version)) cat("\n", paste0(collapse="", grep(x=version, pattern="^(format-version|data-version|default-namespace|ontology):", value=TRUE), "\n"), sep="")
	cat("\nProperties:\n")
	cat(paste0("\t", names(x), ": ", sapply(x, class), "\n"), sep="")
	cat("Roots:\n")
	roots <- x$id[!obs & sapply(x$parents, length) == 0]
	show_roots <- roots[order(sapply(x$children[roots], length), decreasing=TRUE)[seq(min(length(roots), 10))]]
	cat(paste0(collapse="", "\t", show_roots, " - ", x$name[show_roots], "\n"), sep="")
	if (length(roots) > length(show_roots)) cat(" ... ", length(roots)-length(show_roots), " more\n", sep="")
}



