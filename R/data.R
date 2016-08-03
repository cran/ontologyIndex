#' @importFrom stats setNames
str_ancs_from_pars <- function(pars, chld) {
	stopifnot(identical(names(pars), names(chld)))
	int.pars <- c(split(as.integer(factor(unlist(use.names=FALSE, pars), levels=names(pars))), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, names(pars), sapply(pars, length)))), setNames(nm=setdiff(names(pars), unlist(use.names=FALSE, pars)), rep(list(integer(0)), length(setdiff(names(pars), unlist(use.names=FALSE, pars))))))[names(pars)]
	int.chld <- c(split(as.integer(factor(unlist(use.names=FALSE, chld), levels=names(chld))), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, names(chld), sapply(chld, length)))), setNames(nm=setdiff(names(chld), unlist(use.names=FALSE, chld)), rep(list(integer(0)), length(setdiff(names(chld), unlist(use.names=FALSE, chld))))))[names(chld)]

	setNames(nm=names(pars), lapply(ancs_from_pars(
		int.pars,
		int.chld
	), function(x) names(pars)[x]))
}

ancs_from_pars <- function(pars, chld) {
	ancs <- as.list(1:length(pars))
	done <- sapply(pars, function(x) length(x) == 0)
	cands <- which(done)
	new.done <- 1:length(cands)
	while (!all(done)) {
		cands <- unique(unlist(use.names=FALSE, chld[cands[new.done]]))
		v <- sapply(pars[cands], function(x) all(done[x]))
		if (!is.logical(v)) {
			stop("Can't get ancestors for items ", paste0(collapse=", ", which(!done)))
		}
		new.done <- which(v)
		done[cands[new.done]] <- TRUE
		ancs[cands[new.done]] <- mapply(SIMPLIFY=FALSE, FUN=c, lapply(cands[new.done], function(x) unique(unlist(use.names=FALSE, ancs[pars[[x]]]))), cands[new.done])
	}
	ancs
}

#' Get R-Object representation of ontology from obo file
#'
#' @param file File path of obo file
#' @param include_descriptions Logical value determining whether to parse the term descriptions and include them in the \code{ontology_index} object.
#' @return R-Object (list) representing ontology
#' @export
#' @importFrom stats setNames
get_ontology <- function(file, include_descriptions=FALSE) {
	ontology.obo.lines <- readLines(file)

	term.id.pattern <- "^id: ([^ !]+).*"
	term.name.pattern <- "^name: (\\.*)"
	term.parent.pattern <- "^is_a: ([^ !]+).*"

	term.alt_id.pattern <- "^alt_id: ([^ !]+).*"
	obsolete.lines <- grep("^is_obsolete: true", ontology.obo.lines)

	term.id.lines <- grep(term.id.pattern, ontology.obo.lines)
	term.name.lines <- grep(term.name.pattern, ontology.obo.lines)
	term.parent.lines <- grep(term.parent.pattern, ontology.obo.lines)
	term.alt_id.lines <- grep(term.alt_id.pattern, ontology.obo.lines)

	ontology <- NULL

	ontology$id <- sub(
		term.id.pattern,
		"\\1",
		ontology.obo.lines[term.id.lines]
	)

	term.names <- sub(
		term.name.pattern, 
		"\\1", 
		ontology.obo.lines[term.name.lines]
	)

	ontology$obsolete <- setNames(nm=ontology$id, ontology$id %in% as.character(cut(
		obsolete.lines,
		breaks=c(term.id.lines,length(ontology.obo.lines)+1),
		labels=ontology$id
	)))

	ontology$name <- sapply(split(
		term.names,
		cut(
			term.name.lines,
			breaks=c(term.id.lines,length(ontology.obo.lines)+1),
			labels=ontology$id
		)
	)[ontology$id], "[", 1)

	names(ontology$name) <- ontology$id

	Encoding(ontology$name) <- "latin1"
	ontology$name <- iconv(
		ontology$name,
		"latin1",
		"ASCII",
		sub=""
	)

	ontology.parent.terms <- sub(
		term.parent.pattern,
		"\\1",
		ontology.obo.lines[term.parent.lines]
	)

	ontology$parents <- split(
		ontology.parent.terms,
		cut(
			term.parent.lines,
			breaks=c(term.id.lines,length(ontology.obo.lines)+1),
			labels=ontology$id
		)
	)[ontology$id]
	
	ontology.alt_ids <- sub(
		term.alt_id.pattern,
		"\\1",
		ontology.obo.lines[term.alt_id.lines]
	)

	ontology$alt_id <- as.character( 
		cut(
			term.alt_id.lines,
			breaks=c(term.id.lines,length(ontology.obo.lines)+1),
			labels=ontology$id
		)
	)	

	names(ontology$alt_id) <- ontology.alt_ids

	if (include_descriptions) {
		ontology$def <- sub(
			"^def: (\\.*)",
			"\\1", 
			ontology.obo.lines[grep("^def: (\\.*)", ontology.obo.lines)]
		)

		names(ontology$def) <- as.character( 
			cut(
				grep("^def: (\\.*)", ontology.obo.lines),
				breaks=c(term.id.lines,length(ontology.obo.lines)+1),
				labels=ontology$id
			)
		)

		Encoding(ontology$def) <- "latin1"
		ontology$def <- iconv(
			ontology$def,
			"latin1",
			"ASCII",
			sub=""
		)

		ontology$def <- setNames(nm=ontology$id, ontology$def[ontology$id])
	}

	names(ontology$id) <- ontology$id

	ontology$children <- c(
		lapply(FUN=as.character, X=split(
			unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, names(ontology$parents), sapply(ontology$parents, length))),
			unlist(use.names=FALSE, ontology$parents)
		)),
		setNames(nm=setdiff(ontology$id, unlist(use.names=FALSE, ontology$parents)), rep(list(character(0)), length(setdiff(ontology$id, unlist(use.names=FALSE, ontology$parents)))))
	)[ontology$id]

	ontology$ancestors <- str_ancs_from_pars(ontology$parents, ontology$children)

	ontology$version <- ontology.obo.lines[1:11]

	structure(ontology, class="ontology_index")
}

#' Remove alternate/deprecated term IDs and swap for new ones
#'
#' @template ontology
#' @template terms
#' @param remove_unmatchable Boolean value determining whether to silently remove unmatchable terms. Throws an error if there exist terms which cannot be matched in the ontology, including via `alt_id' fields.
#' @return Character vector of terms which are compatible with the given ontology.
#' @examples
#' data(hpo)
#' force_compatibility(hpo, c("HP:0001873","nonsense term"))
#' @export
force_compatibility <- function(ontology, terms, remove_unmatchable=TRUE) {
	need.swap <- (!terms %in% ontology$id) | ontology$obsolete[terms] | (terms %in% names(ontology$alt_id))
	can_swap <- need.swap & (terms %in% names(ontology$alt_id[!ontology$obsolete[ontology$alt_id]]))
	terms[can_swap] <- ontology$alt_id[terms[can_swap]]
	if (!remove_unmatchable & any(need.swap & (!can_swap))) {
		stop(paste0("Unmatchable terms: ", paste0(collapse="; ", "'", terms[need.swap & !can_swap], "'")))
	}
	terms[!need.swap | can_swap]
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

#' \code{ontology_index} object encapsulating structure of the Gene Ontology (HPO) comprising a \code{list} of lists/vectors of properties of GO terms indexed by term ID
#' 
#' @name go 
#' @title GO index
#' @docType data
#' @format List of lists and vectors
NULL

#' \code{ontology_index} object encapsulating structure of the Human Phenotype Ontology (HPO) comprising a \code{list} of lists/vectors of properties of HPO terms indexed by term ID
#' 
#' @name hpo
#' @title HPO index
#' @docType data
#' @format List of lists and vectors
NULL

#' \code{ontology_index} object encapsulating structure of the Mammalian Phenotype Ontology (MPO) comprising a \code{list} of lists/vectors of properties of MPO terms indexed by term ID
#' 
#' @name mpo
#' @title MPO index
#' @docType data
#' @format List of lists and vectors
NULL

#' List containing cross-species ontology (MPO to HPO) information - character vectors of HPO terms indexed by associated MPO term IDs
#'
#' @name mpo_to_hpo
#' @title Mapping from MPO to HPO
#' @docType data
#' @format List of HPO terms per MPO term
NULL

