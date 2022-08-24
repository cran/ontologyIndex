#' @importFrom stats setNames
str_ancs_from_pars <- function(id, pars, chld) {
	stopifnot(all(sapply(list(pars, chld), function(x) is.null(names(x)) | identical(names(x), id))))
	int.pars <- c(split(as.integer(factor(unlist(use.names=FALSE, pars), levels=id)), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, id, sapply(pars, length)))), setNames(nm=setdiff(id, unlist(use.names=FALSE, pars)), rep(list(integer(0)), length(setdiff(id, unlist(use.names=FALSE, pars))))))[id]
	int.chld <- c(split(as.integer(factor(unlist(use.names=FALSE, chld), levels=id)), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, id, sapply(chld, length)))), setNames(nm=setdiff(id, unlist(use.names=FALSE, chld)), rep(list(integer(0)), length(setdiff(id, unlist(use.names=FALSE, chld))))))[id]

	setNames(nm=id, lapply(ancs_from_pars(
		int.pars,
		int.chld,
		id
	), function(x) id[x]))
}

ancs_from_pars <- function(pars, chld, id) {
	ancs <- as.list(seq(length(pars)))
	done <- sapply(pars, function(x) length(x) == 0)
	cands <- which(done)
	new.done <- 1:length(cands)
	carry_over <- integer(0)
	while (!all(done)) {
		cands <- unique(c(carry_over, as.integer(unlist(use.names=FALSE, chld[cands[new.done]]))))
		v <- sapply(pars[cands], function(x) all(done[x]))
		if (!is.logical(v)) {
			stop("Can't get ancestors for items ", paste0(collapse=", ", which(!done)))
		}
		new.done <- which(v)
		carry_over <- cands[!v]
		done[cands[new.done]] <- TRUE
		ancs[cands[new.done]] <- mapply(SIMPLIFY=FALSE, FUN=c, lapply(cands[new.done], function(x) unique(unlist(use.names=FALSE, ancs[pars[[x]]]))), cands[new.done])
		if (length(new.done) == 0L) stop(sprintf("Cycle detected in ontology! See ancestors of %s", id[carry_over[1]]))
	}
	ancs
}

#' Create \code{ontology_index} object from vectors and lists of term properties
#'
#' @param parents List of character vectors of parents per term.
#' @param id Character vector of term IDs. Defaults to the \code{"names"} attribute of the \code{parents} argument and must be the same length as \code{parents}.
#' @param name Character vector of term labels.
#' @param obsolete Logical vector indicating whether given terms are obsolete.
#' @param version Version information about the ontology.
#' @param ... Additional arguments, each of which should be either a vector or list of term properties, each with the same length as \code{id}. 
#' @export
#' @examples
#' animal_superclasses <- list(animal=character(0), mammal="animal", cat="mammal", fish="animal")
#' animal_ontology <- ontology_index(parents=animal_superclasses)
#' unclass(animal_ontology)
ontology_index <- function(parents, id=names(parents), name=id, obsolete=setNames(nm=id, rep(FALSE, length(id))), version=NULL, ...) {
	if (is.null(id))
		stop("Must give non-NULL term IDs: either as 'id' argument or as the names of the 'parents' argument")
	if (!is.character(id))
		stop("'id' argument must be of class 'character'")

	if (!((is.null(names(parents)) & length(parents) == length(id)) | identical(names(parents), as.character(id)))) {
		stop("`parents` argument must have names attribute identical to `id` argument or be the same length")
	}
	missing_terms <- setdiff(unlist(use.names=FALSE, parents), id)
	if (length(missing_terms) > 0) {
		warning(paste0("Some parent terms not found: ", paste0(collapse=", ", missing_terms[seq(min(length(missing_terms), 3))]), if (length(missing_terms) > 3) paste0(" (", length(missing_terms)-3, " more)") else ""))
		parents <- lapply(parents, intersect, id)
	}

	children <- c(
		lapply(FUN=as.character, X=split(
			unlist(use.names=FALSE, rep(id, times=sapply(parents, length))),
			unlist(use.names=FALSE, parents)
		)),
		setNames(nm=setdiff(id, unlist(use.names=FALSE, parents)), rep(list(character(0)), length(setdiff(id, unlist(use.names=FALSE, parents)))))
	)[id]
	structure(lapply(FUN=setNames, nm=id, X=list(id=id, name=name, parents=parents, children=children, ancestors=str_ancs_from_pars(id, unname(parents), unname(children)), obsolete=obsolete, ...)), class="ontology_index", version=version)
}

term_regexp <- "^\\[(Term|Typedef|Instance)\\]"
tag_regexp <- "^(relationship: )?([^ \t]*[^:]):?\\s+(.+)"

#' Get names of relations used in OBO file
#'
#' @param file File path of OBO formatted file.
#' @export
#' @seealso \code{\link{get_ontology}}
get_relation_names <- function(file) {
	lines <- grep(value=TRUE, pattern=tag_regexp, x=readLines(file))
	parts <- regmatches(x=lines, regexec(text=lines, pattern=tag_regexp))
	relation <- !sapply(parts, "[", 2) == ""
	c(intersect("is_a", unique(sapply(parts[!relation], "[", 3))), unique(sapply(parts[relation], "[", 3)))
}

#' Read ontology from OBO file into R
#'
#' @param file File path of OBO formatted file.
#' @param propagate_relationships Character vector of relations 
#' @param extract_tags Character value: either "minimal" or "everything", determining whether to extract only the properties of terms which are required to run functions in the package - i.e. \code{"id", "name", "parents", "children"} and \code{"ancestors"} - or extract all properties provided in the file. Term properties are named in the resulting \code{ontology_index} as their corresponding tags in the OBO file (except \code{"parents"}, \code{"children"} and \code{"ancestors"} which are appended with \code{"_OBO"} to avoid clashing with standard \code{ontology_index} properties. Defaults to \code{"minimal"}.
#' @param merge_equivalent_terms Logical value determining whether terms that are marked \code{"equivalent_to"} a target term should be merged, retaining properties of the target term when the property should have one value, e.g. the term ID and name. Defaults to \code{TRUE}.
#' @return \code{ontology_index} object.
#' @export
#' @seealso \code{\link{get_relation_names}}
get_ontology <- function(
	file, 
	propagate_relationships="is_a",
	extract_tags="minimal",
	merge_equivalent_terms=TRUE
) {
	raw_lines <- readLines(file)
	#remove comments and trailing modifiers
	m <- regexpr(text=raw_lines, pattern="^([^!{]+[^!{ \t])")
	lines <- regmatches(x=raw_lines, m=m)

	term_lines <- grep(pattern=term_regexp, x=lines)
	
	if (length(term_lines) == 0) stop("No terms detected in ontology source")

	if (!extract_tags %in% c("minimal", "everything"))
		stop("'extract_tags' argument should be equal to either 'minimal' or 'everything'")

	minimal <- extract_tags == "minimal"

	term_lines <- grep(pattern=term_regexp, x=lines)

	tagged_lines <- grep(pattern=tag_regexp, x=lines)

	tag_matches <- regmatches(x=lines[tagged_lines], regexec(text=lines[tagged_lines], pattern=tag_regexp))
	tags <- sapply(tag_matches, "[", 3)
	values <- sapply(tag_matches, "[", 4)

	all_present_tag_types <- unique(tags)
	use_tags <- if (minimal) intersect(c("id", "name", "is_obsolete"), all_present_tag_types) else all_present_tag_types

	tag_lines <- which(tags %in% c(propagate_relationships, use_tags, "equivalent_to"))

	properties <- mapply(
		SIMPLIFY=FALSE,
		FUN=function(vals, lns) {
			unname(split(vals, cut(lns, breaks=c(term_lines, Inf), labels=seq(length(term_lines)))))
		},
		split(values[tag_lines], tags[tag_lines]),
		split(tagged_lines[tag_lines], tags[tag_lines])
	)

	ids_l <- properties[["id"]]
	if (any(lengths(ids_l) != 1)) { stop(sprintf("Term without exactly one id found: %s", ids_l[[which(lengths(ids_l)!=1)[1]]][1])) }
	ids <- simplify2array(ids_l)
	equivs <- if (merge_equivalent_terms && "equivalent_to" %in% names(properties)) {
		vapply(FUN.VALUE=character(1), X=properties[["equivalent_to"]], FUN=function(x) { if (length(x) > 1) stop(sprintf("Terms with multiple equivalent_to fields detected: %s", paste(collapse=", ", x))); if (length(x) == 0) NA_character_ else x })
	} else { rep(NA_character_, length(ids)) }
	if (!all(equivs[!is.na(equivs)] %in% ids)) {
		stop(sprintf("found 'equivalent_to' terms not listed: %s", paste0(collapse=", ", setdiff(equivs[!is.na(equivs)], ids))))
	}
	true_ids <- ids
	while (any(!is.na(equivs[match(true_ids, ids)]))) {
		has_equiv <- which(!is.na(equivs[match(true_ids, ids)]))
		true_ids[has_equiv] <- equivs[match(true_ids[has_equiv], ids)]
	}
	uids <- unique(true_ids)
	properties <- lapply(properties, function(x) split(as.character(unlist(x[order(!is.na(equivs))], use.names=FALSE)), f=factor(rep(true_ids[order(!is.na(equivs))], times=lengths(x[order(!is.na(equivs))])), levels=uids)))

	parents <- local({
		df <- do.call(what=rbind, lapply(properties[intersect(names(properties), propagate_relationships)], function(x) data.frame(stringsAsFactors=FALSE, p=true_ids[match(as.character(unlist(use.names=FALSE, x)), ids)], cd=rep(uids, times=lengths(x)))))
		odf <- (function(x) if (is.null(x)) data.frame(stringsAsFactors=FALSE, p=character(0), cd=character(0)) else x[!duplicated(x),])(df[df$p!=df$cd,])
		split(odf$p, factor(odf$cd, levels=uids))
	})

	simplify <- intersect(names(properties), c("id", "name", "def", "comment", "is_obsolete", "created_by", "creation_date"))

	properties[simplify] <- lapply(properties[simplify], function(lst) sapply(lst, "[", 1))

	names(properties) <- gsub(x=names(properties), pattern="^((equivalent_to)|(parents)|(children)|(ancestors))$", replacement="\\1_OBO")
	if (any(ids!=true_ids)) properties$equivalent_to <- split(ids[ids!=true_ids], factor(true_ids[ids!=true_ids], levels=uids))

	do.call(
		what=ontology_index, 
		c(
			list(
				version=substr(lines[seq(term_lines[1]-1)], 1, 1000),
				parents=parents,
				id=properties[["id"]],
				name=properties[["name"]],
				obsolete=if ("is_obsolete" %in% names(properties)) (!is.na(properties[["is_obsolete"]])) & properties[["is_obsolete"]] == "true" else rep(FALSE, length(properties[["id"]]))),
			properties[c(setdiff(use_tags, c("id","name","is_obsolete")), if (any(!is.na(equivs))) "equivalent_to")]))
}

#' @export
#' @rdname get_ontology
get_OBO <- get_ontology

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

#' Perform simple consistency checks on \code{ontology_index} object 
#'
#' @template ontology
#' @param stop_if_invalid Logical value determining whether the function should call \code{stop} and print an error message upon finding that the given \code{ontology_index} is in valid.
#' @export
check <- function(ontology, stop_if_invalid=FALSE) {
	required <- c("id","parents","children","ancestors")
	#all crucial attributes must be present and of correct class
	if ("obsolete" %in% names(ontology))
		if (!is.logical(ontology[["obsolete"]]))
			if (stop_if_invalid) stop("'obsolete' member must be logical")
			else return(FALSE)
	if (any(!(required %in% names(ontology))))
		if (stop_if_invalid) stop(paste0(collapse=", ", "'", setdiff(required, names(ontology)), "'"), " fields missing from index")
		else return(FALSE)
	#all properties must be named by ontology$id
	if (!all(sapply(lapply(ontology, names), identical, as.character(ontology[["id"]]))))
		if (stop_if_invalid) stop("Not all members stored by the ontology have names attribute equal to the $id element")
		else return(FALSE)
	#all crucial attributes which are term IDs must be contained in $id...
	for (attr in c("parents","children","ancestors")) {
		if (length(setdiff(unlist(use.names=FALSE, ontology[[attr]]), ontology[["id"]])) > 0)
			if (stop_if_invalid) stop(paste0("'", attr, "' has terms which are missing from 'id' member"))
			else return(FALSE)
	}
	#no duplicate ids
	if (length(unique(ontology[["id"]])) != length(ontology[["id"]]))
		if (stop_if_invalid) stop("Duplicate IDs present")
		else return(FALSE)

	TRUE
}
