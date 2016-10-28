#' @importFrom stats setNames
str_ancs_from_pars <- function(id, pars, chld) {
	stopifnot(all(sapply(list(pars, chld), function(x) is.null(names(x)) | identical(names(x), id))))
	int.pars <- c(split(as.integer(factor(unlist(use.names=FALSE, pars), levels=id)), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, id, sapply(pars, length)))), setNames(nm=setdiff(id, unlist(use.names=FALSE, pars)), rep(list(integer(0)), length(setdiff(id, unlist(use.names=FALSE, pars))))))[id]
	int.chld <- c(split(as.integer(factor(unlist(use.names=FALSE, chld), levels=id)), unlist(use.names=FALSE, mapply(SIMPLIFY=FALSE, FUN=rep, id, sapply(chld, length)))), setNames(nm=setdiff(id, unlist(use.names=FALSE, chld)), rep(list(integer(0)), length(setdiff(id, unlist(use.names=FALSE, chld))))))[id]

	setNames(nm=id, lapply(ancs_from_pars(
		int.pars,
		int.chld
	), function(x) id[x]))
}

ancs_from_pars <- function(pars, chld) {
	ancs <- as.list(seq(length(pars)))
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

#' Create \code{ontology_index} object from vectors and lists of term properties
#'
#' @param id Character vector of term IDs.
#' @param name Character vector of term labels.
#' @param parents List of character vectors of parents per term.
#' @template remove_missing
#' @param obsolete Logical vector indicating whether given terms are obsolete.
#' @param version Version information about the ontology.
#' @param ... Additional arguments, each of which should be either a vector or list of term properties. 
ontology_index <- function(id, name, parents, remove_missing=FALSE, obsolete=setNames(nm=id, rep(FALSE, length(id))), version=NULL, ...) {
	if (!((is.null(names(parents)) & length(parents) == length(id)) | identical(names(parents), id))) {
		stop("`parents` argument must have names attribute identical to `id` argument or be the same length")
	}
	if (remove_missing) parents <- lapply(parents, intersect, id)
	children <- c(
		lapply(FUN=as.character, X=split(
			unlist(use.names=FALSE, rep(id, times=sapply(parents, length))),
			unlist(use.names=FALSE, parents)
		)),
		setNames(nm=setdiff(id, unlist(use.names=FALSE, parents)), rep(list(character(0)), length(setdiff(id, unlist(use.names=FALSE, parents)))))
	)[id]
	structure(lapply(FUN=setNames, nm=id, X=list(id=id, name=name, parents=parents, children=children, ancestors=str_ancs_from_pars(id, unname(parents), unname(children)), obsolete=obsolete, ...)), class="ontology_index", version=version)
}

#' Read ontology from obo file into R
#'
#' Reads an ontology from a file in either an OBO or OWL format into R as an \code{ontology_index} object.
#'
#' @param file File path of obo file
#' @param ... Additional arguments to pass onto \code{\link{get_OWL}} or \code{\link{get_OBO}}.
#' @return R-Object (list) representing ontology
#' @export
#' @importFrom stats setNames
#' @seealso \code{\link{get_OWL}} \code{\link{get_OBO}}
get_ontology <- function(file, ...) {
	if (length(file) != 1)
		stop("Pass character vector of length 1 as file argument")
	if (toupper(substr(file, nchar(file)-3, nchar(file))) == ".OWL")
		get_OWL(file, ...)
	else
		get_OBO(file, ...)
}

#' Read ontology from OBO file into R
#'
#' @param file File path of obo file.
#' @param term_regexp Regular expression for which designates the beginning of the specification of a new term in the OBO file. See \code{\link{grep}} for details on formatting the expression.
#' @param id Regular expression for matching term IDs.
#' @param name Regular expression for matching human readable term labels. 
#' @param parents Regular expression for matching against parents for each term. 
#' @param obsolete Regular expression for extracting whether terms are obsolete.
#' @template remove_missing
#' @param ... Additional functions for extracting properties of individual nodes. Arguments should be named (such that the name of the argument becomes the name of the slot in the resulting \code{ontology_index} object), and character vectors of length 1 or 2: the first element being a regular expression, the second specifying the \code{"mode"} of the array to become the property in the returned index (see the \code{"mode"} argument of \code{\link{vector}} for more details).
#' @return \code{ontology_index} object.
#' @export
#' @importFrom stats setNames
get_OBO <- function(
	file, 
	term_regexp="^\\[(Term|Typedef)\\]", 
	id="^id: ([^ !]+).*",
	name="^name: (\\.*)",
	parents=c("^is_a: ([^ !]+).*", "list"),
	obsolete=c("^is_obsolete: (true)","logical"),
	remove_missing=FALSE,
	...
) {
	lines <- readLines(file)

	term_lines <- grep(pattern=term_regexp, x=lines)

	property_patterns <- list(id=id, name=name, parents=parents, obsolete=obsolete, ...)

	properties <- lapply(property_patterns, function(p) { sel <- grep(x=lines, pattern=p[1]); out <- unname(as.vector(mode=if (length(p) > 1) p[2] else "character", x=split(sub(p[1], "\\1", lines[sel]), cut(sel, breaks=c(term_lines, Inf), labels=seq(length(term_lines)))))); if (length(p) > 1) { if (p[2] == "logical") !is.na(out) & out else out } else { out } })
	do.call(what=ontology_index, c(list(remove_missing=remove_missing, version=lines[seq(11)]), properties))
}

#' @title Create lists of attributes per node in OWL file 
#' @rdname owl_list_attributes
#' @name OWL_list_attributes
#' @param nodes \code{xml_nodeset} object where nodes correspond to ontological terms.
#' @param xpath XPath expression.
#' @param attribute Name of attribute to extract for selected nodes.
#' @seealso \code{\link{OWL_strings_from_nodes}}
#' @description \code{\link{OWL_is_a}}, \code{\link{OWL_part_of}} and \code{\link{OWL_is_a_and_part_of}} return lists of attributes for ontological terms represented as a \code{xml_nodeset} (see \code{xml2} package for more details on \code{xml_nodesets}). \code{\link{OWL_is_a}} returns the superclass/`is a' term IDs for each node in the given nodes, and is the default value of the \code{parents} argument in the \code{\link{get_OWL}} function. \code{\link{OWL_list_attributes_per_node}} returns a function for extracting lists of attributes of subnodes for each node in a \code{xml_nodeset}. It accepts an XPath expression for selecting the subnodes of each node in the returned function's \code{nodes} argument, and an attribute name argument for specifying which attribute should be extracted for each one. The returned function should be suitable for use as a parameter to the \code{\link{get_OWL}} function.
NULL

#' @rdname owl_list_attributes
#' @export
OWL_list_attributes_per_node <- function(xpath, attribute) {
	force(xpath)
	force(attribute)
	function(nodes) {
		par_node <- xpath; par_count <- paste0("count(", par_node, ")"); split(xml2::xml_attr(x=xml2::xml_find_all(x=nodes, xpath=par_node), attr=attribute, ns=xml2::xml_ns(nodes)), factor(rep(seq_along(nodes), times=xml2::xml_find_num(x=nodes, xpath=par_count)), levels=seq_along(nodes)))
	}
}

#' @rdname owl_list_attributes
#' @export
OWL_is_a <- OWL_list_attributes_per_node(xpath="rdfs:subClassOf[@rdf:resource]", attribute="rdf:resource")

#' @rdname owl_list_attributes
#' @export
OWL_part_of <- OWL_list_attributes_per_node(xpath="rdfs:subClassOf/owl:Restriction/owl:onProperty[@rdf:resource='http://purl.obolibrary.org/obo/BFO_0000050']/../owl:someValuesFrom", attribute="rdf:resource")

#' @rdname owl_list_attributes
#' @export
OWL_is_a_and_part_of <- function(nodes) lapply(FUN=unique, mapply(SIMPLIFY=FALSE, FUN=c, OWL_is_a(nodes), OWL_part_of(nodes)))

#' @title Get vectors of term properties from set of OWL nodes 
#' @rdname owl_properties
#' @name OWL_properties
#' @param nodes \code{xml_nodeset} object where nodes correspond to ontological terms.
#' @param xpath XPath expression.
#' @description Functions which return vectors of properties for OWL formatted ontological terms represented as \code{xml_nodeset}s. \code{\link{OWL_strings_from_nodes}} accepts an XPath expression (a \code{character} vector of length 1) and returns the string value of the XPath expression evaluated for each node (i.e. for each term in the ontology). One can use \code{\link{OWL_list_attributes_per_node}} to create functions for extracting lists of term attributes for each node of encoded in the OWL file. 
#' @seealso \code{\link{OWL_list_attributes_per_node}}
NULL

#' @export
#' @rdname owl_properties
OWL_strings_from_nodes <- function(xpath) {
	force(xpath)
	function(nodes) xml2::xml_find_chr(x=nodes, xpath=xpath)
}

#' @export 
#' @rdname owl_properties
OWL_IDs <- OWL_strings_from_nodes("string(@rdf:about)")

#' @export
#' @rdname owl_properties
OWL_labels <- OWL_strings_from_nodes("string(rdfs:label)")

#' @export
#' @rdname owl_properties
OWL_obsolete <- function(nodes) xml2::xml_find_chr(x=nodes, xpath="string(owl:deprecated)") == "true"

#' Read ontology as \code{ontology_index} from OWL format file
#'
#' @param file Name of OWL formatted file.
#' @param class_xpath XPath expression for selecting the nodes corresponding to terms in the ontology.
#' @param id Function for pulling term IDs out of a \code{nodeset} object.
#' @param name Function for pulling term labels out of a \code{nodeset} object. 
#' @param parents Function for selecting term parent IDs from for each term in a \code{nodeset} object. Defaults to \code{\link{OWL_is_a}}.
#' @param obsolete Function for selecting indicator of obsolescence for each term.
#' @param version_xpath XPath expression for selecting node which contains version information.
#' @template remove_missing
#' @param ... Additional (named) arguments, each a function accepting a \code{nodeset} object as an argument and each returning an array of values corresponding to the terms of the ontology.
#' @return \code{ontology_index} object.
#' @export
get_OWL <- function(
	file, 
	class_xpath="owl:Class[@rdf:about]", 
	id=OWL_IDs,
	name=OWL_labels,
	parents=OWL_is_a,
	obsolete=OWL_obsolete,
	version_xpath="owl:Ontology",
	remove_missing=FALSE,
	...
) {
	if (!requireNamespace("xml2", quietly = TRUE)) {
		stop("Please install the 'xml2' package to use this function.",
		  call. = FALSE)
	}
	funcs <- list(id=id, name=name, parents=parents, obsolete=obsolete, ...)
	doc <- xml2::read_xml(file)
	ns <- xml2::xml_ns(doc)
	classes <- xml2::xml_find_all(x=doc, ns=ns, xpath=class_xpath)
	properties <- lapply(funcs, function(f) unname(f(classes)))
	version_nodes <- xml2::xml_children(xml2::xml_find_first(x=doc, xpath="owl:Ontology", ns=ns))
	version_text <- xml2::xml_text(version_nodes)
	version <- paste0(xml2::xml_name(version_nodes),": ", version_text)[nchar(version_text) > 0]
	do.call(what=ontology_index, c(list(remove_missing=remove_missing, version=version), properties))
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

#' Perform simple consistency checks on \code{ontology_index} object 
#'
#' @template ontology
#' @param stop_if_invalid Logical value determining whether the function should call \code{stop} and print an error message upon finding that the given \code{ontology_index} is in valid.
#' @export
check <- function(ontology, stop_if_invalid=FALSE) {
	required <- c("id","parents","children","ancestors")
	#all crucial attributes must be present and of correct class
	if ("obsolete" %in% names(ontology))
		if (class(ontology[["obsolete"]]) != "logical")
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
