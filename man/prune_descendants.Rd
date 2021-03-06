% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descendants.R
\name{prune_descendants}
\alias{prune_descendants}
\title{Exclude terms descending from a given set of roots but include those roots which were originally implicitly present.}
\usage{
prune_descendants(ontology, roots, terms)
}
\arguments{
\item{ontology}{\code{ontology_index} object.}

\item{roots}{Character vector of IDs for terms with respect to which descendants are to be defined.}

\item{terms}{Character vector of ontological terms.}
}
\value{
Character vector of terms
}
\description{
Given two sets of terms, \code{roots} and \code{terms}, construct a set of terms containing those in \code{terms} which do not descend from any term in \code{roots}, and also any terms in \code{roots} which are ancestors of any term in \code{terms}.
}
\seealso{
\code{\link{exclude_descendants}}, \code{\link{intersection_with_descendants}}
}
