\name{ontologyIndex-package}
\alias{ontologyIndex-package}
\alias{ontologyIndex}
\docType{package}
\title{
	Functions for Reading Ontologies into R	
}
\description{
Functions for reading ontologies into R and manipulating sets of ontological terms.
}
\details{
\tabular{ll}{
Package: \tab ontologyIndex\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2016-07-20\cr
License: \tab GPL (>= 2)\cr
}
The key functions include \code{\link{get_ontology}} for creating the index, \code{\link{get_ancestors}} for computing the set of all terms which are ancestors of at least one from set of terms and \code{\link{get_descendants}} for getting all terms which descend from a term. The package comes with R objects \code{hpo}, \code{mpo} and \code{go} which index the Human Phenotype Ontology, Mammalian Phenotype Ontology and Gene Ontology respectively.
}
\author{
Daniel Greene <dg333@cam.ac.uk>

Maintainer: Daniel Greene <dg333@cam.ac.uk>
}
\references{
Westbury SK, Turro E, Greene D, Lentaigne C, Kelly AM, Bariana TK, Simeoni I, Pillois X, Attwood A, Austin S, Jansen SB, Bakchoul T, Crisp-Hihn A, Erber WN, Favier R, Foad N, Gattens M, Jolley JD, Liesner R, Meacham S, Millar CM, Nurden AT, Peerlinck K, Perry DJ, Poudel P, Schulman S, Schulze H, Stephens JC, Furie B, Robinson PN, Geet Cv, Rendon A, Gomez K, Laffan MA, Lambert MP, Nurden P, Ouwehand WH, Richardson S, Mumford AD and Freson K (2015). `Human phenotype ontology annotation and cluster analysis to unravel genetic defects in 707 cases with unexplained bleeding and platelet disorders.' _Genome Med_, *7*(1), pp. 36.

Kohler S, Doelken SC, Mungall CJ, Bauer S, Firth HV, Bailleul-Forestier I, Black GC, Brown DL, Brudno M, Campbell J, FitzPatrick DR, Eppig JT, Jackson AP, Freson K, Girdea M, Helbig I, Hurst JA, Jahn J, Jackson LG, Kelly AM, Ledbetter DH, Mansour S, Martin CL, Moss C, Mumford A, Ouwehand WH, Park SM, Riggs ER, Scott RH, Sisodiya S, Van Vooren S, Wapner RJ, Wilkie AO, Wright CF, Vulto-van Silfhout A, de Leeuw N, de Vries B, Washingthon NL, Smith CL, Westerfield M, Schofield P, Ruef BJ, Gkoutos GV, Haendel M, Smedley D, Lewis SE and Robinson PN (2014). `The Human Phenotype Ontology project: linking molecular biology and disease through phenotype data.' _Nucleic Acids Res._, *42*(Database issue), pp. D966-974.

Ashburner et al. Gene ontology: tool for the unification of biology (2000) Nat Genet 25(1):25-9

Smith, Cynthia L., Carroll-Ann W. Goldsmith, and Janan T. Eppig. `The Mammalian Phenotype Ontology as a tool for annotating, analyzing and comparing phenotypic information.' Genome biology 6.1 (2004): 1.
}
