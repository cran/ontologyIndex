## ------------------------------------------------------------------------
library(ontologyIndex)
data(hpo)

## ----eval=FALSE----------------------------------------------------------
#  ontology <- get_ontology(file)

## ------------------------------------------------------------------------
get_term_ancestors(ontology=hpo, term="HP:0001873", as_names=FALSE)
get_term_ancestors(ontology=hpo, term="HP:0001873", as_names=TRUE)

## ---- echo=FALSE---------------------------------------------------------
data.frame(property=names(hpo), class=sapply(hpo, class), stringsAsFactors=FALSE, row.names=NULL)

## ------------------------------------------------------------------------
hpo$name["HP:0001873"]
hpo$id[grep(x=hpo$name, pattern="Thrombocytopenia")]
hpo$ancestors[["HP:0001873"]]
hpo$name[hpo$ancestors[["HP:0001873"]]]

## ------------------------------------------------------------------------
terms <- c("HP:0001871", "HP:0001873", "HP:0011877")
hpo$name[terms]
minimal_set(hpo, terms)

## ------------------------------------------------------------------------
get_ancestors(hpo, c("HP:0001873", "HP:0011877"))

## ------------------------------------------------------------------------
terms <- c("HP:0001873","HP:0000006")
hpo$name[terms]

mode_of_inheritance <- hpo$id[grep(x=hpo$name, pattern="Mode of inheritance")]
hpo$name[mode_of_inheritance]

#remove mode of inheritance branch
exclude_branches(ontology=hpo, branch_roots=mode_of_inheritance, terms=terms)

#prune down to mode of inheritance root
prune_branches(ontology=hpo, branch_roots=mode_of_inheritance, terms=terms)

#only mode of inheritance branch
intersection_with_branches(ontology=hpo, branch_roots=mode_of_inheritance, terms=terms)

## ------------------------------------------------------------------------
force_compatibility(hpo, c("HP:0001873","nonsense term"))

