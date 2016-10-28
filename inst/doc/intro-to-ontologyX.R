## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(dev="svg", fig.width=7, fig.height=5, dev="svg", fig.align="center")

## ------------------------------------------------------------------------
library(ontologyIndex)
data(hpo)

## ----eval=FALSE----------------------------------------------------------
#  ontology <- get_ontology(file)

## ---- echo=FALSE---------------------------------------------------------
data.frame(property=names(hpo), class=sapply(hpo, class), stringsAsFactors=FALSE, row.names=NULL)

## ------------------------------------------------------------------------
get_term_property(ontology=hpo, property="ancestors", term="HP:0001873", as_names=TRUE)

## ------------------------------------------------------------------------
hpo$name["HP:0001873"]
hpo$id[grep(x=hpo$name, pattern="Thrombocytopenia")]
hpo$ancestors[["HP:0001873"]]
hpo$name[hpo$ancestors[["HP:0001873"]]]

## ------------------------------------------------------------------------
terms <- c("HP:0001871", "HP:0001873", "HP:0011877")
hpo$name[terms]
minimal <- minimal_set(hpo, terms)
hpo$name[minimal]

## ------------------------------------------------------------------------
get_ancestors(hpo, c("HP:0001873", "HP:0011877"))

## ----echo=FALSE----------------------------------------------------------
library(ontologyPlot)
in_set <- c("HP:0000707", "HP:0000924", "HP:0000006")
show <- c(in_set, "HP:0000118", "HP:0000001")
fns <- list(
	`Original set`=function(ontology, roots, terms) terms, 
	`intersection_with_descendants of 'Phenotypic abnormality'`=intersection_with_descendants,
	`exclude_descendants of 'Phenotypic abnormality'`=exclude_descendants,
	`prune_descendants of 'Phenotypic abnormality'`=prune_descendants
)
par(mfrow=c(2,2))
for (i in seq_along(fns)) plot(cex.main=0.8, main=names(fns)[i], onto_plot(ontology=hpo, label=official_labels(hpo, show), terms=show, color=if (i == 1) "transparent" else ifelse(show == "HP:0000118", "red", "transparent"), fillcolor=ifelse(show %in% fns[[i]](hpo, roots="HP:0000118", terms=in_set), "#66FF66", "#DBE6E0"), fontsize=15))
par(mfrow=c(1,1))

