## ----eval=FALSE----------------------------------------------------------
#  ontology <- get_ontology(file)

## ----eval=FALSE----------------------------------------------------------
#  ontology <- get_OBO(file, definition=c("^def: (\\.*)", "character"))

## ----eval=FALSE----------------------------------------------------------
#  ontology <- get_OWL(file, part_of=OWL_part_of)

## ----eval=FALSE----------------------------------------------------------
#  term_url <- "http://purl.obolibrary.org/obo/(\\w+)_(\\d+)"
#  for (property in names(ontology)) {
#  	if (is.character(unlist(use.names=FALSE, ontology[[property]]))) ontology[[property]] <- if (is.list(ontology[[property]])) lapply(ontology[[property]], function(term_properties) gsub(x=term_properties, pattern=term_url, replacement="\\1:\\2")) else gsub(x=ontology[[property]], pattern=term_url, replacement="\\1:\\2")
#  	names(ontology[[property]]) <- gsub(x=names(ontology[[property]]), pattern=term_url, replacement="\\1:\\2")
#  }

## ----eval=FALSE----------------------------------------------------------
#  ontology$number_of_children <- sapply(ontology$children, length)

