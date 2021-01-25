
convert_gene_symbol_to_ens<-function(gene) {
  # package
  suppressMessages(library(biomaRt))
  
  # convert gene symbol to ensembl gene
  ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
  query_ensembl<-getBM(attributes=c('ensembl_gene_id','hgnc_symbol'), filters ='hgnc_symbol', values =gene, mart = ensembl)
  list_gene_ens<-unique(query_ensembl$ensembl_gene_id)

  return (list_gene_ens)
}


convert_gene_ens_to_symb<-function(gene) {
  
  # package
  suppressMessages(library(biomaRt))
  
  # convert gene symbol to ensembl gene
  ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
  query_ensembl<-getBM(attributes=c('ensembl_gene_id','hgnc_symbol'), filters ='ensembl_gene_id', values =gene, mart = ensembl)
  list_gene_symb<-unique(query_ensembl$ensembl_gene_id)
  
  return (list_gene_symb)
}
