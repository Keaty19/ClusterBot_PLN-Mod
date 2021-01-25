# source
source("random_gene.R")
source("convert_gene_symbol_ens.R")
source("compute_clustering.R")

setwd("/Users/dpag0499/OneDriveCardiffUniversity/Documents/PROJECTS/Sam/PLN_scripts")

# Parameters
f_gene="list_gene_symbol"
f_link="wl_general.final.scale.ord"
f_cds="hs_68_cds_max_mean_length"


# Fixed Parameters
nb_gene=100 # nb gene used as random gene set
nb_link=1000000 # nb link considered in the network
nb_sim=100

# Get list genes
gene=read.table(f_gene,h=F)
gene<-as.character(gene$V1)
gene_ens<-convert_gene_symbol_to_ens(gene)

# Match gene in term of cds length
df_random_gene<-get_random_gene(gene_ens,f_link,nb_link,f_cds,nb_gene)
gene_ens2<-colnames(gene_ens)


# compute clustering
list_val<-compute_clustering(f_link,nb_link,df_random_gene,nb_sim)
