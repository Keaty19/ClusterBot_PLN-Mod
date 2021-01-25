compute_clustering<-function(f_link,nb_link,df_random_gene,nb_sim) {

  
  # get links
  score_link<-read.table(f_link,h=F,nrows=nb_link)
  colnames(score_link)<-c("gene1","gene2","weight")
  
  # score network
  calculate_score_gene_set<-function(list_gene,score_link) {
    index<-which(score_link$gene1 %in% list_gene & score_link$gene2 %in% list_gene)
    return(sum(score_link$weight[index]))
  }
  
  score_obs<-calculate_score_gene_set(colnames(df_random_gene),score_link=score_link)
  
  get_subset<-function(index,comb,df_random_gene) {
    comb
    a<-as.character(df_random_gene[comb[index,1],comb[index,2]])
    return(a)
  }
  get_sim_score<-function(sim,df_random_gene,score_link) {
    index_random<-sample(1:nb_gene,dim(df_random_gene)[2],replace=T)
    comb<-data.frame(i1=index_random,i2=c(1:dim(df_random_gene)[2]))
    list_gene<-sapply(c(1:dim(comb)[1]),get_subset,comb=comb,df_random_gene=df_random_gene)
    score<-calculate_score_gene_set(list_gene,score_link)
    return (score)
  }
  
  list_score_sim<-sapply(c(1:nb_sim),get_sim_score,df_random_gene=df_random_gene,score_link=score_link)
  
  pvalue<-length(which(score_obs < list_score_sim))/nb_sim
  return(list(pvalue,score_obs,list_score_sim))
}
