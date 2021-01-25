

get_random_gene<-function(gene,f_link,nb_link,f_cds,nb_gene) {

  library(data.table)
  
  # get links
  score_link<-read.table(f_link,h=F,nrows=nb_link)
  colnames(score_link)<-c("gene1","gene2","weight")
  
  # get cds lenght
  df_cds<-read.table(f_cds,h=F)
  colnames(df_cds)<-c("gene","cds1","cds2")
  
  
  # score network
  df_link1<-score_link[c(1,3)]
  colnames(df_link1)<-c("gene","weight")
  df_link2<-score_link[,c(2,3)]
  colnames(df_link2)<-c("gene","weight")
  df_link<-rbind(df_link1,df_link2)
  df_link <- as.data.table(df_link)
  df_score<-df_link[, sum(weight), by = gene]
  colnames(df_score)<-c("gene","score_net")
  
  
  # general table cds & score weight
  df_cds <- as.data.table(df_cds)
  df_score_cds<-merge(df_score,df_cds,by=c("gene"))
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  df_score_cds$score_net_norm<-range01(df_score_cds$score_net)
  df_score_cds$cds1_norm<-range01(df_score_cds$cds1)
  
  # get random gene: gene with same propriety in term pln connectivity and cds lenght
  get_list_random_gene<-function(gene_ref,df_score_cds){
    index<-which(df_score_cds$gene==gene_ref)
    df_temp<-df_score_cds
    df_temp$score_cds<-abs(df_temp$cds1_norm-df_temp$cds1_norm[index])
    df_temp$score_net<-abs(df_temp$score_net_norm-df_temp$score_net_norm[index])
    df_temp$total<-df_temp$score_cds+df_temp$score_net
    df_temp_ord<-df_temp[order(df_temp$total),]
    list_gene<-as.character(df_temp_ord$gene[c(2:(nb_gene+1))])
    return(list_gene)
  }
  
  
  list_gene<-as.character(df_score_cds$gene[df_score_cds$gene %in% gene_ens])
  df_random_gene<-sapply(list_gene,get_list_random_gene,df_score_cds=df_score_cds)
  df_random_gene<-as.data.frame(df_random_gene)
  colnames(df_random_gene)<-list_gene
  return (df_random_gene)
}
  





