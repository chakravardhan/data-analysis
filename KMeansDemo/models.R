# Implement 2 >> Code the model function here
library(klaR)
clusterData <- function (r_data=NULL,num_clusters=2){
  
  #implement clustering and return results
  kmodes(r_data,num_clusters,iter.max = 10,weighted = FALSE)
  
}
