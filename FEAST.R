#必须使用R3.6.3，否则有的包装不上。
#加载必要的包
library("vegan")
library("dplyr")
library("doParallel")
library("foreach")
library("mgcv")
library("reshape2")
library("ggplot2")
library("cowplot")
library("Rcpp")
library("RcppArmadillo")
library("ggrepel")
#准备
rm(list = ls())
gc()

#所有需要设置的变量，共3个
#Set the arguments of your data设置计算数据，最好都统一为csv格式
metadata_file = "gutsource.csv" #分组信息 #这里必须改名字，为具体你写的名。
count_matrix = "GUT-SOIL.csv" #otu表
EM_iterations = 1000 #default value=1000
##if you use different sources for each sink, different_sources_flag = 1, otherwise = 0
different_sources_flag = 0


# Load main code加载主程序
print("Change directory path")
dir_path = paste("C:/Users/zhangqi/Desktop/FEAST/") #修改成FEAST文件夹所在目录*最后别漏了/
setwd(paste0(dir_path, "FEAST_src"))
source("src.R") #提前在R session里改路径，路径一定是有src.R文件的路径
#R版本最好是3.6.3，高了运行不了。

# Load sample metadata加载数据，仍旧统一为csv格式, *再次加载罢了
setwd(paste0(dir_path, "Data_files"))
metadata <- read.csv(file.choose(), header=T, sep = ",", row.names = 1)
# Load OTU table
otus <- read.csv(file.choose(), header=T, sep = ",", row.names = 1)
otus <- t(as.matrix(otus))


#计算过程，不用管
# Extract only those samples in common between the two tables
common.sample.ids <- intersect(rownames(metadata), rownames(otus))
otus <- otus[common.sample.ids,]
metadata <- metadata[common.sample.ids,]
# Double-check that the mapping file and otu table
# had overlapping samples
if(length(common.sample.ids) <= 1) {
  message <- paste(sprintf('Error: there are %d sample ids in common '),
                   'between the metadata file and data table')
  stop(message)
}


if(different_sources_flag == 0){
  
  metadata$id[metadata$SourceSink == 'Source'] = NA
  metadata$id[metadata$SourceSink == 'Sink'] = c(1:length(which(metadata$SourceSink == 'Sink')))
}


envs <- metadata$Env
Ids <- na.omit(unique(metadata$id))
Proportions_est <- list()


for(it in 1:length(Ids)){
  
  
  # Extract the source environments and source/sink indices
  if(different_sources_flag == 1){
    
    train.ix <- which(metadata$SourceSink=='Source' & metadata$id == Ids[it])
    test.ix <- which(metadata$SourceSink=='Sink' & metadata$id == Ids[it])
    
  }
  
  else{
    
    train.ix <- which(metadata$SourceSink=='Source')
    test.ix <- which(metadata$SourceSink=='Sink' & metadata$id == Ids[it])
  }
  
  num_sources <- length(train.ix)
  COVERAGE =  min(rowSums(otus[c(train.ix, test.ix),]))  #Can be adjusted by the user
  str(COVERAGE)
  # Define sources and sinks
  
  sources <- as.matrix(rarefy(otus[train.ix,], COVERAGE))
  sinks <- as.matrix(rarefy(t(as.matrix(otus[test.ix,])), COVERAGE))
  
  
  print(paste("Number of OTUs in the sink sample = ",length(which(sinks > 0))))
  print(paste("Seq depth in the sources and sink samples = ",COVERAGE))
  print(paste("The sink is:", envs[test.ix]))
  
  # Estimate source proportions for each sink
  
  FEAST_output<-FEAST(source=sources, sinks = t(sinks), env = envs[train.ix], em_itr = EM_iterations, COVERAGE = COVERAGE)
  Proportions_est[[it]] <- FEAST_output$data_prop[,1]
  
  
  names(Proportions_est[[it]]) <- c(as.character(envs[train.ix]), "unknown")
  
  if(length(Proportions_est[[it]]) < num_sources +1){
    
    tmp = Proportions_est[[it]]
    Proportions_est[[it]][num_sources] = NA
    Proportions_est[[it]][num_sources+1] = tmp[num_sources]
  }
  
  print("Source mixing proportions")
  print(Proportions_est[[it]])
  
  
}

print(Proportions_est)#原版仅可得到这个数据，可视化程度较差
write.csv(Proportions_est, 'Proportions_estGUT.csv', quote = FALSE)
