library(randomForest)
library(pheatmap)
rf <- function(file,metadata1){
  
  set.seed(8)
  
  # otu_sub=read.csv("~/01rfshiny/tax_8OTU0.1.txt",header = 1,sep = '\t',row.names = 1)
  # metadata=read.csv("~/01rfshiny/metadata.txt",header = 1,sep = '\t')
  # 
  
  
  ######################
  set.seed(10)
  idx = sample(1:352, size = 124, replace = F)
  set.seed(10)
  idxx = sample(352:476, size = 124, replace = F)
  # 选择的样本标记为TRUE，未选择的为FALSE
  idx = 1:352 %in% idx
  idxx = 352:475 %in% idxx
  
  
  #442 117
  set.seed(01)
  idx = sample(1:442, size = 100, replace = F)
  set.seed(01)
  idxx = sample(443:559, size = 100, replace = F)
  # 选择的样本标记为TRUE，未选择的为FALSE
  idx = 1:442 %in% idx
  idxx = 443:559 %in% idxx
  
  
  
  #--------------------------------- 
  
  otu_sub=read.table("tax_8OTU0.1.txt",header = T,sep = '\t',row.names = 1)
  metadata=read.csv("metadata.txt",header = T,sep = '\t',row.names = 1)
  
  
  otu_sub=read.table("tax_wet_8OTU0.1.txt",header = T,sep = '\t',row.names = 1)
  metadata=read.csv("wet_meta.txt",header = T,sep = '\t',row.names = 1)
  
  file="tax_wet_8OTU0.1.txt"
  metadata1="wet_meta.txt"
  
  
  otu_sub=read.table("tax_8OTU0.1_add_wet.txt",header = T,sep = '\t',row.names = 1)
  metadata=read.csv("meta_tax_8OTU0.1_add_wet.txt",header = T,sep = '\t',row.names = 1)
  
  
  set.seed(100)
  idx = sample(1:352, size = 80, replace = F)
  set.seed(100)
  idxx = sample(353:466, size = 80, replace = F)
  # 选择的样本标记为TRUE，未选择的为FALSE
  idx = 1:352 %in% idx
  idxx = 353:466 %in% idxx
  #---------------------------
  #尝试
  otu_sub=read.table("tax_8OTU0.1.txt",header = T,sep = '\t',row.names = 1)
  metadata=read.csv("metadata.txt",header = T,sep = '\t',row.names = 1)
  set.seed(10)
  idx = sample(1:352, size = 190, replace = F)
  set.seed(10)
  idxx = sample(353:466, size = 114, replace = F)
  # 选择的样本标记为TRUE，未选择的为FALSE
  idx = 1:352 %in% idx
  idxx = 353:466 %in% idxx 
  
  
  
  
  
  idx_al = c(idx,idxx)
  tt = metadata
  # 再用这个索引idx筛选对应的数据表，一部分作为训练集(train)，另一部分作为测试集(test)
  metadata=metadata[idx_al,]
  # otu_sub=otu_sub[idx,]
  # test=metadata[!idx,]
  #########################
  
  
  
  # otu_sub=read.table("tax_temp_OTU_all_head_477",header = T,sep = '\t',row.names = 1)
  # metadata=read.csv("tax_temp_OTU_all_head_477_meta",header = T,sep = '\t',row.names = 1)
  # metadata = subset(metadata, Group  %in% c("HSL"))
  
  #group=metadata$Group
  #group=c('group',group)
  otu_sub = otu_sub[,rownames(metadata)] 
  otu_sub=data.frame(t(otu_sub)) 
  otu_sub_group <- cbind(otu_sub, metadata$Group)
  otu_sub_group=data.frame(otu_sub_group)
  
  rf = randomForest(data=otu_sub_group, as.factor(metadata.Group)~., importance=TRUE, proximity=TRUE, ntree = 1000)
  
  print(rf)
  
  
  
  ###########################
  for (i in 0:9){
    set.seed(i)
    rf = randomForest(data=otu_sub_group, as.factor(metadata.Group)~., importance=TRUE, proximity=TRUE, ntree = 1000)
    print(i)
    print(rf)
  }
  i=8
  set.seed(i)
  rf = randomForest(data=otu_sub_group, as.factor(metadata.Group)~., importance=TRUE, proximity=TRUE, ntree = 10000)
  print(i)
  print(rf)
  ####################
  
  file="tax_temp_OTU_all_tail_20"
  metadata1="tax_temp_OTU_all_tail_20_meta"
  
  
  file="otu_temp"
  metadata1="meta_temp"
  
  
  
  file="dry_otutab_rare.txt"
  metadata1="dry_metadata.txt"
  
  file="tax_dry_8OTU0.1.txt"
  metadata1="dry_metadata.txt"
  
  file="tax_wet_8OTU0.1.txt"
  metadata1="wet_meta.txt"
  set.seed(8)
  
  #交叉
  for (i in 1:(1+4)){
    print(i)
    set.seed(i)
    result= rfcv((otu_sub_group), metadata$Group, cv.fold=10) #  scale = "log", step = 0.9
    error.cv0 = cbind(error.cv0, result$error.cv)
  }
  error.cv0 
  #开始测试
  # otu_sub1=read.csv("~/01rfshiny/tax_8OTU0.1.txt",header = 1,sep = '\t',row.names = 1)
  # test1=otu_sub1[,2:45]
  # write.csv(aaa,'~/01rfshiny/testmetadata.csv')
  # aaa=metadata[2:45,]
  test=read.csv(file,header = 1,sep = ',',row.names = 1)
  
  
  
  test=data.frame(t(test))
  testmetadata=read.csv(metadata1,header = 1,sep = ',',row.names = 1)
  
  metadata1 = tt[!idx_al,]
  test = otu_sub[,rownames(metadata)] 
  # otu_sub=data.frame(t(otu_sub)) 
  
  
  otutab.pred = predict(rf, test )  
  pre_tab = table(observed=testmetadata[,"Group"],
                  predicted=otutab.pred) 
  print(pre_tab)
  ########################
  test=read.csv(file,header = TRUE,sep = '\t',row.names = 1)
  # 
  names_missing <- names(otu_sub)[!names(otu_sub) %in% names(test)]
  test[names_missing,] <- 0
  test=data.frame((test))
  testmetadata=read.csv(metadata1,header = 1,sep = '\t',row.names = 1)
  otutab.pred = predict(rf, t(test) )
  pre_tab = table(observed=testmetadata[,"Group"],
                  predicted=otutab.pred)
  print(pre_tab)
  
  
  
  #######################
  
  # test=read.csv('~/01rfshiny/www/test.csv',header = 1,sep = ',',row.names = 1)
  # testmetadata=read.csv('~/01rfshiny/www/testmetadata.csv',header = 1,sep = ',',row.names = 1)
  
  
  ######画热力图
  predict = data.frame(group = testmetadata[,"Group"], predicted=otutab.pred)
  
  predict$result = ifelse(predict$group == predict$predicted, 1, 0)
  
  if (sum(predict$result )== length(predict$result)){
    print("All predictions are correct !")
  }else{
    
    
    column = 10
    
    
    row = round(length(predict$result)/column + 0.5)
    
    matrix = matrix(predict$result, nrow = row, ncol = column, byrow = T)
    
    pheatmap(matrix, cluster_rows = F, cluster_cols = F, cellwidth = 15, cellheight = 12)
  }
}

