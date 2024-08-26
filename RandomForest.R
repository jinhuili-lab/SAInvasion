#RF_classification_数据筛选和交叉筛选
---
  date: "`r Sys.Date()`"
output:
  html_document:
  df_print: paged
theme: cerulean
highlight: haddock
toc: yes
toc_depth: 3
toc_float:
  collapsed: no
smooth_scroll: yes
code_fold: show
---
  
  ```{r setup, include=T}
getwd()

knitr::opts_chunk$set(echo = TRUE)
# 检测和安装依赖包
package_list <- c("randomForest","ggplot2","pheatmap")
# 判断R包加载是否成功来决定是否安装后再加载
for(p in package_list){
  if(!suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    install.packages(p, repos=site)
    suppressWarnings(suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}
```



## 样本随机分组(可选)

注：数据可以自由分组，如选择50%-80%建模，其余部分验证。为提高模型准确性，可以整合更多来源的数据，如北京、上海中一半作模型，另一半做验证，有可能提高验证时预测的准确率。随机取样代码如下：

```{r}
# 假设从一百个样本中随机取70个，且无放回
set.seed(100)
idx = sample(1:466, size = 372, replace = F)
# 选择的样本标记为TRUE，未选择的为FALSE
idx = 1:466 %in% idx
# 再用这个索引idx筛选对应的数据表，一部分作为训练集(train)，另一部分作为测试集(test)
train=metadata[idx,]
test=metadata[!idx,]




######################################################

set.seed(10)
idx = sample(1:352, size = 80, replace = F)
set.seed(10)
idxx = sample(353:466, size = 80, replace = F)
# 选择的样本标记为TRUE，未选择的为FALSE
idx = 1:352 %in% idx
idxx = 353:466 %in% idxx
idx = c(idx,idxx)






## 分类级选择(可选)

先使用format2stamp.Rmd基于OTU表(otutab.txt)、物种注释(taxonomy.txt)和元数据(metadata.txt)筛选样本、高丰度特征，并分类汇总各分类级(tax_1-8)。然后对各分类级进行准确性评估

```{r}
# 读取实验设计、和物种分类文件
metadata = read.table("metadata.txt",header = T, row.names = 1)

# R4.0读取表不于默认为数据框
metadata$Group = as.factor(metadata$Group)
#######train$Group = as.factor(train$Group)
#######train=subset(train, Group  %in% c("HSL","SA"))
# 筛选"L"地点为训练集
metadata = subset(metadata, Group  %in% c("HSL","SA"))

# 物种分类文件，由usearch10 -sintax_summary生成，详见扩增子分析流程系列。但存在1对多时无法对应分类级颜色(如Unassigned可能属于多个门)，使用format2stamp.Rmd保留各级别名称
library(randomForest)

# "1Kingdom",界只有细菌、古菌类别太少；"7Species",扩增子中不太可信
for(i in c("2Phylum","3Class","4Order","5Family","6Genus","7Species","8OTU0.1")){
  i="8OTU0.1"
  set.seed(0)
  table = read.table(paste0("tax_",i,".txt"),header = T, row.names = 1)
  table = table[,rownames(metadata)]
  rf = randomForest(t(table), metadata$Group, importance=T, proximity=T, ntree = 1000)
  print(i)
  print(rf)
}

# 本次观察到科水平最准确，以后使用科水平分析，可以将筛选的结果做成拆线图作为附图
```

## 最佳水平数据读取和统计

读取实验设计、Feature表，并进行数据筛选和交叉筛选

```{r}
# 读取实验设计、和物种分类文件
metadata =read.table("metadata.txt",header = T, row.names = 1)


# R4.0读取表不于默认为数据框
metadata$Group = as.factor(metadata$Group)
metadata_train=metadata[idx,]
metadata_train$Group = as.factor(metadata_train$Group)
# 读取科水平特征表
table =read.table("tax_8OTU0.1.txt",header = T, row.names = 1)

# 筛选L样品作为训练集
metadata_train = subset(metadata, Group %in% c("HSL","SA"))
summary(metadata_train)
# 筛选OTU
idx = rownames(metadata_train) %in% colnames(table)
metadata_train = metadata_train[idx,]
otu_sub = table[, rownames(metadata_train)] 
dim(otu_sub)
```


## 选择最佳随机数(可选)

```{r}
library(randomForest)
for (i in 0:9){
  set.seed(i)
  rf = randomForest(t(otu_sub), metadata_train$Group, importance=TRUE, proximity=TRUE, ntree = 1000)
  print(i)
  print(rf)
}
```

## 随机森林分类

在确定的分类层级和最佳随机数下建模

```{r}
library(randomForest)
set.seed(8)
rf = randomForest(t(otu_sub), metadata_train$Group, importance=TRUE, proximity=TRUE, ntree = 1000)
print(rf)
```

## 交叉验证选择重要特征

```{r}
set.seed(8) # 随机数据保证结果可重复，必须
# rfcv是随机森林交叉验证函数：Random Forest Cross Validation
result = rfcv(t(otu_sub), metadata_train$Group, cv.fold=10)
# 查看错误率表，31时错误率最低，为最佳模型
result$error.cv
# 绘制验证结果 
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

# 多次绘制
## 建立数据框保存多次结果
error.cv0 = data.frame(num = result$n.var, error.1 =  result$error.cv)
## 指定随机数循环5次
for (i in 1:(1+4)){
  print(i)
  set.seed(i)
  result= rfcv(t(otu_sub), metadata_train$Group, cv.fold=10) #  scale = "log", step = 0.9
  error.cv0 = cbind(error.cv0, result$error.cv)
}
error.cv0 
```

## 绘制交叉验证曲线

```{r}
# 提取x轴标签
n.var = error.cv0$num
# 提取y轴数据+标签
error.cv = error.cv0[,2:6]
colnames(error.cv) = paste('err',1:5,sep='.')
# 添加均值
err.mean = apply(error.cv,1,mean)
# 合并新的数据库，x+error+mean
allerr = data.frame(num=n.var,err.mean=err.mean,error.cv)
# number of otus selected 人为在图中观察的结果，30几乎为最低，且数量可接受
optimal = 70

# 图1：机器学习结果交叉验证图，选择Top features
# 图中 + 5条灰色拆线+1条黑色均值拆线+一条最优垂线+X轴对数变换
write.table(allerr, file = "rfcv.txt", sep = "\t", quote = F, row.names = T, col.names = T)

p = ggplot() + # 开始绘图
  geom_line(aes(x = allerr$num, y = allerr$err.1), colour = 'grey') + # 5次验证灰线 
  geom_line(aes(x = allerr$num, y = allerr$err.2), colour = 'grey') + 
  geom_line(aes(x = allerr$num, y = allerr$err.3), colour = 'grey') + 
  geom_line(aes(x = allerr$num, y = allerr$err.4), colour = 'grey') + 
  geom_line(aes(x = allerr$num, y = allerr$err.5), colour = 'grey') + 
  geom_line(aes(x = allerr$num, y = allerr$err.mean), colour = 'black') + # 均值黑线
  geom_vline(xintercept = optimal, colour='black', lwd=0.36, linetype="dashed") + # 最优垂线
  coord_trans(x = "log2") + # X轴对数变换和刻度
  scale_x_continuous(breaks = c(1, 2, 5, 10, 20, 30, 50, 100, 200)) + # , max(allerr$num)
  labs(title=paste('Training set (n = ', dim(t(otu_sub))[1],')', sep = ''), 
       x='Number of families ', y='Cross-validation error rate') + 
  annotate("text", x = optimal, y = max(allerr$err.mean), label=paste("optimal = ", optimal, sep="")) + theme_bw()
p  
ggsave(p, file = "rfcv.pdf", width = 89, height = 59, unit = 'mm')

```

## 特征重要性可视化

```{r}
## 预览和保存特征贡献度
imp= as.data.frame(rf$importance)
imp = imp[order(imp$MeanDecreaseAccuracy, decreasing = T),]
head(imp,n=optimal)
write.table(imp,file = "importance.txt",quote = F,sep = '\t', row.names = T, col.names = T)
# 简单可视化，比较丑
# varImpPlot(rf, main = "Feature importance",n.var = optimal, bg = par("bg"), color = par("fg"), gcolor = par("fg"), lcolor = "gray" )

# 图2. Feature重要性：绘制条形图+门属性着色

# 读取所有feature贡献度
imp = read.table("importance.txt", header=T, row.names= 1, sep="\t") 
# 
# imp = read.table("importance_top9_classify.txt.txt.txt", header=T, row.names= 1, sep="\t") 
# 分析选择top20分组效果最好，参数显示数量
imp = head(imp, n = 70)
imp = imp[order(imp$MeanDecreaseAccuracy, decreasing = T),]
# write.table(imp,file = "importance_order70.txt",quote = F,sep = '\t', row.names = T, col.names = T)
# 简化全名，去掉界

###############################################

imp$Family = gsub("Bacteria\\|","",rownames(imp))

# 添加门用于着色(删除竖线后面全部)
imp$Phylum = gsub("\\|.*","",imp$Family)

# 设置顺序
imp$Family = factor(imp$Family, levels = imp$Family)

# 图2. 绘制物种类型种重要性柱状图
p = ggplot(imp, aes(x = Family, y = MeanDecreaseAccuracy, fill = Phylum)) +   
  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw()
p
 imp$Family = gsub(".*\\|","",imp$Family)
imp$Family = factor(imp$Family, levels = imp$Family)
p = ggplot(imp, aes(x = Family, y = MeanDecreaseAccuracy, fill = Phylum)) +   
  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw()
p

############################################
imp$OTU = gsub("Bacteria\\|","",rownames(imp))
imp$OTU = gsub("\\|.*","",imp$OTU)
# imp$Family = gsub("Bacteria\\|","",rownames(imp))
# 添加门用于着色(删除竖线后面全部)
# imp$Phylum = gsub("\\|.*","",imp$Family)

# 设置顺序
# imp$Family = factor(imp$Family, levels = imp$Family)
imp$OTU = factor(imp$OTU, levels = imp$OTU)
# 图2. 绘制物种类型种重要性柱状图
p = ggplot(imp, aes(x = OTU, y = MeanDecreaseAccuracy, fill = OTU)) +   
  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw()
p
pp = ggplot(imp, aes(x = OTU, y = MeanDecreaseGini, fill = OTU)) +   
  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw()
pp

ggsave(paste("top_feautre_full_species",".pdf", sep=""), p, width=89*2.5*1.25, height=59*2*1.25, unit='mm')
ggsave(paste("top_feautre_full_gini",".pdf", sep=""), pp, width=89*2.5, height=59*2, unit='mm')
# 名称不一定唯一，需要手动修改
#-----------------------------------------------------
#  简化全名(只保留最后，有重名不可用，可选)
imp$Family = gsub(".*\\|","",imp$Family)
imp$Family = factor(imp$Family, levels = imp$Family)

imp$OTU = factor(imp$OTU, levels = imp$OTU)
p = ggplot(imp, aes(x = Family, y = MeanDecreaseAccuracy, fill = Phylum)) +   
  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw()
p
ggsave(paste("top_feautre",".pdf", sep=""), p, width=89*1.5, height=59*1.5, unit='mm')

```

## 测试集独立验证

如果第一地点数据量足够大，可以取出1/2到1/3进行同一地点的独立验证。方法相同。

筛选测序集样品

```{r}
#metadata_test = subset(testmetadata, Group %in% c("SA","HSL")) 
metadata_test = subset(test, Group %in% c("SA","HSL"))
summary(metadata_test)
# metadata_test=test
idx = rownames(metadata_test) %in% colnames(test)
metadata_test = metadata_test[idx,]
otu_sub = test[,rownames(metadata_test)]

# 转置，并添加分组信息
otutab_t = as.data.frame(t(otu_sub))
#otutab_t$Group = testmetadata[rownames(otutab_t),]
otutab_t$Group = test[rownames(otutab_t),]
otutab_t$Group = test[rownames(otutab_t),]

```


基于训练集随机森林模型验证

```{r}
set.seed(94)
#otutab.pred = predict(rf, t(otutab_t),type = "prob" ) 
otutab.pred = predict(rf,t(otutab_t)) 
#rocobj<-roc(Affairs$ynaffair,pre)


metadata = read.table("metadata.txt",header = T, row.names = 1)
# R4.0读取表不于默认为数据框
metadata$Group = as.factor(metadata$Group)
#######train$Group = as.factor(train$Group)
#######train=subset(train, Group  %in% c("HSL","SA"))
# 筛选"L"地点为训练集
metadata = subset(metadata, Group  %in% c("HSL","SA"))
#-------------------------------------------------------
set.seed(0)
table = read.table(paste0("tax_",i,".txt"),header = T, row.names = 1)
table = table[,rownames(trainset)]
rf = randomForest(t(table), trainset$Group, importance=T, proximity=T, ntree = 1000)
print(i)
print(rf)

index <- 1:nrow(metadata)
testindex <- sample(index, trunc(length(index)/3))
testset <- metadata[testindex,]
trainset <- metadata[-testindex,]

otutab.pred = predict(rf, t(table[,rownames(testset)]),type = "prob" )  

otutab.pred
x= otutab.pred

rocc = roc(testset[,"Group"],x[,2])

ggroc(rocc)
plot(rocc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("grey","green"),max.auc.polygon=TRUE,auc.polygon.col="grey",print.thres=TRUE,print.thres.col="red",xlim=c(1,0),ylim=c(0,1),legacy.axes=T)

plot(rocc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='RF模型ROC曲线')



pre_tab = table(observed=otutab_t[,"Group"],
                predicted=otutab.pred) 
x= pre_tab

plot(rocc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","green"),max.auc.polygon=TRUE,auc.polygon.col="grey",print.thres=TRUE,print.thres.col="blue")


library(rpart)
library(pROC)

pred3 <- prediction( otutab.pred[,2], newdata$潜在高价值标识)

perfsuiji0826 <- performance(pred3,"tpr","fpr")

plot(perfsuiji0826, main = "随机森林算法ROC曲线")
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
x<-predict(fit, type = "class")
levels(x)<-c(0,1)
y<-kyphosis[,"Kyphosis"]
levels(y)<-c(0,1)
data_roc<-roc(xx,y)
plot(data_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","green"),max.auc.polygon=TRUE,auc.polygon.col="yellow",print.thres=TRUE,print.thres.col="blue", add=T)


#-------------------------------------------------------


```

可视化验证结果

```{r}
# 整理样本原始分组和预测分类
predict = data.frame(group = otutab_t[,"Group"], predicted=otutab.pred)

# 保存预测结果表
write.table("SampleID\t", file=paste("RF_prediction_binary.txt",sep=""),append = F, quote = F, eol = "", row.names = F, col.names = F)
write.table(predict, file = "RF_prediction_binary.txt",append = T, quote = F, row.names = T, col.names = T, sep = "\t")

# 转换为数值可视化
# 预测准确标为1，错误标为0
predict$result = ifelse(predict$group == predict$predicted, 1, 0)
# HSL=1, SA=2
predict$predict = ifelse(predict$predicted == "HSL", 1, 2)
# Set sample number in each row
column = 10
# IND
HSL = predict[predict$group=="HSL",]$predict
length(HSL)
row = round(length(HSL)/column + 0.5)
i = column * row - length(HSL)
HSL = c(HSL, rep(NA, i))
matrix = matrix(HSL, nrow = row, ncol = column, byrow = T)
pheatmap(matrix, cluster_rows = F, cluster_cols = F, cellwidth = 15, cellheight = 12)
pheatmap(matrix, cluster_rows = F, cluster_cols = F, cellwidth = 15, cellheight = 12,filename = "OTU_test_IND.pdf")

# Draw TEJ prediction result
SA = predict[predict$group=="SA",]$predict
length(SA)
row = round(length(SA)/column + 0.5)
i = column * row - length(SA)
SA = c(SA, rep(NA, i))
matrix = matrix(SA, nrow = row, ncol = column, byrow = T)
pheatmap(matrix, cluster_rows = F, cluster_cols = F, cellwidth = 15, cellheight = 12)
# 保存图片
pheatmap(matrix, cluster_rows = F, cluster_cols = F, cellwidth = 15, cellheight = 12, filename = "family_test_TEJ.pdf")
```