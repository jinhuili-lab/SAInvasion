# SVM
if(!suppressWarnings(require('kernlab')))
{
  install.packages('kernlab')
  require('kernlab')
}
n <- 1000 # 样本数
p <- 2 # 变量数
sigma <- 1 # 分布的标准差
meanpos <- 0 # 正样本分布的均值
meanneg <- 3 # 负样本分布的均值
npos <- round(n/2) # 正样本数目
nneg <- n-npos # 负样本数目
# 生成正样本
##设置种子，为了试验可重复性
set.seed(1234)
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
##生成负样本
set.seed(1234)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)



##正样本和负样本合并
x <- rbind(xpos,xneg)
# 生成标签
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualize the data
plot(x,col=ifelse(y>0,1,2))
ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]

library(kernlab)
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())

plot(svp,data=xtrain)

ypred <- predict(svp,xtest) ##应用到测试集
table(ytest,ypred) ##利用table()生成混淆矩阵
sum(ypred==ytest)/length(ytest) ##模型预测的准确率

if(!suppressWarnings(require('ROCR')))
{
  install.packages('ROCR')
  require('ROCR')
}
ypredscore <- predict(svp,xtest,type="decision")
pred <- prediction(ypredscore,ytest)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

---------------------------------------------------
  if(!suppressWarnings(require(mlbench)))
  {
    install.packages('mlbench')
    require(mlbench)
  }
# library("e1071")

# data(Glass, package="mlbench")

Glass= read.table("tax_8OTU0.1.txt.T.Group.svf",header = T,check.names=F,as.is=F)
Glass$Group = as.factor(Glass$Group)
##第二步：数据集划分：训练集和测试集
# colnames(train_svm)  <- c(paste('p',c(1:(nday*96+ex))),'f')
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

##第三步：构建SVM模型

library("e1071")
svm(Group ~ ., data = trainset)
svm.model <- svm(Group ~ ., data = trainset, cost = 1000, gamma = 1, kernel = "polynomial" )

##第四步：SVM模型应用到测试数据集

svm.pred <- predict(svm.model, testset)

##第五步：模型结果评估

##1混淆矩阵

table(pred = svm.pred,true= testset[,10])

##2计算Accuracy和Kappa值

classAgreement(table(pred = svm.pred,true= testset[,10]))

###绘制ROC曲线

library("pROC")
svm_roc <- roc(testset$Group,as.numeric(svm.pred))
plot(svm_roc, print.auc=TRUE,  grid=c(0.2, 0.2),grid.col=c("green", "red"),auc.polygon.col="skyblue", print.thres=TRUE,main='SVM模型ROC曲线 kernel = polynomial',xlim=c(1,0),ylim=c(0,1)  ,add=T)


#####
#逻辑回归3


lg=glm(Group ~ ., data = trainset,family = "binomial")
lg.pred <- predict(lg, testset)
table(pred = lg.pred,true= testset[,10])
lg_roc <- roc(testset$Group,as.numeric(lg.pred))
plot(lg_roc, add=T,auc.polygon=TRUE,print.auc=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,print.auc.x=0.3,print.auc.y=0.3,asp = NA)  #RF
######


#####
#随机森林1
plot(rocc,print.auc=TRUE,col="red", print.thres=TRUE,print.auc.x=0.86,print.auc.y=0.86,auc.polygon.col="skyblue",asp = NA,main='ROC曲线', bty = "n" ) #RF
#####
plot_colors <- c("blue","red","black")
legend(x = "bottom",inset = 0,
       legend = c("SVM:72.9%", "Logistic: 71.0%", "RandomForest:80.0%"), 
       col=plot_colors, lwd=7, cex=.7, horiz = TRUE)

# －－－－－－－－－－－－－－－－－－－－－－－－－－
#测试集
#pre_svm <- predict(wine_svm,newdata = test_data)
pre_svm <- predict(svm.model, testset)

#obs_p_svm = data.frame(prob=pre_svm,obs=test_data$等级)
obs_p_svm = data.frame(prob=pre_svm,obs=testset)
obs_p_svm = data.frame(prob=pre_svm,obs=testset$等级)

###输出混淆矩阵
#table(test_data$等级,pre_svm,dnn=c("真实值","预测值"))
table(testset$等级,pre_svm,dnn=c("真实值","预测值"))
###绘制ROC曲线
#SVM2
svm_roc <- roc(testset$等级,as.numeric(pre_svm))
plot(svm_roc, add=T,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),auc.polygon.col="gray",col="blue",, print.thres=TRUE,print.auc.x=0.9,print.auc.y=0.7,)
