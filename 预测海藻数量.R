#用R语言对海藻数量的数据进行预处理
#加载数据包
install.packages("DMwR") 
library(DMwR)
head(algae)
#对于数据给出行名称
algae=read.table("E:/Analysis.txt",
                 header=F,
                 dec='.',
                 col.names=c('season','size','speed','mxPH','mno2','cl','no3','nh4','opo4','po4','chla','a1','a2','a3','a4','a5','a6','a7'),
                 na.strings=c('XXXXXXX'))

#绘制PH直方图和密度图并用QQ图查看数据是否符合正态分布
install.packages("car") 
library(car)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,xlab='',main='最大PH值的直方图',ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qqPlot(algae$mxPH,main='最大PH值的QQ图')
par(mfrow=c(1,1))

#数据缺失处理
#剔除
library(DMwR)
data(algae)
algae[!complete.cases(algae),]

nrow(algae[!complete.cases(algae),])

#用均值填补缺失值
algae[48,"mxPH"]=mean(algae$mxPH,na.rm=T)

#采用欧式距离
algae=knnImputation(algae,k=10)

#小数定标规范化
i1=ceiling(log(max(abs(algae[,4])),10))
c1=algae[,4]/10^i1
i2=ceiling(log(max(abs(algae[,5])),10))
c2=algae[,5]/10^i2
i3=ceiling(log(max(abs(algae[,6])),10))
c3=algae[,6]/10^i3
i4=ceiling(log(max(abs(algae[,7])),10))
c4=algae[,7]/10^i4
data_dot=cbind(c1,c2,c3,c4)

#打印结果
options(digits = 4) #控制有效的输出位数
algae

#用多元线性回归来建立预测海藻的模型
#由于多元线性回归模型中没有处理缺失值的方法
#因此 在这里我们再对数据进行一次预处理
data(algae)
algae=algae[-manyNAs(algae),]
clean.algae=knnImputation(algae,k=10)
algae

#建立用于预测海藻频率的线性回归模型
lm.a1=lm(a1~.,data=clean.algae[,1:12])
#函数lm()建立一个线性回归模型 其中 第一个参数给出了模型的函数形式
#这个函数的形式是用数据中的其他所有变量来预测变量a1
#第一个参数中的"."代表数据框中的所有除a1以外的变量

#用下列的代码我们能获取到更多关于线性回归模型的信息
summary(lm.a1)

#经过思考觉得海藻案例应用线性模型是不合适的
#用线性思维去考虑非线性问题 得不到理想的结果
#这里考虑用回归树模型预测
library(rpart)
data(algae)
algae<-algae[-manyNAs(algae),]
rt.a1<-rpart(a1~.,data=algae[ ,1:12])
#回归树图形表示
plot(rt.a1)
text(rt.a1)

#用k折交叉验证对模型性能进行可靠性估计
#我们先用NMSE作为回归树模型和线性回归模型的性能评估指标
cv.rpart<-function(form,train,test,...){
  m<-rpartXse(form,train,...)
  p<-predict(m,test)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
cv.lm<-function(form,train,test,...){
  m<-lm(form,train,...)
  p<-predict(m,test)
  p<-ifelse(p<0,0,p)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

#下列代码进行模型的交叉验证比较：
res<-experimentalComparison(
  c(dataset(a1~.,clean.algae[,1:12],'a1')),
  c(variants('cv.lm'),
    variants('cv.rpart',se=c(0,0.5,1))),
  cvSettings(3,10,1234)
)

#比较结果的摘要
summary(res)
#结果可视化
plot(res)

#多个预测任务同时进行
DSs <- sapply( names(clean.algae)[12:18],
               function(x,names.attrs){
                 f<-as.formula(paste(x,"~."))
                 dataset(f,clean.algae[,c(names.attrs,x)],x)
               },
               names(clean.algae)[1:11]
)
res.all<-experimentalComparison( DSs,
                                 c(variants('cv.lm'),
                                   variants('cv.rpart',se=c(0,0.5,1))
                                 ),
                                 cvSettings(5,10,1234)
)
#该代码首先创建用于比较7个预测任务的数据集向量
#然后 采用重复5次10折交叉验证以提高统计结果的显著性

#所有海藻交叉验证结果的可视化
plot(res.all)
#图中显示有几个结果很差 即其NMSE值明显大于1 
#这意味着测试结果比简单采用目标变量的均值这一基准模型还要

#查看每个问题对应的最优模型的代码
bestScores(res.all)
#其结果说明，只有海藻1的预测结果尚可

