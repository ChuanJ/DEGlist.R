###########################################################################################################################
# #Programmer:  Chuan Jiao
# #email  jiaochuan@sklmg.edu.cn
# #Code: R
# #Program name: DEGlist.R
# #Date: March 15, 2018
###########################################################################################################################


dat<-as.matrix(read.csv("U133Aquchong.csv",header=T,sep=",",row.names=1,check.names=F))
sam<-read.csv("u133asample.csv",header=T,sep=",")
sum(is.na(sam$age))
sum(is.na(sam$gender))
sum(is.na(sam$Brain.region))
#
en<-sam[complete.cases(sam$gender),]

ma<-intersect(en$sampleID,colnames(dat))
data<-dat[,ma]
age<-en$age
gender<-en$gender
brain.region<-en$Brain.region

#age   lm
library(sva)
mod=model.matrix(~as.factor(gender)+age+as.factor(brain.region),data=en)
n.sv=num.sv(data,mod,method="be")
mod0=model.matrix(~as.factor(gender)+as.factor(brain.region),data=en)
svobj = sva(data,mod,mod0,n.sv=n.sv)$sv
mm<-model.matrix(~svobj+as.factor(gender)+as.factor(brain.region))
datalm<-apply(data,1,function(x) {residuals(lm(x~mm))})
data0<-t(datalm)+rowMeans(data)
mm=model.matrix(~-1+age)
p=apply(data0,1,function(x){summary(lm(x~mm))$coefficients[,4]})

#gender    t.test
mod0=model.matrix(~as.factor(brain.region)+age,data=en)
svobj = sva(data,mod,mod0,n.sv=n.sv)$sv
mm<-model.matrix(~svobj+age+as.factor(brain.region))
datalm<-apply(data,1,function(x) {residuals(lm(x~mm))})
data1<-t(datalm)+rowMeans(data)
datat<-apply(data1,1,function(x) t.test(x~gender))
sexp<-lapply(datat,function(y) y$p.value)
#brain.region  ANOVA
mod0=model.matrix(~as.factor(gender)+age,data=en)
svobj = sva(data,mod,mod0,n.sv=n.sv)$sv
  mm<-model.matrix(~svobj+age+as.factor(gender))
datalm<-apply(data,1,function(x) {residuals(lm(x~mm))})
data2<-t(datalm)+rowMeans(data)
data0_anova<-apply(data2,1,function(x) aov(x~brain.region))
out1<-lapply(data0_anova,function(y) summary(y)[[1]][["Pr(>F)"]])
brp<-lapply(out1,function(x) x[1])
agep<-p[2,]
out<-rbind(agep,sexp,brp)
outt<-t(out)
p1<-matrix(unlist(outt),nrow(outt),ncol(outt),dimnames=list(rownames(data),colnames(outt)))

library(qvalue)
q<-apply(p1,2,function(x) qvalue(x))
q<-lapply(q,function(x) x$qvalues)
#t<-data.frame(q)
#apply(t,2,function(x) length(which(x<0.05)))
#
write.csv(q,"u133aq1.csv")

