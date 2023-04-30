hypertension <- read.csv("hypertension_data.csv")
View(hypertension)

names(hypertension) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg","thalach","exang", "oldpeak","slope", "ca", "thal", "target")
head(hypertension)


#=========== EKSPLORASI DATA ===========
str(hypertension)
#Mengubah variabel numerik menjadi kategorik
hypertension$sex<-as.factor(hypertension$sex)
hypertension$cp<-as.factor(hypertension$cp)
hypertension$fbs<-as.factor(hypertension$fbs)
hypertension$restecg<-as.factor(hypertension$restecg)
hypertension$exang<-as.factor(hypertension$exang)
hypertension$slope<-as.factor(hypertension$slope)
hypertension$ca<-as.factor(hypertension$ca)
hypertension$thal<-as.factor(hypertension$thal)
hypertension$target<-as.factor(hypertension$target)
str(hypertension)

#Melihat jumlah kelas pada variabel yang akan diprediksi
levels(hypertension$target) = c("No Disease","Disease")
levels(hypertension$sex) = c("Female","Male")
table(hypertension$target)
prop.table(table(hypertension$target))

mytable <- table(hypertension$target)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,main="Pie Chart of Disease \n (with sample sizes)",col=c("blue","red"))

#Membuat Grouped Bar Plot berdasarkan jenis kelamin
counts <- table(hypertension$target,hypertension$sex)
barplot(counts,main="Sebaran Penyakit berdasarkan Jenis Kelamin",xlab=" ", col=c("blue","red"),legend=rownames(counts),beside=TRUE)

#============PEMBAGIAN DATA TRAINING DAN TESTING===========
library(caTools)
set.seed(123)
split = sample.split(hypertension, SplitRatio = 0.70)
hyper.train = subset(hypertension, split==TRUE)
hyper.test = subset(hypertension, split==FALSE)

#=========== REGRESI LOGISTIK ===========
model.logistik<-glm(target~.,data=hyper.train, family=binomial(link = "logit"))
summary(model.logistik)

model2<-glm(target~.-age,data=hyper.train, family=binomial(link = "logit"))
model3 <- glm(target~.-age-sex,data=hyper.train, family=binomial(link = "logit"))
model4<-glm(target~.-age-sex-chol,data=hyper.train, family=binomial(link = "logit"))

variabel = c("model 1", "model 2","model 3", "model 4")
AIC = c(model.logistik$aic, model2$aic, model3$aic, model4$aic)
tabel.kriteria = data.frame(variabel, AIC)

#======GOODNESS OF FIT=========
gof <- glm(target~1, data = hypertension, family = binomial(link="logit"))
1-as.vector(logLik(model4)/logLik(gof))

#==========UJI MULTIKOLINEARITAS==============
library(car)
vif(model4)

#=========== PREDIKSI VARIABEL RESPON PADA DATA TESTING ===========
prob.prediksi<-predict(model4, hyper.test, type="response")
prediksi<-ifelse(prob.prediksi>0.5,"Disease","No Disease")
prediksi.aktual<-data.frame(Prediksi=prediksi,Aktual=hyper.test$target)
head(prediksi.aktual)


#=========== AKURASI ===========
#Hasil prediksi pada data testing dibandingkan dengan data sebenarnya pada respon data testing
library(caret) #untuk klasifikasi dan training regresi 
confusionMatrix(as.factor(prediksi), hyper.test$target) #Tabel Tabulasi Silang

#=========== UJI WALD ===========
library(car) #pendamping regresi terapan
Anova (model4,type='II',test='Wald')
