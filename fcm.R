#data
data = read.csv("E:/kuliah/kkp/data.csv")
head(data)
attach(data)
#plot
summary(data)
library(Hmisc)
describe(data)
library(ggplot2)

SDM = ggplot(data,aes(x=SDM)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="SDM", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
SDM

infrastruktur = ggplot(data,aes(x=kondisi.masyarakat.dan.lingkungan.sekitar)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Kondisi Masyarakat dan Lingkungan Sekitar", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
infrastruktur

kondisi = ggplot(data,aes(x=infrastruktur)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Infrastruktur", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
kondisi

regulasi = ggplot(data,aes(x=Regulasi.dan.Administrasi)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Regulasi dan Administrasi", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
regulasi

bahan = ggplot(data,aes(x=Bahan.baku)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Bahan Baku", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
bahan

market = ggplot(data,aes(x=kondisi.market)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Kondisi Market", y="Frekuensi",caption="1 = Sangat Tidak Setuju, 2 = Tidak Setuju, 3 = Netral, 4 = Setuju, dan 5 = Sangat Setuju ")
market
#library
library(ppclust)
library(factoextra)
library(dplyr)
library(cluster)                                                             
library(fclust)

x = data[,1:6]
head(x)
        
describe(x)
shapiro.test(data$SDM)
shapiro.test(data$kondisi.masyarakat.dan.lingkungan.sekitar)
shapiro.test(data$infrastruktur)
shapiro.test(data$Regulasi.dan.Administrasi)
shapiro.test(data$kondisi.market)
shapiro.test(data$Bahan.baku)
pairs(x, col=data[,1:6])

cor(data[,1:6], method="spearman")
cor.test(data$SDM, data$infrastruktur, method=c("spearman"))
cor.test(data$kondisi.masyarakat.dan.lingkungan.sekitar, data$infrastruktur, method=c("spearman"))
cor.test(data$Regulasi.dan.Administrasi, data$infrastruktur, method=c("spearman"))
cor.test(data$Bahan.baku, data$infrastruktur, method=c("spearman"))
cor.test(data$infrastruktur, data$infrastruktur, method=c("spearman"))
library(psych)
pairs.panels(data[,1:6], method = "spearman")



#run fcm with multiple starts
res.fcm <- fcm(x, centers=3)
res.fcm$csize
res.fcm$u
res.fcm$cluster


#pairwise scatter plot
plotcluster(res.fcm, cp=1, trans=TRUE)

library(ppclust)
#cluster plot with fviz_cluster
res.fcm2 <- ppclust2(res.fcm, 'kmeans')
fviz_cluster(res.fcm2, data = x, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE)


#validation of the clustering results
res.fcm4 <- ppclust2(res.fcm, 'fclust')


 # Partition Entropy:
idxsf <- PE(res.fcm4$U)
paste("Partition Entropy: ",idxsf)
# Partition Coefficient:
idxpc <- PC(res.fcm4$U)
paste("Partition Coefficient : ",idxpc)
# Modified Partition Coefficient:
idxmpc <- MPC(res.fcm4$U)
paste("Modified Partition Coefficient :",idxmpc)

