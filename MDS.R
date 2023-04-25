library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)
library(ggplot2)

data10 = read.delim("clipboard")
data10
summary(data10)
attach(data10)

#normalitas
ks.test(data10$Sony, "pnorm")
ks.test(data10$Acer, "pnorm")
ks.test(data10$Asus, "pnorm")
ks.test(data10$Hp, "pnorm")
ks.test(data10$Lenovo, "pnorm")
ks.test(data10$Msi, "pnorm")
ks.test(data10$Toshiba, "pnorm")

#histogram
Sony = ggplot(data10,aes(x=Sony)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Sony", y="Skor")
Sony 
Acer =ggplot(data10,aes(x=Acer)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Acer", y="Skor")
Acer
Asus =ggplot(data10,aes(x=Hp)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Asus", y="Skor")
Asus
Hp =ggplot(data10,aes(x=Hp)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Hp", y="Skor")
Hp
Lenovo =ggplot(data10,aes(x=Lenovo)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Lenovo", y="Skor")
Lenovo
Msi =ggplot(data10,aes(x=Msi)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Msi", y="Skor")
Msi
Toshiba =ggplot(data10,aes(x=Toshiba)) + geom_histogram(fill="skyblue", binwidth = 1) + labs(x="Toshiba", y="Skor")
Toshiba

#compute MDS normal
mds = data10 %>%
  dist() %>%
  cmscale() %>%
  as_tibble()
colnames(mds)= c("Dim.1", "Dim.2")

ggscatter(mds, x="Dim.1", y="Dim.2",
          label= rownames(mds),
          size=1,
          repel=True)

#mds tdk normal
mds = data10 %>%
  dist() %>%
  sammon() %>%
  .$points %>%
  as_tibble()
mds
colnames(mds) = c("Dim.1","Dim.2")

# Plot
ggscatter(mds, x="Dim.1", y="Dim.2",
          label= rownames(data10),
          size=1,
          repel=TRUE)

res.cor = cor(data10[,2:7], method="spearman")
mds.cor = (1-res.cor) %>%
  cmdscale() %>%
  as_tibble
colnames(mds.cor) = c("Dim.1", "Dim.2")
plot = ggscatter(mds.cor, x= "Dim.1", y="Dim.2",
          size =3,
          label = colnames(res.cor),
          repel= TRUE,
          col = "blue") 
plot
plot(plot)

abline(h=0,v=0, col="black")


