## FELIX : Exploring data, preparing for clustering :

# 26/07/18 - Camille



cdv <- read.csv2("C:/Users/Camille/Desktop/projet FELIX/base de données CDV/felix.csv", sep=",")

head(cdv)

test2 <- cdv[11000:11005,]
View(test2)

cdvTest <- cdv[, c("NOT_FAMI", "NOT_PROF", "NOT_AMIS", "NOT_COHE", "NOT_POLI", "NOT_LIBR", "NOT_LOG", "NOT_CAD", "ANNEEFUZ", "INTER6")]
View(cdvTest)

cdv1718 <- subset(cdvTest, cdvTest$ANNEEFUZ %in% c("2017", "2018"))

summary(cdv1718$NOT_FAMI)
summary(cdv1718$NOT_PROF)
summary(cdv1718$NOT_AMIS)
summary(cdv1718$NOT_COHE)
summary(cdv1718$NOT_POLI)
summary(cdv1718$NOT_LIBR)
summary(cdv1718$NOT_LOG)
summary(cdv1718$NOT_CAD)

test <- subset(cdv1718, is.na(cdv1718$NOT_POLI))
View(test)

View(subset(cdv, cdv$INTER6=="390047"))

library(Rcmdr)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)


# First try with naive PCA :

cdv1718.PCA<-cdv1718[, c("NOT_FAMI", "NOT_PROF", "NOT_AMIS", "NOT_COHE", 
                         "NOT_POLI", "NOT_LIBR", "NOT_LOG", "NOT_CAD")]
res<-PCA(cdv1718.PCA , scale.unit=TRUE, ncp=5, graph = FALSE)
res.hcpc<-HCPC(res ,nb.clust=0,consol=TRUE,min=3,max=10,graph=TRUE)
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE)
plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", 
         col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0)


par(mfrow=c(1,2))

plot.HCPC(res.hcpc, axes=c(1,2), choice="map", draw.tree=FALSE, ind.names=FALSE)
plot.HCPC(res.hcpc, axes=c(3,4), choice="map", draw.tree=FALSE, ind.names=FALSE)


plot.PCA(res, axes=c(1, 2), choix="var", habillage="none",  new.plot=TRUE)
plot.PCA(res, axes=c(3, 4), choix="var")
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")

summary(res.hcpc$data.clust$clust)


# Try with PCA, centering the data (so we have relative effects) :


cdv1718 <- subset(cdvTest, cdvTest$ANNEEFUZ %in% c("2017", "2018"))



# on centre nos variables par individus :
moyenneNot <- function(data){
  res <- data
  for(i in 1:nrow(data)){
    varRepondues <- c()
    for(j in 1:8)
    if(!is.na(data[i,j])){
      varRepondues <- c(varRepondues, data[i,j])
    }

    res[i, length(data)+1] <- mean(varRepondues)
    
    for(j in 1:8){
      if(!is.na(data[i,j])){
        res[i,j] <- res[i,j] - res[i, length(data)+1]
      }
    }
  }
  colnames(res)[length(data)+1] <- c("Moyenne_Not")
  return(res)
}


cdv1718 <- moyenneNot(cdv1718)
View(cdv1718)

summary(cdv1718$Moyenne_Not)
summary(cdv1718$NOT_FAMI) 
plot(density(cdv1718$NOT_FAMI[!is.na(cdv1718$NOT_FAMI)]))
summary(cdv1718$NOT_PROF)
plot(density(cdv1718$NOT_PROF[!is.na(cdv1718$NOT_PROF)]))
summary(cdv1718$NOT_AMIS)
plot(density(cdv1718$NOT_AMIS[!is.na(cdv1718$NOT_AMIS)]))
summary(cdv1718$NOT_COHE)
plot(density(cdv1718$NOT_COHE[!is.na(cdv1718$NOT_COHE)]))
summary(cdv1718$NOT_POLI)
plot(density(cdv1718$NOT_POLI[!is.na(cdv1718$NOT_POLI)]))
summary(cdv1718$NOT_LIBR)
plot(density(cdv1718$NOT_LIBR[!is.na(cdv1718$NOT_LIBR)]))
summary(cdv1718$NOT_LOG)
plot(density(cdv1718$NOT_LOG[!is.na(cdv1718$NOT_LOG)]))
summary(cdv1718$NOT_CAD)
plot(density(cdv1718$NOT_CAD[!is.na(cdv1718$NOT_CAD)]))


# People marking every dimension the same (5% of pop.) :
test <- subset(cdv1718, cdv1718$NOT_FAMI==cdv1718$NOT_PROF & cdv1718$NOT_FAMI==cdv1718$NOT_AMIS & cdv1718$NOT_FAMI==cdv1718$NOT_POLI & cdv1718$NOT_FAMI == cdv1718$NOT_LIBR & cdv1718$NOT_FAMI==0 & cdv1718$NOT_CAD==0)
View(test)
summary(test$Moyenne_Not)
plot(hist(test$Moyenne_Not))




enleveNonRep <- function(data){
  res <- data
  for(i in 1:nrow(data)){
    res[i, length(data)+1] <- 0
    for(j in 1:8){
      if(is.na(data[i,j])){
        res[i, length(data)+1] <- res[i, length(data)+1] + 1
      }
    }
  }
  colnames(res)[length(data)+1] <- c("NonRep")
  return(res)
  
}

cdvRep <- enleveNonRep(cdv1718)
View(cdvRep)
table(cdvRep$NonRep)





indecis <- function(data){
  res <- data
  for(i in 1:nrow(data)){
    varRepondues <- NULL
    for(j in 1:8){
      if(!is.na(data[i,j])){
        varRepondues <- c(varRepondues, data[i,j])
      }
    }
    if(length(varRepondues) > 1){
      x <- 0
      for(r in 1:length(varRepondues)){
        x <- x + abs(varRepondues[r])
      }
      
      if(sum(abs(varRepondues)) == 0){
        res[i, length(data)+1] <- 1
      } else {
        res[i, length(data)+1] <- 0
      }
    }
    
    
  }
  colnames(res)[length(data)+1] <- c("Indecis")
  return(res)
}

cdv1718 <- indecis(cdv1718)
View(cdv1718)





cdv1718.PCA<-cdv1718[, c("NOT_FAMI", "NOT_PROF", "NOT_AMIS", "NOT_COHE", 
                         "NOT_POLI", "NOT_LIBR", "NOT_LOG", "NOT_CAD")]
res<-PCA(cdv1718.PCA , scale.unit=TRUE, ncp=5, graph = FALSE)
res.hcpc<-HCPC(res ,nb.clust=0,consol=TRUE,min=3,max=10,graph=TRUE)
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 4, file="")
summary(res.hcpc$data.clust$clust)

test <- data.frame(res.hcpc$data.clust, cdvRep$NonRep)
View(test)
table(test$clust, test$cdvRep.NonRep)

test <- data.frame(res.hcpc$data.clust, cdv1718$Indecis)
table(test$clust, test$cdv1718.Indecis)

par(mfrow=c(1,2))

plot.HCPC(res.hcpc, choice="tree")

plot.HCPC(res.hcpc, axes=c(1,2), choice="map", draw.tree=FALSE, ind.names=FALSE)
plot.PCA(res, axes=c(1, 2), choix="var", habillage="none",  new.plot=TRUE)
plot.HCPC(res.hcpc, axes=c(3,4), choice="map", draw.tree=FALSE, ind.names=FALSE)
plot.PCA(res, axes=c(3, 4), choix="var")





# Exploring with 5 clusters :

clust1 <- subset(res.hcpc$data.clust, res.hcpc$data.clust$clust=="1")
clust2 <- subset(res.hcpc$data.clust, res.hcpc$data.clust$clust=="2")
clust3 <- subset(res.hcpc$data.clust, res.hcpc$data.clust$clust=="3")
clust4 <- subset(res.hcpc$data.clust, res.hcpc$data.clust$clust=="4")
clust5 <- subset(res.hcpc$data.clust, res.hcpc$data.clust$clust=="5")


# cluster 1 :

par(mfrow=c(2,2))

plot(density(clust1$NOT_FAMI))
plot(density(clust1$NOT_PROF))
plot(density(clust1$NOT_AMIS))
plot(density(clust1$NOT_COHE))

plot(density(clust1$NOT_POLI))
plot(density(clust1$NOT_LIBR))
plot(density(clust1$NOT_LOG))
plot(density(clust1$NOT_CAD))

summary(clust1)


# cluster 2 :

plot(density(clust2$NOT_FAMI))
plot(density(clust2$NOT_PROF))
plot(density(clust2$NOT_AMIS))
plot(density(clust2$NOT_COHE))

plot(density(clust2$NOT_POLI))
plot(density(clust2$NOT_LIBR))
plot(density(clust2$NOT_LOG))
plot(density(clust2$NOT_CAD))

summary(clust2)


# cluster 3 :


plot(density(clust3$NOT_FAMI))
plot(density(clust3$NOT_PROF))
plot(density(clust3$NOT_AMIS))
plot(density(clust3$NOT_COHE))

plot(density(clust3$NOT_POLI))
plot(density(clust3$NOT_LIBR))
plot(density(clust3$NOT_LOG))
plot(density(clust3$NOT_CAD))

summary(clust3)


# cluster 4 :

plot(density(clust4$NOT_FAMI))
plot(density(clust4$NOT_PROF))
plot(density(clust4$NOT_AMIS))
plot(density(clust4$NOT_COHE))

plot(density(clust4$NOT_POLI))
plot(density(clust4$NOT_LIBR))
plot(density(clust4$NOT_LOG))
plot(density(clust4$NOT_CAD))

summary(clust4)


# clust5 :

plot(density(clust5$NOT_FAMI))
plot(density(clust5$NOT_PROF))
plot(density(clust5$NOT_AMIS))
plot(density(clust5$NOT_COHE))

plot(density(clust5$NOT_POLI))
plot(density(clust5$NOT_LIBR))
plot(density(clust5$NOT_LOG))
plot(density(clust5$NOT_CAD))

summary(clust5)


