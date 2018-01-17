
load(file = "~/temp7/GRASS7/output/trainingtraindat_corrected.RData")
library(corrplot)
library("PerformanceAnalytics")
drops <- c("alpha","IC1","Haralick_Correlation","Cluster_Prominence","Long_Run_High_Grey-Level_Emphasis","Long_Run_Low_Grey-Level_Emphasis","Run_Percentage","Skewness","Variance","Mean","Cluster_Shade","Inertia","Long_Run_Emphasis")
keeps <- c("ID","red","green","blue","GLAI","Energy","Correlation","Short_Run_Emphasis","HI","SI","Entropy","Low_Grey-Level_Run_Emphasis","FN")
trainingDF<-tr
tr<-tr[ , (names(tr) %in% keeps)]
save(tr,file = "~/temp7/GRASS7/output/trainingtraindat_corrected2.RData")
drops <- c("ID","FN")
tr<-tr[ , !(names(tr) %in% drops)]
res <- cor(tr, use = "complete.obs")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(res)
corrplot(res, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)



#####
chart.Correlation(tr, histogram=TRUE, pch=19)


corrplot(res, method="circle")
corrplot(res,add=TRUE, type="lower", method="number",
         order="AOE", diag=FALSE, tl.pos="n", cl.pos="n")


tr
c("red","green","blue","GLAI","Energy","Correlation","Short_Run_Emphasis","HI","SI","Entropy","Low_Grey-Level_Run_Emphasis")

