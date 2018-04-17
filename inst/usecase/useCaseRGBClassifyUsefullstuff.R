
library(corrplot)
library("PerformanceAnalytics")
# drops <- c("IC1","Haralick_Correlation","Cluster_Prominence","Long_Run_High_Grey-Level_Emphasis","Long_Run_Low_Grey-Level_Emphasis","Run_Percentage","Skewness","Variance","Mean","Cluster_Shade","Inertia","Long_Run_Emphasis")
# keeps <- c("ID","red","green","blue","GLAI","Energy","Correlation","Short_Run_Emphasis","HI","SI","Entropy","Low_Grey-Level_Run_Emphasis","FN","Skewness","Variance","Mean")
# keepsGreen <-c("ID","red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI","FN")
tr<-trainDF
# tr<-tr[ , !(names(tr) %in% drops)]
# tr<-tr[ , (names(tr) %in% keepsGreen)]
#save(tr,file = "~/temp7/GRASS7/output/training.RData")
drops <- c("ID","FN")
tr<-tr[ , !(names(tr) %in% drops)]
res<-tr[complete.cases(tr), ]
res<-res[complete.cases(res), ]


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

p.mat <- cor.mtest(res,na.action="na.omit")
p2<-p.mat[,- caret::nearZeroVar(p.mat)]
res2<-res[,- caret::nearZeroVar(p.mat)]
res3<-res2[p2>0.5,]
res3[complete.cases(res3), ]
t<-corrplot(cor(res2), type="upper", order="hclust", 
         p.mat = p2>0.8, sig.level = 0.01,tl.col="black")

saveRDS(res3,"df3.rds")
p<-class(p.mat)
corrplot(cor(res), method="shade",shade.col=NA, tl.col="black", tl.srt=45,tl.cex = 0.5,type = "upper")
corrplot(cor(res), diag = FALSE, order = "FPC", 
          tl.cex = 0.5, method = "color", type = "upper")

#####
chart.Correlation(tr, histogram=TRUE, pch=19)


corrplot(res, method="circle")
corrplot(res,add=TRUE, type="lower", method="number",
         order="AOE", diag=FALSE, tl.pos="n", cl.pos="n")


tr
c("red","green","blue","GLAI","Energy","Correlation","Short_Run_Emphasis","HI","SI","Entropy","Low_Grey-Level_Run_Emphasis")

n<-names(res3) 
