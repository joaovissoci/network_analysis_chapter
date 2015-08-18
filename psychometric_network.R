#####################################################
#psychometric_network.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
####################################################
##this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step
#####################################################
#SETTING ENVIRONMENT
#####################################################
#install all packages with
install.packages(“qgraph”)
#change the name in " " to install other packages

#loadings packages required for this script. To install please see: http://goo.gl/dEtgV
library(qgraph)
library(psych)
library(parcor) 
library(Matrix) 
library(glmnet)
library(IsingFit)
library(repmis)

####################################################
#IMPORTING DATA
####################################################
#mannually importing the data through computer path
#downlado data from: 
#add the path from your computer to the location of the dataset in "", it should look like this "/Users/something/Desktop/data_chapter.csv"
#data<-read.csv("/Users/joaovissoci/Desktop/data_chapter.csv",header=T,sep=";")

#another method to import directly from dropbox using the repmiss package. Problems with installing the package should be addressed in this tutorial: http://goo.gl/0z0brM
data <- repmis::source_DropboxData("data_chapter.csv",
                                  "4xtu6fj0gpgqn1g",
                                  sep = ";",
                                  header = TRUE)
####################################################
#DATA MANAGEMENT
####################################################

# excluding missing data
data<-na.omit(data)

# creating vector with grouping variables for each dimension of the questionnaire
groups <- factor(rep(c("Extroversão","Socialização","Conscienciosidade","Neuroticismo","Abertura"), 5))


####################################################
#FIRST PORTION OF THE CHAPTER
####################################################
E2 <- data.frame(from = c(1,1,2,1), to = c(2,3,3,1), weight = c(1,3,-2,0))
print(E2)
qgraph(E2,directed=FALSE)
qgraph(E2,layout="circle")

E3 <-  data.frame(from = c("B","A","B","A"), to = c("A","C","C","A"), weight = c(-2,0.5,2,0))
print (E3)

qgraph(E3,directed=FALSE)
qgraph(E3,layout="circle")
qgraph(E3,directed=FALSE,layout="spring")
qgraph(E3) 

Mat1 <- matrix( c(0, 0.5, 0.4, 0.4, 0.2, 0, 0, 0.5, 0.1, 0.4, 0, 0, 0, 0.1, 0.4, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0) ,5 ,5)
print(Mat1)

Labels<- LETTERS[1:5]
Names<-c("Item A", "Item B", "Item C", "Item D", "Item E")
qgraph(Mat1, labels = Labels, nodeNames = Names, layout = "spring", directed=FALSE)

simData1<-item.dichot(nvar = 12, nsub = 500, circum = FALSE, xloading = 0.7, yloading = 0.5, gloading = 0, xbias = 0, ybias = 0, low = 0, high = 1)
simData1matrix<-tetrachoric(simData1)
simData1matrix<-simData1matrix$rho

simData1Graph<-qgraph(simData1matrix,layout="spring")
centralityPlot(simData1Graph,standardized=TRUE,theme_bw=TRUE)

####################################################
#SECOND PORTION OF THE CHAPTER
####################################################


par(mfrow=c(2,2))
### 
cor<-polychoric(data)$rho
cov<-cov(data)

graph_correlation <- qgraph(cor,layout="spring",colour=groups,groups=groups,legend=FALSE,borders=FALSE,labels=rownames(cor))

### 

graph_partial <- qgraph(cor,layout=graph_correlation$layout,colour=groups,groups=groups,legend=FALSE,borders=FALSE,graph="concentration",labels=rownames(cor))


### 

set.seed(300)

adls <- adalasso.net(data) 

network <-as.matrix(forceSymmetric(adls$pcor.adalasso)) 
qgraph_adlasso<-qgraph(network, layout=graph_correlation$layout, colour=groups,groups=groups,legend=FALSE,borders=FALSE,labels=rownames(cor)) 

### 
data_ising<-data
data_ising[,1]<-car::recode(data[,1],"1:2=0;3:5=1")
data_ising[,2]<-car::recode(data[,2],"1:2=0;3:5=1")
data_ising[,3]<-car::recode(data[,3],"1:2=0;3:5=1")
data_ising[,4]<-car::recode(data[,4],"1:2=0;3:5=1")
data_ising[,5]<-car::recode(data[,5],"1:2=0;3:5=1")
data_ising[,6]<-car::recode(data[,6],"1:2=0;3:5=1")
data_ising[,7]<-car::recode(data[,7],"1:2=0;3:5=1")
data_ising[,8]<-car::recode(data[,8],"1:2=0;3:5=1")
data_ising[,9]<-car::recode(data[,9],"1:2=0;3:5=1")
data_ising[,10]<-car::recode(data[,10],"1:2=0;3:5=1")
data_ising[,11]<-car::recode(data[,11],"1:2=0;3:5=1")
data_ising[,12]<-car::recode(data[,12],"1:2=0;3:5=1")
data_ising[,13]<-car::recode(data[,13],"1:2=0;3:5=1")
data_ising[,14]<-car::recode(data[,14],"1:2=0;3:5=1")
data_ising[,15]<-car::recode(data[,15],"1:2=0;3:5=1")
data_ising[,16]<-car::recode(data[,16],"1:2=0;3:5=1")
data_ising[,17]<-car::recode(data[,17],"1:2=0;3:5=1")
data_ising[,18]<-car::recode(data[,18],"1:2=0;3:5=1")
data_ising[,19]<-car::recode(data[,19],"1:2=0;3:5=1")
data_ising[,20]<-car::recode(data[,20],"1:2=0;3:5=1")
data_ising[,21]<-car::recode(data[,21],"1:2=0;3:5=1")
data_ising[,22]<-car::recode(data[,22],"1:2=0;3:5=1")
data_ising[,23]<-car::recode(data[,23],"1:2=0;3:5=1")
data_ising[,24]<-car::recode(data[,24],"1:2=0;3:5=1")
data_ising[,25]<-car::recode(data[,25],"1:2=0;3:5=1")

Res <- IsingFit(data_ising, plot=FALSE)

qgraph_ising<-qgraph(Res$weiadj,fade = FALSE,layout=qgraph_adlasso$layout,colour=groups,groups=groups,legend=FALSE,borders=FALSE,labels=rownames(cor))

## Load binary data
V <- colnames(data_ising)
## estimate CPDAG
pc.B <- pc(suffStat = list(dm = data_ising, adaptDF = FALSE),
indepTest = binCItest, alpha = 0.01, labels = V, verbose = TRUE)

qgraph(pc.B, layout=graph_correlation$layout, colour=groups,groups=groups,legend=FALSE,borders=FALSE,labels=V)

data_factor<-lapply(data, as.factor)
data_factor<-as.data.frame(data_factor)
summary(as.data.frame(data_factor))
nlev<-rep(5,25)
data_pcalg<-data
data_pcalg[,1]<-car::recode(data[,1],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,2]<-car::recode(data[,2],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,3]<-car::recode(data[,3],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,4]<-car::recode(data[,4],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,5]<-car::recode(data[,5],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,6]<-car::recode(data[,6],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,7]<-car::recode(data[,7],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,8]<-car::recode(data[,8],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,9]<-car::recode(data[,9],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,10]<-car::recode(data[,10],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,11]<-car::recode(data[,11],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,12]<-car::recode(data[,12],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,13]<-car::recode(data[,13],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,14]<-car::recode(data[,14],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,15]<-car::recode(data[,15],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,16]<-car::recode(data[,16],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,17]<-car::recode(data[,17],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,18]<-car::recode(data[,18],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,19]<-car::recode(data[,19],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,20]<-car::recode(data[,20],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,21]<-car::recode(data[,21],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,22]<-car::recode(data[,22],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,23]<-car::recode(data[,23],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,24]<-car::recode(data[,24],"1=0;2=1;3=2;4=3;5=4")
data_pcalg[,25]<-car::recode(data[,25],"1=0;2=1;3=2;4=3;5=4")
data_pcalg<-lapply(data_pcalg, as.integer)


data_pcalg<-as.data.frame(data_pcalg)

## Load data
V <- colnames(data_pcalg)
## define sufficient statistics
suffStat <- list(dm = data_pcalg, nlev = nlev, adaptDF = FALSE)
## estimate CPDAG
pc.D <- pc(suffStat, indepTest = disCItest, alpha = 0.01, labels = V, verbose = TRUE)
qgraph(pc.D)
