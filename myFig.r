Sweave("ggESDA_Jiang&Wu_20210915.Rnw")
tools::texi2pdf("ggESDA_Jiang&Wu_20210915.tex")
#ref前面要加~ 
library(ggESDA)
library(grid)
library(gridExtra)
#Fig:compare
myDiamonds <- diamonds
set.seed(20211020)
myDiamonds.i  <- classic2sym(myDiamonds)$intervalData
a <- ggplot(myDiamonds, aes(x = carat, y = price))+
  geom_point()
myOrder <- c(5, 4, 3, 1, 2)
#RColorBrewer::display.brewer.all()
myCol <- RColorBrewer::brewer.pal(5, "Blues")

b <- ggInterval_scatter(myDiamonds.i, aes(x = carat, y = price)) + 
  scale_fill_manual(values = myCol[myOrder], 
                    name="Kmeans-Group", 
                    labels = c(1:5));b

grid.arrange(a, b, ncol=2)
marrangeGrob(list(a, b), ncol = 2, nrow = 1)
plot_grid(a, b, ncol = 2, rel_heights = c(1/4, 1/4, 1/2))
grid.arrange(a, b, ncol=2, widths=c(1.5, 2))
#end Fig:compare

#show_breastData_attr
breastData <- data.table::fread("doc/data.csv")
dim(breastData)
colnames(breastData)
#end show_breastData_attr


#classic2sym_kmeans
breastData <- dplyr::select(breastData, -id)
breastData.sym <- classic2sym(breastData, groupby = "kmeans", k = 5)
breastData.sym.i <- breastData.sym$intervalData
head(breastData.sym.i[, 1:4], 5)
#end classic2sym_kmeans

#classic2sym_hclust
breastData.sym <- classic2sym(breastData, groupby = "hclust")
breastData.sym.i <- breastData.sym$intervalData
#end classic2sym_hclust

#classic2sym_hclust
breastData.sym <- classic2sym(breastData, groupby = "hclust")
breastData.sym.i <- breastData.sym$intervalData
#end classic2sym_hclust

#classic2sym_parVar
breastData.sym <- classic2sym(breastData, groupby = "diagnosis")
breastData.sym.i <- breastData.sym$intervalData
head(breastData.sym.i[, 1:4], 5)
#end classic2sym_parVar



#classic2sym_userDefined
minData <- runif(100, -100, -50)
maxData <- runif(100, 50, 100)
demoData <- data.frame(min = minData, max = maxData)
demoData.sym <- classic2sym(demoData, groupby = "customize", 
                            minData = demoData$min,
                            maxData = demoData$max)

demoData.sym.i <- demoData.sym$intervalData
as.data.frame(head(demoData.sym.i, 5))
#end classic2sym_userDefined



#HistDAWass_getData
library(HistDAWass)

# Get min and max data
blood.min <- get.MatH.stats(BLOOD, stat = "min")
blood.max <- get.MatH.stats(BLOOD, stat = "max")
blood <- data.frame(blood.min, blood.max)

# Reorganized and Build ggESDA obj.
blood.sym <- classic2sym(blood, groupby = "customize",
                     minData = blood[, 2:4],
                     maxData = blood[, 6:8])

# Make names
blood.names <- get.MatH.main.info(BLOOD)$varnames
blood.i <- blood.sym$intervalData
colnames(blood.i) <- blood.names
head(as.data.frame(blood.i), 5)


#end HistDAWass_getData


#MAINT.Data_getData
library(MAINT.Data)
library(tibble)
#get data interval-valued data in AbaloneIdt
Aba.range <- exp(AbaloneIdt@LogR)
Aba.mid <- AbaloneIdt@MidP


#make a necessary transformation for build min max data
Aba <- data.frame(Aba.min = Aba.mid - Aba.range / 2,
                  Aba.max = Aba.mid + Aba.range / 2)


# Reorganized and Build ggESDA obj.
Aba.sym<- classic2sym(Aba, groupby = "customize",
                      minData = Aba[, 1:7],
                      maxData = Aba[, 8:14])


# Make names
colnames(Aba.sym$intervalData) <- AbaloneIdt@VarNames
Aba.i <- Aba.sym$intervalData %>% 
  cbind(Aba.obs = AbaloneIdt@ObsNames) %>% 
  column_to_rownames(var = "Aba.obs")

head(Aba.i[, 1:4], 5)
#end MAINT.Data







#testtttttttttttttttttttttttttt
#symbolicDA 下個版本才有example
library(HistDAWass)
#get the second variable and its histogram data
BLOOD[,2]@M[[1]]
abc<- HistDAWass::data2hist(iris[,3])
edf<- HistDAWass::data2hist(iris[,2])
?HistDAWass::register
mydist1 <- new("distributionH", c(1, 2, 3), c(0, 0.4, 1))
class(HistDAWass::plot(abc))

a<- get.MatH.stats(BLOOD, stat = "min")
b<- get.MatH.stats(BLOOD, stat = "max")
d <- data.frame(a, b)
d.sym <- classic2sym(d, groupby = "customize",
            minData = d[,2:4],
            maxData = d[,6:8])
mycolNames <- get.MatH.main.info(BLOOD)$varnames
d.i <- d.sym$intervalData
colnames(d.i) <- mycolNames
d.i



MAINT.Data::AbaloneIdt
str(MAINT.Data::AbaloneIdt)
#取中位數 @midp
#沒取就是最大最小值
#取range @LogR
MAINT.Data::AbaloneIdt[,1]@LogR
#MAINT.Data::AbaloneIdt$Shucked_weight


myrange <- exp(MAINT.Data::AbaloneIdt@LogR)
mymid <- MAINT.Data::AbaloneIdt@MidP
mymin <- mymid - myrange/2
mymax <- mymid + myrange/2
d<-data.frame(mymin,mymax)
mynames <- MAINT.Data::AbaloneIdt@VarNames
myRnames <- MAINT.Data::AbaloneIdt@ObsNames
d.sym<- classic2sym(d,groupby = "customize",
            minData = d[,1:7],
            maxData = d[,8:14])
d.i <- d.sym$intervalData
colnames(d.i) <- mynames
rownames(d.i)<-myRnames
iData<-tibble::rownames_to_column(d.i)
iData<-tibble::column_to_rownames(iData, var = "rowname")

#end testttttttttttttt

?get.MatH.stats
a <- classic2sym(mtcars,groupby=c("cyl","vs"))
a$intervalData
