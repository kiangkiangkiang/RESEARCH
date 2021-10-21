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
categoryData <- as.factor(sample(letters[1:5], 100, replace = TRUE))

demoData <- data.frame(min = minData,
                       max = maxData,
                       group = categoryData)

demoData.sym <- classic2sym(demoData, groupby = "customize", 
                            minData = demoData$min,
                            maxData = demoData$max)

demoData.sym.i <- demoData.sym$intervalData

#end classic2sym_userDefined



?hclust
?kmeans




?data.table::unique

b<-rbind(mtcars,mtcars[1:5,])

b[33,1]<-22

#base::duplicated(b)

test <- b[, -which(colnames(b)=="mpg")]

duplicated(test)

test[!duplicated(test), ]

aaa <- mtcars

gsub(1, "fuck", aaa$mpg)
gsub()
aaa$mpg

cl <- apply(aaa, 2, class)

bbb <- sapply(aaa, gsub, pattern = 1, replacement = "fuck", invert=F);bbb

?gsub





