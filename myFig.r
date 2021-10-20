Sweave("ggESDA_Jiang&Wu_20210915.Rnw")
tools::texi2pdf("ggESDA_Jiang&Wu_20210915.tex")
#ref前面要加~ 
library(ggESDA)
library(grid)
library(gridExtra)
testData <- data.table::fread("doc/data.csv")

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





