set.seed(1)
library(ordinalClust)
# loading the real dataset
data("dataqol")
# loading the ordinal data
x <- as.matrix(dataqol[,2:31])
# defining different number of categories:
m <- c(4,7)
# defining number of row and column clusters
krow <- 3
kcol <- c(3,1)
# configuration for the inference
nbSEM <- 20
nbSEMburn <- 15
nbindmini <- 2
init <- 'random'

d.list <- c(1,29)
# Co-clustering execution
browser()
object <- boscoclust(x = x,kr = krow, kc = kcol, m = m,
                idx_list = d.list, nbSEM = nbSEM,
                nbSEMburn = nbSEMburn, nbindmini = nbindmini,
                init = init)