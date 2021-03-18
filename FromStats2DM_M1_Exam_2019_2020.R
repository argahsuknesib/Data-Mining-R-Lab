##################################
# From Statistics to Data Mining #
#   Computer Lab Session Exam    #
#           2019-2020            #            
##################################



###############################################################################
# 1 Gaufres, Quatre-quarts, Crêpes, Îles flottantes et Beignets (40 points)   #
###############################################################################
# Linear Algebra                                                              #
###############################################################################

rm(list=ls())




# Totals quantities
totals <- matrix(c(4935, 6075, 21375, 252, 22500), ncol=1)


# gaufre
waffle <- c(80, 100, 250, 4, 350)
waffle <- waffle / 8 # the recipe seves 8


# quatre-quarts
quatre_quarts <- c(125, 125, 125, 2, 0)
quatre_quarts <- quatre_quarts / 6  # the recipe serves 6

# crepes
crepes <- c(50, 20, 250, 4, 500)
crepes <- crepes / 10  # the recipe serves 10

# iles flottantes
iles_flottantes <- c(0, 60, 0, 5, 600)
iles_flottantes <- iles_flottantes / 5   # the recipe serves 5

# beignets
beignets <- c(120, 120, 1000, 4, 300)
beignets <- beignets / 6  # the recipe serves 6


lin_eq <- cbind(waffle, quatre_quarts, crepes, iles_flottantes, beignets)
lin_eq




# sum(totaux[1,])

# system of linear equations
# lin_eq <- cbind(quatre_quarts, sables, crepes, iles_flottantes, beignets)

rownames(lin_eq) <- c("butter", "sugar", "flour", "eggs", "milk")
lin_eq

# Question 1 (30 points)
# solving the system of linear equations with the totals
people <- solve(lin_eq, totals)

people

# people <- matrix(c(96, 90, 60, 90, 90), ncol=1)


# We can check the results
lin_eq  %*% people

# Question 2 (10 points)
qtt <- c(1/8, 1/6, 20/10, 1/5, 18/6)
# waffles: serves 8
# quatre-quarts: serves 6
# crepes: serves 10 for 20 crêpes
# floating island: serves 5
# beignets: serves 6 for 18 midsize beignets

qtt * people
# Preparation: 12 pâtes à gaufres, 15 quatre-quarts, 120 crêpes, 18 floating islands and 270 beignets



###############################################################################
# 2. Space Battle (60 points)                                                 #
###############################################################################
# 3D plot, PCA, Clustering and Clustering Quality Index                       #
###############################################################################


library(rgl)

mydata <- read.csv("~/R/data/space_battle.csv", header=T)


# Question 1 (5 points)

pairs(mydata)  # pairwise representation
# How many clusters?
# Maybe 5...


# Question 2 (20 points)

# Other representation 1: 3D
plot3d(x=mydata[,1],
       y=mydata[,2],
       z=mydata[,3])

# How many clusters?
# 7?

# Other representation 2: PCA
mydata.pca <- princomp(mydata)
mydata.pca
win.graph(800, 600, 10)
biplot(mydata.pca)

# How many clusters?
# 7?

# Other possibility (2'): dendrogram
hc <- hclust(dist(mydata), "ave")
plot(hc)
# We will try to plot 7 clusters on the dendrogram
rect.hclust(hc, k=7)




# Question 3 (30 points)

# Search for the number of clusters

n = nrow(mydata)
CH <- matrix(nrow=10, ncol=1)
for (k in 2:10)
{
  print(paste("Test with k=",k))
  CH_loop <- matrix(nrow=5, ncol=1)
  for (i in 1:5)
  {
    print(paste("Test ",i))
    kmeans.result <- kmeans(mydata,k)
    kmeans.result
    w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
    b <- (kmeans.result$betweenss/kmeans.result$totss)*100
    print(paste("WSS=",round(w,2),"%"))
    print(paste("BSS=",round(b,2),"%"))
    CH_index <- (b / (k - 1)) / (w / (n - k))
    CH_loop[i][1]=CH_index
    print(paste("CH index=",round(CH_index,2)))
    print("********")
  }
  max_CH <- max(CH_loop)
  CH[k][1] <- max_CH
  print(paste("max CH index=",round(max_CH,2)))
  print("##########")
}
CH

# Maximal value for C and H index: 210.45628 for k=7


# Question 4 (5 points)

# 3D Representation with the color for each cluster

kmeans.result <- kmeans(mydata,7)

plot3d(x=mydata[,1],
       y=mydata[,2],
       z=mydata[,3],
       col=kmeans.result$cluster)

pairs(mydata, col=kmeans.result$cluster)

