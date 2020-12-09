library("mlr")           
library("tidyverse")     
library("DataExplorer")
library("factoextra")    
library("dendextend")    
library("reshape2")      
library("ggforce")       
library("cluster")       
library("dplyr")         
library("corrplot")
library("cluster")
library("NbClust")
library("gridExtra")
library("GGally")
library("readr") 
library("kableExtra") 
library("caret")
library("rlang")
library("tidyr")
library("purrr")
library("ggplot2")
library("kselection")

# Reading the dataset
dat <- read.csv("C:/Users/akshay/Desktop/Credit Card/credit-card-data.csv", stringsAsFactors = F, na.strings = c(" "))

glimpse(dat)

str(dat)

summary(dat)

sum(is.na(dat))   #Trying to find NA values.

plot_missing(dat) # CREDIT_LIMIT and MINIMUM_PAYMENTS have missing values.

plot_histogram(dat) # Nearly all variables are skewed and/or have some outliers.

dat_clust = dat %>% # drop the missing values, since imputing the mean would probably add noise to the data.
  select(-CUST_ID) %>% 
  drop_na()

# Correlation Plot
corrplot(cor(dat_clust), diag = FALSE, type = "upper", order = "hclust", tl.col = "black", tl.pos = "td", tl.cex = 0.9, method = "circle")

# Hieracical Clustering
fit_hc_clust = hclust(dist(scale(dat_clust), method = "euclidean"), method = "ward")

# Constructing dendogram of 6 clusters
plot(fit_hc_clust, labels = FALSE, sub = "", xlab = "", ylab = "Euclidean distance")
rect.hclust(fit_hc_clust, k = 6)

# Cutting the dendrogram for 6 clusters
hc_cluster = cutree(fit_hc_clust, k = 6)

# The PCA plots the data in two-dimensional space. 
# Overall, there are no clear clusters in the data. 
# However, the generated clusters look quite noisy since they are overlapping.
hc_pc = prcomp(scale(dat_clust))

fviz_pca_ind(hc_pc, habillage = hc_cluster)

# Silhouette plot shows if an observation is associated with the right (1) or wrong (-1) cluster. 
# The average silhouette width is quite low. 
# Many observations probably in the wrong clusters.
hc_sil = silhouette(hc_cluster, dist(scale(dat_clust), method = "euclidean"), lable = FALSE)

fviz_silhouette(hc_sil, print.summary = FALSE) + theme_minimal()

# K-Means fit with 4 clusters.
# Regarding the wss plot 4 clusters seem to be a proper number of clusters.
fviz_nbclust(scale(dat_clust), kmeans, method = "wss", k.max = 10)

fit_km = kmeans(scale(dat_clust), centers = 4)

fviz_pca_ind(hc_pc, habillage = fit_km$cluster) #  PCA plot looks better then the plot before

# Silhouette plot
# Overall, the result is better than before. 
# However, especially cluster 1 and 3 have still some observations which are still in the wrong cluster. 
# But it´s the best solution for now which we will use for interpretation.
hc_sil = silhouette(fit_km$cluster, dist(scale(dat_clust), method = "euclidean"), lable = FALSE)

fviz_silhouette(hc_sil, print.summary = FALSE) + theme_minimal()

# In order to iterpretate the clusters grouped boxplots will be used.
c = dat_clust

c$cluster = fit_km$cluster

c_plots = melt(c, id.var = "cluster")

c_plots$cluster = as.factor(c$cluster)

# PART I
c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# PART II
c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 2) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
  
# PART III
c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# Cluster 1: Frequent user, with (probably) lower income that spends his money mostly on consumer goods.
# Cluster 2: Frequent user, with (probably) higher income that spends his money mostly on consumer goods.
# Cluster 3: Mid to rare users, with (probably) mid to high income which spends his money more for higher priced products with longterm use.
# Cluster 4: Rare user, with (probably) mid to low income which spends his money more on consumer goods.