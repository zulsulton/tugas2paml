# Tugas 2 PAML 
# Nama : ZULKIFLI SULTON
# NRP  : 6032221023

library(readr)
library(FactoMineR)
library(MASS)

library(factoextra)
library(cluster)

# getwd()
# setwd(...)
# cmdscale()
# isoMDS()
# sammon()

# orig_deterjen <- read_csv("G:/My Drive/Kuliah/administrations/Semester 3/PAML/Tugas 2/deterjen.csv")
orig_deterjen <- read_csv("https://github.com/zulsulton/tugas2paml/raw/main/deterjen.csv")
head(orig_deterjen)

# hanya pergunakan kolom-kolom yang akan dianalisis, drop kolom yang tidak diperlukan.
df <- subset(orig_deterjen, select = -c(1, 2, 3))

# normalize data numerik
df <- scale(df)

# pergunakan nama merk sebagai nama baris
rownames(df) = orig_deterjen$Merek_deterjen

# sajikan beberapa data baris awal
head(df)

# sajikan descriptive statistik data
summary(df)

# cari nilai k optimal
# Optimal number of clusters in the data
# ++++++++++++++++++++++++++++++++++++++
# Examples are provided only for kmeans
## Elbow method (look at the knee)
## Elbow method for kmeans
nbclust_wss = fviz_nbclust(df, FUN=kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2)

# memvisualisasikan elbow
nbclust_wss

# Compute k-means 
set.seed(123)

# dengan menggunakan metode "wss" untuk mendapatkan nilai optimum k, 
# didapatkan nilai optimal k adalah 2, 
# pertimbangan pemilihan angka 2 adalah bahwa jumlah data terlalu sedikit 
# untuk dibagi menjadi lebih banyak cluster.
k = 2
km.res <- kmeans(df, k, nstart = 25)

# Print the results
print(km.res)

hasil_akhir = aggregate(orig_deterjen, by=list(cluster=km.res$cluster), mean)
hasil_akhir

dd <- cbind(orig_deterjen, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster

head(km.res$cluster, 4)

# Cluster size
km.res$size

# Cluster means
km.res$centers

fviz_cluster(km.res, df[,-5], ellipse.type = "norm")

