# Tugas 2 PAML 
# Nama : ZULKIFLI SULTON
# NRP  : 6032221023

library(readr)
library(FactoMineR)
library(MASS)

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

# pergunakan nama merk sebagai nama baris
rownames(df) = orig_deterjen$Merek_deterjen

# sajikan beberapa data baris awal
head(df)

# sajikan descriptive statistik data
summary(df)

# cek correlation matrix
cor(df)

require(stats); 
require(graphics)

# Tampilkan scatter plot, 
# differensiasi warna berdasarkan apakah skor penjualan diatas rata-rata
pairs(df, panel = panel.smooth, pch=16, main = "Deterjen Data", col = 3 + (df$Skor_penjualan > mean(df$Skor_penjualan)))

# plot density data 
plot(density(df$Skor_penjualan),main="Skor_penjualan",xlab="Skor_penjualan")
rug(df$Skor_penjualan)
hist(df$Skor_penjualan, freq=F, add=T, col=NULL)

# multiple regresion skor penjualan berdasarkan skor produk, harga, distribusi, dan promosi.
summary(lm(Skor_penjualan ~ . , data = df))


## Classical MDS
# MDS fitting
# distance matrix data
distdf <- dist(df)

# lakukan multidimensional scaling menggunakan classical MDS.
fitdf <- cmdscale(distdf,eig=TRUE, k=2) # k is the number of dimension

# sajikan hasil multidimensional scaling
fitdf

# plot solution
x <- fitdf$points[,1]
y <- fitdf$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS", pch = 21, col = df$Skor_penjualan, bg = df$Skor_penjualan)
text(x, y, labels = row.names(df), cex=.7)
# Choose your dots: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

