Clustering on Chemical Element Data
================
Erin Gregoire,
March 2025

The “chorSub” data contains measurements of 10 chemicals in 61
geological samples from the Kola Peninsula. In this mini-project, I
demonstrate my skills at using two different methods of clustering -
kmeans and hierarchical.

Preprocessing and Exploratory Data Analysis:

``` r
library(fpc)
```

    ## Warning: package 'fpc' was built under R version 4.5.1

``` r
library(cluster)
library(fossil)
```

    ## Warning: package 'fossil' was built under R version 4.5.1

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 4.5.1

    ## Loading required package: maps

    ## Warning: package 'maps' was built under R version 4.5.1

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:cluster':
    ## 
    ##     votes.repub

    ## Loading required package: shapefiles

    ## Loading required package: foreign

    ## 
    ## Attaching package: 'shapefiles'

    ## The following objects are masked from 'package:foreign':
    ## 
    ##     read.dbf, write.dbf

``` r
data(chorSub)
?chorSub # data is already scaled
```

    ## starting httpd help server ...

    ##  done

``` r
str(chorSub)
```

    ##  int [1:61, 1:10] 101 50 5 -40 -13 -49 44 285 4 -48 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:61] "190" "191" "192" "193" ...
    ##   ..$ : chr [1:10] "Al" "Ca" "Fe" "K" ...

``` r
summary(chorSub)
```

    ##        Al                 Ca                 Fe                K           
    ##  Min.   :-201.000   Min.   :-178.000   Min.   :-200.00   Min.   :-133.000  
    ##  1st Qu.: -49.000   1st Qu.: -59.000   1st Qu.: -75.00   1st Qu.: -74.000  
    ##  Median :  -1.000   Median :   2.000   Median : -37.00   Median : -17.000  
    ##  Mean   :   0.541   Mean   :  -2.066   Mean   : -14.64   Mean   :  -4.295  
    ##  3rd Qu.:  47.000   3rd Qu.:  59.000   3rd Qu.:  48.00   3rd Qu.:  51.000  
    ##  Max.   : 285.000   Max.   : 211.000   Max.   : 162.00   Max.   : 248.000  
    ##        Mg                Mn                Na                P           
    ##  Min.   :-155.00   Min.   :-139.00   Min.   :-242.00   Min.   :-102.000  
    ##  1st Qu.: -75.00   1st Qu.: -66.00   1st Qu.: -25.00   1st Qu.: -61.000  
    ##  Median : -30.00   Median : -41.00   Median :  47.00   Median : -36.000  
    ##  Mean   : -13.05   Mean   : -13.92   Mean   :  17.97   Mean   :  -6.623  
    ##  3rd Qu.:  31.00   3rd Qu.:   7.00   3rd Qu.:  81.00   3rd Qu.:   8.000  
    ##  Max.   : 254.00   Max.   : 354.00   Max.   : 187.00   Max.   : 406.000  
    ##        Si                 Ti          
    ##  Min.   :-223.000   Min.   :-190.000  
    ##  1st Qu.: -46.000   1st Qu.: -73.000  
    ##  Median :  22.000   Median : -24.000  
    ##  Mean   :   8.328   Mean   :  -7.361  
    ##  3rd Qu.:  60.000   3rd Qu.:  55.000  
    ##  Max.   : 177.000   Max.   : 351.000

``` r
pairs(chorSub)
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

This basic pairs plot shows a breakdown of how each variable relates to
each other. This plot is a useful starting point to get a scope of which
variables should be further explored. We can see that there is a
significant amount of correlation between nearly all of these variables.

``` r
boxplot(chorSub, main = "Boxplot of C-horizon data", xlab = "Chemical Elements", ylab = "Range")
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

This shows the distribution of values for each variable. This graph is
useful as it shows the outliers that persist in many of the chemicals.

Implementing K-means clustering:

``` r
gap_kmeans <- clusGap(chorSub, kmeans, nstart = 25, K.max = 10, B = 1000)
plot(gap_kmeans, main = "Gap Statistic: kmeans")
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This graph shows the performance of k-means with different values of k
for the number of clusters used. Typically, we would like to see a
definitive elbow denoting where the best value for k is. However, this
data does not converge well. Since the gap statistic did not find a
definitive best value for k, I will use a different technique to see if
that provides better results.

``` r
k_values <- c(seq(1, 10, by = 1))
within_cluster <- c()

for (i in k_values){
  km <- kmeans(chorSub, centers = i, nstart = 10)
  within_cluster <- c(within_cluster, km$tot.withinss)
}

plot(k_values, within_cluster, type="b", main = "K-Values on the Total Within-Cluster Sum of Squares", xlab = "K-Values", ylab = "Within-Cluster")
axis(1, at=k_values, labels = TRUE) 
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The second technique that I used to assist in determining a value for k
was to plot the within-cluster sum of squares of the k-means algorithms
for different values of k. Again, there is not definitive elbow but this
method does show more covergence than the gap statistic. Based off of
the Within-cluster sum of squares and the gap statistic, I belive that
the best value for k is 5. This is because, when looking at the gap
statistic plot, the values for 3 and 4 are very similar but 5 provides
more distance. Then, on the within-cluster sum of squares, the slope
between 5 and 6 begins to decrease much more than the first few values.

``` r
best_kmeans <- kmeans(chorSub, centers = 5, nstart = 25)

names(best_kmeans)
```

    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
best_kmeans$cluster
```

    ## 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 
    ##   4   5   4   2   4   4   1   5   2   5   1   1   2   3   2   1   1   4   5   4 
    ## 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 
    ##   2   1   3   4   4   4   5   4   4   4   1   1   2   3   2   5   4   5   4   4 
    ## 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 
    ##   2   2   2   4   2   4   4   4   2   1   5   4   1   4   4   5   5   2   2   5 
    ## 250 
    ##   2

After choosing the best k value for k to be 5, the best k-means
algorithm was created with 5 clusters. The results are shown here.

Implementing Hierarchical clustering:

``` r
hier_clust <- function(x, k, d.meth = "euclidean")
  list(cluster = cutree(hclust(dist(x, method=d.meth)), k=k))
gap_hclust <- clusGap(chorSub, hier_clust, K.max = 10, B = 1000)
plot(gap_hclust, main = "Gap Statistic: Hierarchical Clustering")
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This graph shows the gap statistic when performed using hierarchical
clustering. Based on this graph, the best value for k, that will denote
the number of clusters to use, is 7. This is because, although there is
some variance that occurs (mostly at a k value of 4), the line takes a
big leap upward at 7.

``` r
d <- dist(chorSub)
best_hclust <- hclust(d, method = "ave")
plot(best_hclust, main = "C-Horizon Cluster Dendogram", xlab = "Dissimilarity")
```

![](Clustering-on-Chemical-Element-Data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This graph shows the dendogram of clusters formed from the C-horizon
chemical element data. Another way to choose the height at which to cut
to determine clusters is to find an obvious breaking point as seen on
this dendogram. Since we already chose a k value from the gap statistic,
that is not needed here but can be a handy tool.

``` r
best_hclust_cut <- cutree(best_hclust, k = 7)
best_hclust_cut
```

    ## 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 
    ##   1   2   1   1   1   1   1   3   1   2   1   4   1   5   1   1   1   1   2   1 
    ## 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 
    ##   1   4   6   1   1   1   2   1   1   1   4   1   1   7   1   2   1   2   1   1 
    ## 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 
    ##   1   1   1   1   1   1   1   1   1   4   2   1   4   1   1   2   2   1   1   1 
    ## 250 
    ##   1

Now that a best value for k has been chosen, the best version of the
hierarchical clustering on the c-horizon data was performed. First, the
hierarchical clustering was performed and then the dendogram was cut
using a k value of 7. The resulting clusters are shown.

Comparative Analysis:

``` r
table(best_kmeans$cluster, best_hclust_cut)
```

    ##    best_hclust_cut
    ##      1  2  3  4  5  6  7
    ##   1  5  0  0  5  0  0  0
    ##   2 15  0  0  0  0  0  0
    ##   3  0  0  0  0  1  1  1
    ##   4 22  0  0  0  0  0  0
    ##   5  1  9  1  0  0  0  0

To compare the two clustering algorithms, I looked at the observations
that fell into each of their clusters by creating a matrix of which
observations fell into the two groups.
