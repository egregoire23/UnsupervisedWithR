PageRank Algorithm
================
Erin Gregoire,
May 2025

The PageRank algorithm is useful for determining the importance of web
pages based on link structure, and in modern day, it is typically built
into many advanced ranking systems. In this mini-project, I will utilize
the following page data to implement the PageRank algorithm.

Preprocessing & Exploratory Data Analysis:

``` r
nodes <- data.frame(names = c("PageA", "PageB", "PageC", "PageD", "PageE"))
relations <- data.frame(from = c("PageA", "PageA", "PageB", "PageC", "PageD", "PageD", "PageE"), 
                        to = c("PageB", "PageC", "PageC", "PageA", "PageA", "PageB", "PageD"))

relations
```

    ##    from    to
    ## 1 PageA PageB
    ## 2 PageA PageC
    ## 3 PageB PageC
    ## 4 PageC PageA
    ## 5 PageD PageA
    ## 6 PageD PageB
    ## 7 PageE PageD

``` r
g <- graph_from_data_frame(relations, directed = TRUE, vertices = nodes)
plot(g, main = "Web Page DAG")
```

![](PageRank-algorithm_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

This plot shows the structure of the directed graph between the web
pages. The structure can also be seen as a matrix with the relations
listed as to and from connections.

Implementing the PageRank algorithm:

``` r
N <- dim(nodes)[1] # total number of pages
init_ranks <- c(rep(1/N, N))
damping_fac <- .85

pr_scores <- page_rank(g, damping = damping_fac)$vector
pr_scores
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.3551670 0.2045335 0.3547995 0.0555000 0.0300000

The page rank scores for each of the five pages are seen here.

``` r
nodes2 <- data.frame(names = c("PageA", "PageB", "PageC", "PageD", "PageE"))
relations2 <- data.frame(from = c("PageB", "PageC", "PageD", "PageD", "PageE"), 
                        to = c("PageC", "PageA", "PageA", "PageB", "PageD"))

relations2 # removed outbound links from Page A
```

    ##    from    to
    ## 1 PageB PageC
    ## 2 PageC PageA
    ## 3 PageD PageA
    ## 4 PageD PageB
    ## 5 PageE PageD

``` r
g2 <- graph_from_data_frame(relations2, directed = TRUE, vertices = nodes2)
plot(g2, main = "Web Page DAG with Missing Outbound Links")
```

![](PageRank-algorithm_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
pr_scores2 <- page_rank(g2, damping = damping_fac)$vector
pr_scores2
```

    ##      PageA      PageB      PageC      PageD      PageE 
    ## 0.35436167 0.16119385 0.22725626 0.16694674 0.09024148

After removing all outgoing links from Page A, but still keeping inbound
links to Page A, its PageRank score did not change very drastically.
However, the other pages’ PageRank scores did change drastically.

The damping factor accounts for the fact that at any given point, the
user will no longer want to continue on clicking forward with links but
instead chose a new page from scratch at random. To account for this
randomness and realistic nature that a user will not continue forward
forever, the damping factor is used. It affects the ranking of the pages
because the probability of the rank is multiplied by a constant damping
factor. If the damping factor is .15, this means that 15% of the time,
the user will get bored and start from scratch. The other 85% of the
time, the user will continue clicking forward on the links. So, the
probability of the rank gets the added constant probability of the
damping tacked on to measure this.

``` r
pr_scores_85 <- page_rank(g, damping = .85)$vector # standard value, also used in question 1
pr_scores_85
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.3551670 0.2045335 0.3547995 0.0555000 0.0300000

``` r
pr_scores_99 <- page_rank(g, damping = .99)$vector # very close to maximum damping factor value
pr_scores_99
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.3968144 0.2003932 0.3968124 0.0039800 0.0020000

``` r
pr_scores_50 <- page_rank(g, damping = .50)$vector 
pr_scores_50
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.2730769 0.2057692 0.2711538 0.1500000 0.1000000

``` r
pr_scores_25 <- page_rank(g, damping = .25)$vector 
pr_scores_25
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.2307927 0.2022866 0.2294207 0.1875000 0.1500000

``` r
pr_scores_01 <- page_rank(g, damping = .01)$vector # very close to minimum damping factor value
pr_scores_01
```

    ##     PageA     PageB     PageC     PageD     PageE 
    ## 0.2010100 0.2000049 0.2010051 0.1999800 0.1980000

In this example, the damping factor had a significant influence on the
PageRank scores. With the higher damping factors of .85 and .99, there
is a clear pattern of which pages are most likely to be clicked on and
two pages that are rarely visited. However with the very low damping
factors of .25 and .01, the randomness takes hold and nearly all of the
pages are equally likely to be clicked on.
