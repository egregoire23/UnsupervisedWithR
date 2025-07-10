Bayesian Network on Heart Disease Data
================
Erin Gregoire,
April 2025

The Bayesian Network has many use cases and is often tested on medical
data due to its categorical nature. With the coronary heart disease data
from the Danish Heart Clinic, I demonstrate my skills using the
hill-climbing algorithm to learn a Bayesian Network for this data.

Preprocessing & Exploratory Data Analysis:

``` r
str(cad1)
```

    ## 'data.frame':    236 obs. of  14 variables:
    ##  $ Sex        : Factor w/ 2 levels "Female","Male": 2 2 1 2 2 2 2 2 1 2 ...
    ##  $ AngPec     : Factor w/ 3 levels "Atypical","None",..: 2 1 2 2 2 2 2 2 2 1 ...
    ##  $ AMI        : Factor w/ 2 levels "Definite","NotCertain": 2 2 1 2 2 2 2 2 2 2 ...
    ##  $ QWave      : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 2 2 1 1 ...
    ##  $ QWavecode  : Factor w/ 2 levels "Nonusable","Usable": 2 2 2 2 2 2 2 2 1 2 ...
    ##  $ STcode     : Factor w/ 2 levels "Nonusable","Usable": 2 2 2 1 1 1 1 1 1 2 ...
    ##  $ STchange   : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 2 ...
    ##  $ SuffHeartF : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Hypertrophi: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Hyperchol  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Smoker     : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Inherit    : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Heartfail  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CAD        : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...

``` r
boxplot(cad1, main = "Distribution of Factor Variables in CAD1 data")
```

![](Bayesian-Network-on-Heart-Disease-Data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

This graph shows the distribution of the features in the Danish Heart
Clinic coronary heart disease data. For the most part, since these
features are all factor variables, this doesn’t give us the most
beneficial overview.

Fitting the hill-climbing algorithm for the Bayesian Network:

``` r
cad_bn <- hc(cad1)
cad_bn
```

    ## 
    ##   Bayesian network learned via Score-based methods
    ## 
    ##   model:
    ##    [AngPec][STcode|AngPec][SuffHeartF|STcode][Hypertrophi|SuffHeartF]
    ##    [QWavecode|STcode:Hypertrophi][Heartfail|STcode:Hypertrophi]
    ##    [CAD|AngPec:Heartfail][Sex|CAD][AMI|CAD][STchange|STcode:Hypertrophi:CAD]
    ##    [Hyperchol|CAD][Smoker|CAD][Inherit|CAD][QWave|AMI:CAD]
    ##   nodes:                                 14 
    ##   arcs:                                  19 
    ##     undirected arcs:                     0 
    ##     directed arcs:                       19 
    ##   average markov blanket size:           3.29 
    ##   average neighbourhood size:            2.71 
    ##   average branching factor:              1.36 
    ## 
    ##   learning algorithm:                    Hill-Climbing 
    ##   score:                                 BIC (disc.) 
    ##   penalization coefficient:              2.731916 
    ##   tests used in the learning procedure:  338 
    ##   optimized:                             TRUE

``` r
score(cad_bn, data = cad1, type = "bic")
```

    ## [1] -1783.82

``` r
arc.strength(cad_bn, data = cad1, criterion = "bic")
```

    ##           from          to   strength
    ## 1       STcode    STchange -49.249093
    ## 2       AngPec         CAD -34.981981
    ## 3  Hypertrophi   Heartfail -26.055325
    ## 4       STcode  SuffHeartF -27.095552
    ## 5          CAD         AMI -20.223003
    ## 6          CAD       QWave  -6.885522
    ## 7          CAD   Hyperchol -14.411615
    ## 8       AngPec      STcode  -8.569443
    ## 9   SuffHeartF Hypertrophi  -6.967166
    ## 10         CAD     Inherit  -6.825920
    ## 11         CAD      Smoker  -5.133059
    ## 12      STcode   QWavecode  -4.530218
    ## 13         AMI       QWave  -4.087649
    ## 14   Heartfail         CAD  -3.989082
    ## 15         CAD    STchange -14.598555
    ## 16 Hypertrophi    STchange -13.009014
    ## 17      STcode   Heartfail  -1.349319
    ## 18         CAD         Sex  -1.105395
    ## 19 Hypertrophi   QWavecode  -1.057690

``` r
plot(cad_bn, main = "Bayesian Network of CAD data")
```

![](Bayesian-Network-on-Heart-Disease-Data_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This plot shows the resulting Bayesian Network that was formed from the
hill-climbing algorithm on the Danish Heart Clinic coronary heart
disease data. In the network, we can see the fourteen discrete variables
and how they relate to one another including their directions.

``` r
modelstring(cad_bn)
```

    ## [1] "[AngPec][STcode|AngPec][SuffHeartF|STcode][Hypertrophi|SuffHeartF][QWavecode|STcode:Hypertrophi][Heartfail|STcode:Hypertrophi][CAD|AngPec:Heartfail][Sex|CAD][AMI|CAD][STchange|STcode:Hypertrophi:CAD][Hyperchol|CAD][Smoker|CAD][Inherit|CAD][QWave|AMI:CAD]"

This string provides the joint distribution of all the variables in the
fitted Bayesian Network into compact factored form.

Making predictions for possible patients:

``` r
cad_bn_fit <- bn.fit(cad_bn, data = cad1, method = "mle")

base_cad_prob <- cpquery(cad_bn_fit, event = (CAD == "Yes"), evidence = TRUE)
base_cad_prob
```

    ## [1] 0.4538

``` r
query_c <- cpquery(cad_bn_fit, event = (CAD == "Yes"), evidence = (Sex == "Female") & (Smoker == "Yes"))
query_c
```

    ## [1] 0.3151233

In the event that the patient is female and a smoker, the probability
that they will have coronary heart disease is in the low 30%. Compared
to the baseline probability of having coronary heart disease, which is
approximately 45%, this female smoker is not considered to be at a
higher risk for CAD. This is an interesting finding since we tend to
think that smokers will have a higher risk of these types of diseases.

``` r
base_cad_prob
```

    ## [1] 0.4538

``` r
query_d <- cpquery(cad_bn_fit, event = (CAD == "Yes"), evidence = (Sex == "Male") & (AngPec == "Atypical"))
query_d
```

    ## [1] 0.2110553

A second patient is male and has an atypical angina pectoris. The
probability that they will have coronary heart disease is in the low
20%. Compared to the baseline probability of having this coronary heart
disease, this male patient with an atypical angina pectoris does not
appear to be at high risk for the disease.
