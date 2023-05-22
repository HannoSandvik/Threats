## Scope and severity

After version 1.0 of this code was published, IUCN has
[clarified](https://www.iucnredlist.org/resources/threat-classification-scheme)
that *severity* should be understood as the population decline “within
the scope of the particular threat”. Previously, the definition of
severity had been ambiguous, and the Norwegian Red Lists were based on
an understanding of severity as the decline of the *entire population*.
Whereas the current IUCN definition requires that threat scores are
estimated from the product of scope and severity, the alternative
definition required scope to be ignored.

Version 1.1 of this code now includes the possibility to choose between
these two definitions.

The multiplicative approach had earlier been used by Garnett et al. 
([2018](http://dx.doi.org/10.1111/cobi.13220), appendix S3) and Mair et
al. ([2021](http://dx.doi.org/10.1038/s41559-021-01432-0), supplementary
table 2). It should be noted, however, that the present approach does
not give the same threat weights as these earlier studies. The main
reasons for this are:

-   Garnett et al. and Mair et al. chose uniform distributions within
    each scope and severity class. We do the same for scopes (which are
    fractions of areas) but not for severities (which are population
    declines). Especially for the highest severity class (“very rapid
    decline” according to IUCN, “rapid decline” according to the
    Norwegian Red Lists), it is unrealistic to assume that a 99% decline
    and a 31% decline are equiprobable. We therefore assumed a
    decreasing distribution for the highest severity interval (and an
    increasing distribution for the lowest of the Norwegian severity
    intervals, which is “negligible”; however, this does not make sense
    for IUCN’s lowest severity interval, which is “no decline”).
-   Garnett. et al. and Mair et al. used the averages of the products of
    the limits of each interval, whereas we use the products of the
    averages of the limits of each interval.

The differences are here illustrated with the IUCN classes for scope and
severity:

**(1)** By using *averages of products*, the earlier studies obtained
the following values (avoiding their rounding errors, however):

    ScopeSev <- list(
      c("whole_population", "majority", "minority"),
      c("very_rapid", "rapid", "slow", "negligible", "no_decline")
    )
    average <- function(x, y) (x + y) / 2
    table <- average(ScopeIUCN$lower[3:1] %*% t(SeverityIUCN$lower[5:1]),
                     ScopeIUCN$upper[3:1] %*% t(SeverityIUCN$upper[5:1]))
    print(round(matrix(table, 3, 5, dimnames = ScopeSev), 3))

    ##                  very_rapid rapid  slow negligible no_decline
    ## whole_population      0.635 0.240 0.109      0.010          0
    ## majority              0.525 0.185 0.095      0.009          0
    ## minority              0.250 0.075 0.050      0.005          0

**(2)** By using *products of averages* instead, one obtains:

    table <-   average(   ScopeIUCN$lower[3:1],    ScopeIUCN$upper[3:1]) %*%
             t(average(SeverityIUCN$lower[5:1], SeverityIUCN$upper[5:1]))
    print(round(matrix(table, 3, 5, dimnames = ScopeSev), 3))

    ##                  very_rapid rapid  slow negligible no_decline
    ## whole_population      0.617 0.238 0.104      0.010          0
    ## majority              0.455 0.175 0.077      0.007          0
    ## minority              0.162 0.062 0.028      0.002          0

For most combinations of scope and severity, the differences are
negligible. In the lower left corner of the matrix, the differences are
clear, however.

The choice between (1) and (2) is important mainly if one wishes to
quantify uncertainties (e.g. by re-sampling procedure). In this case,
one obtains a distribution of randomised threat scores. The mean of this
distribution is given by (2), whereas (1) would overestimate it.

**(3)** In addition to using products of averages, we use a decreasing
distribution for the highest severity class. We therefore obtain:

    table <-   average(   ScopeIUCN$lower[3:1],       ScopeIUCN$upper[3:1]) %*%
             t(average(SeverityIUCN$lower[5:1], c(0.65, 0.3, 0.2, 0.02, 0)))
    print(round(matrix(table, 3, 5, dimnames = ScopeSev), 3))

    ##                  very_rapid rapid  slow negligible no_decline
    ## whole_population      0.451 0.238 0.104      0.010          0
    ## majority              0.332 0.175 0.077      0.007          0
    ## minority              0.119 0.062 0.028      0.002          0
