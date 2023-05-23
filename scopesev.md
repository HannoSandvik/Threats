## Scope and severity

Shortly after [version 1.0](http://dx.doi.org/10.5281/zenodo.7930001) of
this code was published, IUCN has
[clarified](https://www.iucnredlist.org/resources/threat-classification-scheme)
that *severity* should be understood as the population decline “within
the scope of the particular threat”. Previously, the definition of
severity had been ambiguous, and the Norwegian Red Lists were based on
an understanding of severity as the decline of the *entire population*.
Whereas the current IUCN definition requires that threat scores are
estimated from the product of scope and severity, the alternative
definition required scope to be ignored.

Version 1.1 of this code now includes the possibility to choose between
these two definitions. (This choice is handled by the variable
`useIUCNthreats`, see [species.md](species.md).)

The multiplicative approach had earlier been applied by Garnett et al. 
([2018](http://dx.doi.org/10.1111/cobi.13220), appendix S3) and Mair et
al. ([2021](http://dx.doi.org/10.1038/s41559-021-01432-0), supplementary
table 2). It should be noted, however, that our code does not use the
same threat weights as these earlier studies. The main reasons for this
are:

-   Garnett et al. and Mair et al. assumed uniform distributions within
    each scope and severity class. We do the same for scopes (which are
    fractions of population sizes) but not for severities (which are
    population declines). Especially in the highest severity class
    (“very rapid decline” according to IUCN, “rapid decline” according
    to the Norwegian Red Lists), which spans the interval up to and
    including 100% decline (i.e. extinction), it seems unrealistic to
    treat extinction and a 30% decline as equiprobable. We therefore
    assumed a decreasing distribution for the highest severity interval
    (and an increasing distribution for the lowest of the Norwegian
    severity intervals, which is “negligible”; however, this does not
    make sense for IUCN’s lowest severity interval, which is “no
    decline”).
-   Garnett. et al. and Mair et al. used the averages of the products of
    the limits of each interval, whereas we use the products of the
    averages of the limits of each interval.

The differences are here illustrated with the IUCN classes for scope and
severity:

**(1)** By using *averages of products* of the limits of each interval,
the earlier studies obtained the following values (avoiding their
rounding errors, however):

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

**(2)** By using *products of averages* of the limits of each interval
instead, one obtains:

    table <-   average(   ScopeIUCN$lower[3:1],    ScopeIUCN$upper[3:1]) %*%
             t(average(SeverityIUCN$lower[5:1], SeverityIUCN$upper[5:1]))
    print(round(matrix(table, 3, 5, dimnames = ScopeSev), 3))

    ##                  very_rapid rapid  slow negligible no_decline
    ## whole_population      0.617 0.238 0.104      0.010          0
    ## majority              0.455 0.175 0.077      0.007          0
    ## minority              0.162 0.062 0.028      0.002          0

For some combinations of scope and severity, the differences may be
negligible. In the lower left corner of the matrix, the differences are
clear, however.

Method (2) is statistically more meaningful than method (1). If the true
values of *scope* and *severity* are uniformly distributed within their
respective intervals, their product is not. Method (2) takes this into
account, whereas method (1) assumes that the *products* of the true
values of scope and severity are uniformly distributed. The same
analogously holds for other distributions: whichever distribution one
assumes for scope and severity, their product will necessarily follow a
different distribution.

**(3)** In addition to using products of averages, we use a decreasing
distribution for the highest severity class. We therefore obtain:

    table <-   average(   ScopeIUCN$lower[3:1],       ScopeIUCN$upper[3:1]) %*%
             t(average(SeverityIUCN$lower[5:1], c(0.65, 0.3, 0.2, 0.02, 0)))
    # That's a shortcut which reproduces our mean for the highest severity class
    print(round(matrix(table, 3, 5, dimnames = ScopeSev), 3))

    ##                  very_rapid rapid  slow negligible no_decline
    ## whole_population      0.451 0.238 0.104      0.010          0
    ## majority              0.332 0.175 0.077      0.007          0
    ## minority              0.119 0.062 0.028      0.002          0
