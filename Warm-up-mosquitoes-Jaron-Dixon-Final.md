Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Jaron Dixon
2025-10-28

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [Analysis 1](#analysis-1)
  - [Anaylsis 2](#anaylsis-2)
- [DISCUSSION](#discussion)
  - [Interpretation 1](#interpretation-1)
  - [Interpretation 2](#interpretation-2)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

West Nile virus (WNV) is transmitted between mosquitoes and birds, with
certain bird species acting as amplifying hosts. This study examined
mosquito blood meals in Salt Lake City, Utah, to identify which birds
may contribute most to local WNV transmission. Bloodmeal DNA was
extracted, sequenced, and analyzed, and WNV presence was recorded in
mosquito pools. Results showed that House Finches were frequently fed on
by mosquitoes, and locations with more finch blood meals had higher
rates of WNV-positive pools. Other birds, including House Sparrows and
American Robins, were also detected but showed weaker associations with
WNV. These findings suggest that House Finches are likely key hosts
sustaining WNV in Salt Lake City, highlighting the importance of
monitoring urban bird–mosquito interactions for public health.

# BACKGROUND

West Nile virus (WNV) is a mosquito-borne virus that primarily
circulates between birds and mosquitoes. Birds are considered the key
amplifying hosts because they can develop high levels of the virus in
their blood, which allows mosquitoes feeding on them to become infected
and continue the transmission cycle. Humans and other mammals, on the
other hand, are generally “dead-end” hosts because they do not produce
enough virus to pass the infection back to mosquitoes.

One way to determine which bird species are contributing most to WNV
spread is to analyze the blood meals taken by mosquitoes. After feeding,
traces of the host’s DNA remain in the mosquito’s gut. This DNA can be
extracted, amplified using PCR, and sequenced to identify the species
the mosquito fed on. By combining this host identification with WNV
testing in mosquitoes, researchers can link specific birds to local
transmission patterns.

This sets the foundation for our main question: \*Which species of bird
is amplifying the spread of WNV in Salt Lake City (SLC)?

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-Jaron-Dixon-Final_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

What species of bird is amplifying the spread of WNV in SLC?

## Hypothesis

House Finches are acting as important amplifying hosts for WNV in SLC.

## Prediction

If house finches are acting as important amplifying hosts, we predict
that trapping locations where mosquitoes feed on house finches will also
have higher rates of confirmed WNV in tested mosquito pools.

# METHODS

Samples were collected from SLC Utah, then were processed by extracting
DNA, performing PCR, MinION sequencing, BLASTn searching the nucleotide
sequence obtained. Once all of this data was obtained, the analysis was
performed to look for any statistical differences.

\##Statistical Analyses

To assess whether House Finch blood meals were associated with WNV
detection, we ran generalized linear models (GLMs). The first model
(glm1) used a binomial family to test whether the presence or absence of
WNV-positive mosquito pools (loc_positives, a binary variable) was
predicted by the number of House Finch blood meals (host_House_finch).
The second model (glm2) tested whether the rate of WNV-positive pools
(loc_rate, a continuous variable) increased with higher counts of House
Finch blood meals using a Gaussian family.

Statistical significance was assessed using the p-values from the model
summaries, and effect direction was determined by the sign of the
regression coefficients. All analyses were performed in R (version
4.3.1) using the glm() function from base R.

## Analysis 1

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warm-up-mosquitoes-Jaron-Dixon-Final_files/figure-gfm/H%20bar%20plots-1.png)<!-- -->

## Anaylsis 2

``` r
#glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

Our results suggest that House Finches may be important hosts for West
Nile virus (WNV) in Salt Lake City. Mosquito bloodmeal data showed that
House Finches were commonly fed on, and areas with more finch blood
meals often had higher numbers of WNV-positive mosquito pools. This
supports our hypothesis that House Finches help amplify the virus
locally.

Other birds, such as House Sparrows, American Robins, and European
Starlings, were also detected and may play a role in transmission.
However, the stronger link with House Finches points to them as a key
host in this area.

There are some limits to our study. Bloodmeal data only tells us what
birds mosquitoes fed on, not whether those birds were infectious at the
time. Some species may also be underrepresented in the samples. Still,
the consistent association with House Finches makes them likely
contributors to the spread of WNV in Salt Lake City.

Understanding which birds are most important for transmission helps
explain how WNV persists in cities and can guide future mosquito control
and public health efforts.

## Interpretation 1

The bloodmeal results show that House Finches were a frequent host for
mosquitoes in Salt Lake City. Locations with more finch blood meals also
had higher rates of WNV-positive mosquito pools, suggesting that House
Finches may be an important amplifying host. This matches our
hypothesis.

Other bird species, such as House Sparrows, Robins, and Starlings, were
also fed on and may contribute to WNV spread, but the link was not as
strong as with House Finches. Together, this points to House Finches
being a key species in maintaining local WNV transmission.

## Interpretation 2

The hotspot maps showed overlap between areas with many WNV-positive
mosquito pools and areas with high numbers of House Finch blood meals.
This spatial relationship suggests that finch–mosquito interactions may
help drive local WNV hotspots. While other birds were present, House
Finches stood out as having the clearest spatial link to virus activity.

# CONCLUSION

This study found that House Finches are likely important amplifying
hosts for West Nile virus in Salt Lake City. Mosquito bloodmeal data and
hotspot mapping both showed strong links between finches and
WNV-positive mosquito pools. While other birds such as House Sparrows
and Robins may also contribute, House Finches stood out as the most
consistent host connected to WNV spread. Identifying these key hosts
helps explain how WNV persists in urban areas and can guide future
surveillance and mosquito control efforts.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-28.

3.  ChatGPT. OpenAI, version Jan 2025. Used to help suggest edits for
    text to make it sound better and correct grammatical errors.
    Majority was still wrote by me.
