data(ef_nh)
```

### Operation

`dfoliatR` employs the same processes as the FORTRAN program OUTBREAK, developed by Richard Holmes and Thomas Swetnam. 

The first thing to do is perform the "correction" on the host trees using the `defoliate_trees()` function. This removes the growth signal of the non-host chronology to reduce the influence of climate from the host trees, providing clearer ecological variability that might relate to defoliation events.

Once corrected, `defoliate_trees` will employ runs analyses to identify defoliation periods in each tree. Some parameters regarding the length and severity of growth departure can be changed by the user. The parameter defaults follow those in OUTBREAK. Definitions of the function parameters are provided with `?defoliate_trees`

To run the function, follow the script below, making a new object with the results. This output dataset is termed by `dfoliatR` as a "defol" object to aid the functions in seeing that it represents individual trees as opposed to a composited site-level dataset.

```R
ef_defol <- defoliate_trees(host_tree = ef_h, nonhost_chron = ef_nh, duration_years = 8, list_output = FALSE)
```

We can view the individual time series and the defoliation periods identified by the function by plotting the new `defol` object in a stacked graph:
  ```R
plot_defol(ef_defol, col_defol = "red")
```

The `plot_defol()` function allows for a color change of the defoliation events with the parameter `col_defol`. The default is "black", but any color can be used by using names (e.g., "red"), color codes (e.g., "#8B4500"), or numeric color indicators (e.g., 3).

Basic and informative tree-level statistics regarding the sample data and defoliation events can be viewed by
```R
defol_stats(ef_defol)
```

It is important to note that`dfoliatR` distinguishes between a "defoliation event", recorded on individual trees, and an "outbreak" that synchronously effected a proportion of trees. 

Outbreak periods can be identified with the function `outbreak`. In essence, this is a compositing function that combines all trees provided in the "defol" object to assess the synchrony and scale of defoliation. Should enough trees record defoliation (regardless of the duration), it will be termed an "outbreak". Filter parameters control the percent of trees in defoliation and minimum number of trees required to be considered an outbreak. Short outbreaks can be removed after running `outbreak_stats()`.

```R
ef_comp <- outbreak(ef_defol, filter_perc = 25,filter_min_series = 3)
```

As with the "defol" object, basic statistics and graphics are available for the new "outbreak" object.
```R
outbreak_stats(ef_comp)

plot_outbreak(ef_comp)

###The intervals between outbreak events can be assessed by extracting representative year for each event. `outbreak_stats` provides several options:
#1. The first year of outbreak
#2. the year with the greatest number of defoliated trees
#3. the year with the greatest departure in growth. 

```R
ef_outbrk <- outbreak_stats(ef_comp)

# remove outbreaks lasting less than 10 years (n=1 in this case)
ef_outbrk <- ef_outbrk[ef_outbrk$duration >= 10, ]

# calculate intervals based on the year with the most defoliated trees
ef_interv <- diff(ef_outbrk$peak_outbreak_year)
```
This produces a simple vector of intervals. We can plot it and calculate basic stats
```R
hist(ef_interv, xlab="Intervals (years)", main = "Outbreak intervals at East Fork")

boxplot(ef_interv, horizontal = TRUE, xlab="Intervals (years)", ylab = "East Fork Douglas-fir")

mean(ef_interv)
median(ef_interv)
```

#### Questions, concerns, problems, ideas, want to contribute?
Please contact the author, Chris Guiterman