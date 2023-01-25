
**Please note that this package (`codaredistlm`, Compositional Data Analysis [CoDA] redistribution linear model) is the new actively maintained package  previously known as [deltacomp](https://github.com/tystan/deltacomp).**

# The `codaredistlm` package


Functions to analyse compositional data and produce predictions (with confidence intervals) for relative increases and decreases in the compositional parts.



## 1. Background

For an outcome variable `Y`, *D* compositional parts (`x_1, ..., x_D`) and *C* covariates (`z_1, ..., z_C`); this package fits the compositional data analysis model (notation inexact):


`Y = b_0 + b_1 ilr_1 + ... + b_{D-1} ilr_{D-1} + a_1 z_1 + ... + a_C z_C + e`

where `ilr_i` are the *D-1* isometric log ratio variables derived from the *D* compositional parts (`x_1, ..., x_D`), `b_0, ...,  b_{D-1}, a_1, ..., a_C` are *D+C* parameters to be estimated and `e ~ N(0, sigma)` is the error. The package then makes predictions in alterations of the time-use variables (the linearly dependent set of compositional parts) based on this model. 


For a starting point to learn about compositional data analysis please see [Aitchison (1982)](https://doi.org/10.1111/j.2517-6161.1982.tb01195.x) or [van den Boogaart and Tolosana-Delgado (2013)](https://link.springer.com/book/10.1007/978-3-642-36809-7). However the articles [Dumuid et al. (2017a)](https://doi.org/10.1177/0962280217710835) and [Dumuid et al. (2017b)](https://doi.org/10.1177/0962280217737805) may be more approachable introductions.


## 2. Reallocation of time-use component options

Please note that the use of 'mean composition' means the geometric mean on the compositional simplex and *not* the arithmetic mean. If these words have little meaning to you, that is no problems as these differently calculated means likely do not differ much in your dataset. 

### 2.1. Option `comparisons = "prop-realloc"` 

Information on outcome prediction with time-use exchange between one part and the remaining compositional parts proportionally (`comparisons = "prop-realloc"` option of the `predict_delta_comps()` function), please see [Dumuid et al. (2017a)](https://doi.org/10.1177/0962280217710835).

### 2.1.1. Example

Suppose you have three (predictor) parts in a day summing to 1 (e.g., a day) to predict an outcome variable. The three parts are `sedentary`, `sleep`  and `activity`. Let's assume the mean sampled composition is:

* `sedentary = 0.5` (i.e., half a day)
* `sleep = 0.3`  (i.e., 30% a day)
* `activity = 0.2` (i.e., 20% a day)

If you wanted to predict the change in the outcome variable from the above mean composition with `delta = +0.05` (5% of the day) is added to `sedentary`, the option `comparisons = "prop-realloc"` reduces the remaining parts by the 5% proportionately based on their mean values, illustrated below:

* `sedentary* = 0.5 + delta = 0.5 + 0.05 = 0.55`
* `sleep* = 0.3 - delta * sleep / (sleep + activity) = 0.3 - 0.05 * 0.3 / (0.3 + 0.2) = 0.3 - 0.03 = 0.27`
* `activity* = 0.2 - delta * activity / (sleep + activity) = 0.2 - 0.05 * 0.2 / (0.3 + 0.2) = 0.2 - 0.02 = 0.18`

Noting that the new compsition: `sedentary* + sleep* + activity* = 0.55 + 0.27 + 0.18 = 1`.

Note for the example above, the option `comparisons = "prop-realloc"` in `predict_delta_comps()` will actually automatically produce separate predictions for a `delta = +0.05` on each of the parts against the remaining parts. i.e., not only the `sedentary* = 0.5 + delta` scenario as illustrated above but also `sleep* = 0.3 + delta` and `activity* = 0.2 + delta` cases.

### 2.2. Option `comparisons = "one-v-one"` 

For information on outcome prediction with time-use exchange between two compositional parts (i.e., the `comparisons = "one-v-one"` option of the `predict_delta_comps()` function), please see
[Dumuid et al. (2017b)](https://doi.org/10.1177/0962280217737805).

### 2.2.1. Example

Similarly to the previous example, suppose you have three (predictor) parts in a day summing to 1 (i.e. a day) to predict an outcome variable. The three parts are `sedentary`, `sleep`  and `activity`. Let's assume the mean sampled composition is:

* `sedentary = 0.5` (i.e., half a day)
* `sleep = 0.3`  (i.e., 30% a day)
* `activity = 0.2` (i.e., 20% a day)

If you wanted to predict the change in the outcome variable from the above mean composition with `delta = +0.05` (5% of the day), the option `comparisons = "one-v-one"` looks at all pairwise exchanges between the parts `(sedentary*, sleep*, activity*)`:

* `(0.5 + 0.05, 0.3 - 0.05, 0.2       )`
* `(0.5 + 0.05, 0.3       , 0.2 - 0.05)`
* `(0.5       , 0.3 + 0.05, 0.2 - 0.05)`
* `(0.5 - 0.05, 0.3 + 0.05, 0.2       )`
* `(0.5 - 0.05, 0.3       , 0.2 + 0.05)`
* `(0.5       , 0.3 - 0.05, 0.2 + 0.05)`



## 3. Datasets in package

Two datasets are supplied with the package:

* `fairclough` and 
* `fat_data`.

The `fairclough` dataset was kindly provided by the authors of [Fairclough et al. (2017)](https://doi.org/10.1186/s12966-017-0521-z). `fat_data` is a randomly generated test dataset that might roughly mimic a real dataset.

## 4. Example usage

```R
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/codaredistlm')
library(codaredistlm)
### see help file to run example
?predict_delta_comps

predict_delta_comps(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by = 5) / (24 * 60),
    comparisons = "prop-realloc",
    alpha = 0.05
)

# OR

predict_delta_comps(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by = 5) / (24 * 60),
    comparisons = "one-v-one",
    alpha = 0.05
)

```


## 5. Output and plotting results

Output is a `data.frame` that can be turned into the plot below using the following code.

```R

pred_df <- 
    predict_delta_comps(
        dataf = fairclough,
        y = "z_bmi",
        comps = c("sleep", "sed", "lpa", "mvpa"),
        covars = c("decimal_age", "sex"),
        # careful deltas greater than 25 min in magnitude induce negative compositions
        # predict_delta_comps() will warn you about this :-)
        deltas =  seq(-20, 20, by = 5) / (24 * 60), 
        comparisons = "prop-realloc", # or try "one-v-one"
        alpha = 0.05
    )

plot_delta_comp(
    pred_df, # provide the returned object from predict_delta_comps()
    # x-axis can be converted from propotion of composition to meaningful units
    comp_total = 24 * 60, # minutes available in the composition
    units_lab = "min" # just a label for plotting
)


```


![](https://github.com/tystan/codaredistlm/blob/master/inst/img/delta_comps2.png)


### 5.1. Prediction for the mean composition

The function `predict_delta_comps()` now outputs the predicted outcome value (with `100 * (1 - alpha)`% confidence interval). This data is printed to the console but also can be extracted from the output of `predict_delta_comps()` as per the below code:

```R

# produces a 1 line data.frame that contains 
#    the (simplex/geometric) mean composition,
#    the "average" covariates (the median of the factor variables in order of the levels are taken as default),
#    the ilr coords of the (simplex/geometric) mean composition, and
#    the predicted outcome value with 100*(1-alpha)% confidence interval
attr(pred_df, "mean_pred")


```


## 6. Release notes

See [/change-notes.md](https://github.com/tystan/codaredistlm/blob/master/change-notes.md).

