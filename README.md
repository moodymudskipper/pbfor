
<!-- README.md is generated from README.Rmd. Please edit that file -->
pbfor
=====

Install with :

``` r
devtools::install_github("moodymudskipper/pbfor")
```

This package proposes two solutions to use progress bars conveniently with the standard `for` syntax, both are using the great package [*progress*](https://cran.r-project.org/package=progress) from Gábor Csárdi and Rich FitzJohn.

-   with `pb_for()` : override temporarily or locally the `for` function to wrap around `base::for` and support progress bars.
-   with `for<-` : wrap around `base::for` using syntax `pb -> for(it in seq) {exp}` where `pb` .

Both solutions behave as standard for calls :

-   The values changed at the previous iteration are available
-   on error the modified variables will have the value they had just before the error

------------------------------------------------------------------------

Usage
=====

``` r
library(pbfor)
#> Loading required package: progress
```

### Using `pb_for()`

By default `pb_for()` will override the `for` function for one run only.

``` r
pb_for()
for (i in 1:10) {
  # DO SOMETHING
  Sys.sleep(0.5)
}
```

Using parameters from `progress::progress_bar$new()` :

``` r
pb_for(format = "Working hard: [:bar] :percent :elapsed", 
       callback = function(x) message("Were'd done!"))
for (i in 1:10) {
  # DO SOMETHING
  Sys.sleep(0.5)
}
#> Were'd done!
```

### Using `for<-`

The only restriction compared to a standard `for` call is that the first argument must exist and can't be `NULL`.

``` r
i <- NA 
progress_bar$new() -> for (i in 1:10) {
  # DO SOMETHING
  Sys.sleep(0.5)
}
```

We can define a custom progress bar, and maybe define it conveniently in an initialisation script or in one's R profile.

``` r
pb <- progress_bar$new(format = "Working hard: [:bar] :percent :elapsed", 
       callback = function(x) ("Were'd done!"))
pb  -> for (i in 1:10) {
  # DO SOMETHING
  Sys.sleep(0.5)
}
```

For nested progress bars we can use the following trick :

``` r
pbi <- progress_bar$new(format = "i: [:bar] :percent\n\n")
pbj <- progress_bar$new(format = "j: [:bar] :percent  ")
i <- NA
j <- NA
pbi  -> for (i in 1:10) {
  pbj  -> for (j in 1:10) {
    # DO SOMETHING
    Sys.sleep(0.1)
  }
}
```

Note that bars are cleared by default but in that case the `i` bar will be shown at each iteration.

------------------------------------------------------------------------

how they work
=============

### `pb_for()`

`pb_for()` creates a `for` function object in its parent environment, then the new `for` :

-   sets up a progress bar
-   modifies the loop content
-   adds a `` `*pb*`$tick() `` at the end of the loop content expression
-   feeds it back to `` base::`for` `` in a clean environment
-   assigns on exit all modified or created variables to the parent environment.
-   removes itself if `once` is `TRUE` (the default)

It's generally sensitive to override an operator, but it cleans after itself and won't affect the global environment if used in a function so I think it's safe enough to use.

### `for<-`

This approach :

-   doesn't override `for`
-   allows the use of progress bar templates
-   has an arguably more intuitive api

It has a few drawbacks however:

-   its first argument must exist, which is the case for all assignment functions (`fun<-`).
-   it does some memory magic to find the name of its first argument as it's [not easily done with assignment functions](https://stackoverflow.com/questions/51246432/get-name-of-x-when-defining-operator), this might have a performance cost, and I'm not 100% sure about the robustness
-   we need the package *pryr*

What it does :

-   find the name of the first argument, using a helper function
-   clone the progress bar input
-   edit it to account for the number of iterations of the loop (the length of the second argument of `for<-`

After this it's similar to what is described for `pb_for()` in the section above.
