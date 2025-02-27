# if(requireNamespace('spelling', quietly = TRUE))
#   spelling::spell_check_test(vignettes = TRUE, error = FALSE,
#                              skip_on_cran = TRUE)

if(requireNamespace('spelling', quietly = TRUE) &&
   requireNamespace('Rcpp', quietly = TRUE))
  spelling::spell_check_test(vignettes = TRUE, error = FALSE,
                             skip_on_cran = TRUE)
