# Release summary

This is a resubmission of our first submission of this package in response
to the following requests from Victoria Wimmer. All have been addressed.

If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

>>> Added reference.

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. 
\value{No return value, called for side effects} or similar) Missing Rd-tags:
      CLIP.confint.difflevel.Rd: \value
      distance_density_plot.Rd: \value
      finalize_data.Rd: \value
      get_threshold.Rd: \value
      test_mh.Rd: \value
      threshold_model_plot.Rd: \value
      unique_controls.Rd: \value
      
>>> Added \value to all exported functions.

Please write TRUE and FALSE instead of T and F.
-> man/plot.logistf.profile.Rd:

>>> T changed to TRUE.

Unexecutable code in nncc/vignettes/nncc.Rmd:
   unexpected symbol: ## free to add other modules if necessarymodule load
   
>>> Modified vignete accordingly.

Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package 
directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. 
In your examples/vignettes/tests you can write to tempdir().

>>> Modified function cache.it so it does not include a default write path 
>>> which precludes it from writing at all by default.  
>>> It is not run in examples, vignettes, or tests.

Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1 ...
par(mfrow=c(2,2))            # somewhere after ...
e.g.:
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.

>>> Modified function to use on.exit as requested.

# R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded
