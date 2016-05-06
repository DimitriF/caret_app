Application to facilitate machine learning training with the caret package

to run it locally:

Download and install R:
http://cran.r-project.org/

Install the dependencies by running this in the R console:
```r
install.packages(
    c('shiny','caret',"plyr","maggritr","readxl","pls")
    )

```

Finally, launch the application by running this line

```r
shiny::runGitHub(repo='caret_app', username='DimitriF',launch.browser=T)
```