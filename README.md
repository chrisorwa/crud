# crud
Functions to weed the crud out of your data

Package: crud
Type: Package
Title: Data cleaning utilities
Version: 0.1.0
Author: Henk Harmsen
Maintainer: Henk Harmsen <henk@carbonmetrics.com>
## Description: Sturgeon's law says that "Ninety percent of everthing is crud".
    This package helps you to decrud this ninety percent of your data. 
    Remove crud: duplicate records, empty columns, empty rows, variables without variance.
    Clean crud: header names, excessive whitespace, invalid strings.
    Flag crud: missing values, outliers, mixed characters and numbers, excessive number of factor levels, records containing (sub)totals. 
    Deal with crud: negative filter, unpack lists, summaries, modus.
   
License: What license is it under?
Encoding: UTF-8
LazyData: true
Depends:
    *  bit64,
    *  ineq,
    *  quanteda
    *  stringr
    *  stringi
    *  lubridate
    *  magrittr
    *  ggplot2
    *  gganimate
    *  foreach
    *  doParallel
    *  scales
    *  viridis
    *  parallel
    *  knitr
    *  data.table
    *  broom
    *  simputation
    * forcats
    * gridExtra
    * HDoutliers 
    * vcd
    * vcdExtra 
    * ranger 
    * xgboost
    * Boruta
    * rpart
    * rpart.plot
    * RoxygenNote: 5.0.1
