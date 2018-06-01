example.R
================
Garrick Aden-Buie
2018-06-01

First, we need some fake data. The following functions make fake data,
with a core data structure of columns named `colname_NN` and ID columns
`id_NN`. The data are randomly generated as integers, doubles,
characters and factors, and the IDs are drawn from integer or character
labels.

A single big tibble is created and then copied to a new tibble where it
is “corrupted” in a few rows and
columns.

``` r
make_char   <- function(iter, len) sapply(1:iter, function(x) paste0(sample(letters, len, replace = TRUE), collapse = ""))
make_factor <- function(iter, len) sapply(1:iter, function(x) factor(sample(letters[1:len], 1), levels = letters[1:len]))

make_core_fake <- function(n, rows = 100) {
  x <- list()
  for (i in 1:n) {
    type <- sample(c("int", "dbl", "char", "factor"), 1)
    x[[sprintf("colname_%02d", i)]] <- switch(
      type,
      int    = sample(-50:50, rows, replace = TRUE),
      dbl    = runif(rows, 0, 1) * 10^(sample(1:5, 1)),
      char   = make_char(rows, 6),
      factor = make_factor(rows, 10)
    )
  }
  tibble::as_tibble(x)
}

factorize <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))[-1]
  all_fct <- div[x %% div == 0L]
  factors <- c()
  flag <- TRUE
  while(flag) {
    this <- all_fct[x %% all_fct == 0L]
    if (!length(this)) {
      flag <- FALSE
    } else {
      factors <- c(factors, this[1])
      x <- x / this[1]
    }
  }
  return(factors)
}

add_ids <- function(df, n_ids = NULL) {
  x <- list()
  rows <- nrow(df)
  n <- if (is.null(n_ids)) factorize(rows) else n_ids
  types <- sample(c("int", "char"), length(n), replace = TRUE)
  for (i in 1:length(n)) {
    x[[sprintf("id_%02d", i)]] <- switch(
      types[i],
      int = sample(1:rows, n[i]),
      char = make_char(n[i], 6)
    )
  }
  df_id <- if (is.null(n_ids)) {
    expand.grid(x)
  } else {
    x <- lapply(x, function(k) rep(k, ceiling(rows/length(k)))[1:rows])
  }
  x <- dplyr::bind_cols(df_id, df)
  tibble::as_tibble(x)
}

corrupt_values <- function(df, ..., n_rows = nrow(df)/5) {
  vars <- rlang::enexprs(...)
  vars <- tidyselect::vars_select(names(df), !!!vars)
  for (var in vars) {
    df[[var]][sample(1:nrow(df), n_rows)] <- sample(df[[var]], n_rows)
  }
  df
}
```

Here is the fake data with 10 data columns, 2 ID columns and 10^5 rows.

``` r
x <- make_core_fake(10, 10^6) %>%
  add_ids(n_ids = c(10^3, 30))
y <- corrupt_values(x, dplyr::contains("colname"), n_rows = 20)
```

Here I additionally remove some columns from each side and change data
types

``` r
x <- x[, sample(-3:-13, 1)]
y <- y[, sample(-3:-13, 2)]
y_ints <- sapply(y, is.integer)
y_ints[1:2] <- rep(FALSE, 2)
if (any(y_ints)) {
  y[[which(y_ints)[1]]] <- as.character(y[[which(y_ints)[1]]])
}
```

And scramble the order but add grouping to both … this doesn’t work yet.
It starts to get a whole lot more complicated when the number of rows
differ.

``` r
# x <- group_by(x, id_01, id_02) %>% {.[sample(1:nrow(.), nrow(.)), ]}
# y <- group_by(y, id_01, id_02) %>% {.[sample(1:nrow(.), ceiling(nrow(.) * 0.95)), ]}

tibble:::print.tbl_df(x)
```

    # # A tibble: 1,000,000 x 11
    #    id_01   id_02 colname_01 colname_03 colname_04 colname_05 colname_06
    #    <chr>   <int> <chr>           <int>      <int>      <int> <fct>     
    #  1 keklii 271982 pytodq             23         28         -2 h         
    #  2 ccnklh 617614 bjdvvi              7        -20        -41 b         
    #  3 kpehmu 571297 lmygeg             35        -29         -2 d         
    #  4 tugwke 980241 rfimiz            -26        -10        -37 d         
    #  5 kvbzlt 807817 almjtn            -39         15         17 f         
    #  6 hafeln 475980 mchiko            -15         40        -46 g         
    #  7 fxtxue 717063 psmdor             28         13         38 a         
    #  8 ikbnlz 820462 xbmkju              9        -20          3 f         
    #  9 equhbs 166083 amotdn             37          2         12 c         
    # 10 qxqpaa 546228 jxbxjm              8         -6        -11 b         
    # # ... with 999,990 more rows, and 4 more variables: colname_07 <chr>,
    # #   colname_08 <fct>, colname_09 <int>, colname_10 <fct>

``` r
tibble:::print.tbl_df(y)
```

    # # A tibble: 1,000,000 x 10
    #    id_01   id_02 colname_01 colname_02 colname_03 colname_04 colname_06
    #    <chr>   <int> <chr>      <chr>           <int>      <int> <fct>     
    #  1 keklii 271982 pytodq     36                 23         28 h         
    #  2 ccnklh 617614 bjdvvi     -3                  7        -20 b         
    #  3 kpehmu 571297 lmygeg     -6                 35        -29 d         
    #  4 tugwke 980241 rfimiz     -35               -26        -10 d         
    #  5 kvbzlt 807817 almjtn     -43               -39         15 f         
    #  6 hafeln 475980 mchiko     49                -15         40 g         
    #  7 fxtxue 717063 psmdor     -35                28         13 a         
    #  8 ikbnlz 820462 xbmkju     36                  9        -20 f         
    #  9 equhbs 166083 amotdn     49                 37          2 c         
    # 10 qxqpaa 546228 jxbxjm     -32                 8         -6 b         
    # # ... with 999,990 more rows, and 3 more variables: colname_07 <chr>,
    # #   colname_08 <fct>, colname_09 <int>

We can compare the two data frames with `tidy_diff()`.

``` r
library(grkmisc)
system.time(
  z <- tidy_diff(x, y)
)
```

    #    user  system elapsed 
    #   0.177   0.012   0.189

This creates a `tidy_diff` object with `print`, `summary` and `plot`
methods

``` r
> print(z)
```

    Showing differences in first 5 columns...
    
    # A tibble: 2 x 22
      variable   set   `89811` `236178` `274555` `314342` `315263` `335394`
      <chr>      <chr> <chr>   <chr>    <chr>    <chr>    <chr>    <chr>   
    1 colname_01 x     ivrxwx  zvvsus   uytdfq   msqpba   vvyacr   dtbbcu  
    2 colname_01 y     bbwqwp  zvwbmq   bhhfim   vgxncy   bxlyjj   kqhyua  
    # ... with 14 more variables: `377293` <chr>, `412980` <chr>,
    #   `449950` <chr>, `548360` <chr>, `554011` <chr>, `612600` <chr>,
    #   `616969` <chr>, `642041` <chr>, `687075` <chr>, `737504` <chr>,
    #   `841880` <chr>, `889129` <chr>, `910756` <chr>, `945483` <chr>
    
    # A tibble: 2 x 22
      variable   set   `10695` `69367` `104149` `134409` `162126` `197968`
      <chr>      <chr>   <int>   <int>    <int>    <int>    <int>    <int>
    1 colname_03 x          -9      32        8       20      -50       45
    2 colname_03 y         -15     -11      -19      -37       42       22
    # ... with 14 more variables: `198954` <int>, `253607` <int>,
    #   `270216` <int>, `312665` <int>, `335488` <int>, `432529` <int>,
    #   `580123` <int>, `634929` <int>, `663622` <int>, `748165` <int>,
    #   `771764` <int>, `831684` <int>, `915056` <int>, `987873` <int>
    
    # A tibble: 2 x 22
      variable   set   `25536` `34910` `51873` `119893` `156209` `283177`
      <chr>      <chr>   <int>   <int>   <int>    <int>    <int>    <int>
    1 colname_04 x          23      11     -43      -17      -33      -26
    2 colname_04 y          38      34      42       46       49       39
    # ... with 14 more variables: `339947` <int>, `389172` <int>,
    #   `483641` <int>, `574453` <int>, `673029` <int>, `693106` <int>,
    #   `703862` <int>, `730203` <int>, `771930` <int>, `877923` <int>,
    #   `904323` <int>, `931220` <int>, `971278` <int>, `978464` <int>
    
    # A tibble: 2 x 22
      variable   set   `88792` `123694` `174992` `177151` `200950` `206452`
      <chr>      <chr> <chr>   <chr>    <chr>    <chr>    <chr>    <chr>   
    1 colname_06 x     e       a        j        b        i        j       
    2 colname_06 y     j       e        e        h        g        f       
    # ... with 14 more variables: `225852` <chr>, `383856` <chr>,
    #   `488825` <chr>, `489499` <chr>, `576752` <chr>, `588949` <chr>,
    #   `660933` <chr>, `668410` <chr>, `687526` <chr>, `740137` <chr>,
    #   `743152` <chr>, `779694` <chr>, `900570` <chr>, `958053` <chr>
    
    # A tibble: 2 x 22
      variable   set   `25597` `81019` `86588` `120054` `181468` `213365`
      <chr>      <chr> <chr>   <chr>   <chr>   <chr>    <chr>    <chr>   
    1 colname_07 x     atyrzw  suutek  xebwrt  zpixub   pjcdsb   qupkgf  
    2 colname_07 y     rldwre  glzvpk  opvmdl  irxrox   rxnqml   hppwoj  
    # ... with 14 more variables: `221630` <chr>, `271993` <chr>,
    #   `338873` <chr>, `353847` <chr>, `419947` <chr>, `458055` <chr>,
    #   `514348` <chr>, `744611` <chr>, `790790` <chr>, `840215` <chr>,
    #   `847438` <chr>, `850332` <chr>, `862023` <chr>, `870284` <chr>
    
    ... with differences in 2 more columns: `colname_08`, `colname_09`

``` r
> summary(z)
```

``` 
── Comparison Summary ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
● Dimensions
    set      rows  cols
  1 x     1000000    11
  2 y     1000000    10

● 'x' has 2 unique columns: `colname_05`, `colname_10`
● 'y' has 1 unique column: `colname_02`
● There are 139 differing values across 139 rows:
     variable   state    miss_count `misses (row id)`                       
   1 colname_01 diff             20 89811, 236178, 274555, 314342, 315263, …
   2 colname_03 diff             20 10695, 69367, 104149, 134409, 162126, 1…
   3 colname_04 diff             20 25536, 34910, 51873, 119893, 156209, 28…
   4 colname_06 diff             20 88792, 123694, 174992, 177151, 200950, …
   5 colname_07 diff             20 25597, 81019, 86588, 120054, 181468, 21…
   6 colname_08 diff             19 47149, 52937, 72864, 202418, 259684, 44…
   7 colname_09 diff             20 11990, 146848, 150614, 168569, 224131, …
   8 id_01      same              0 ""                                      
   9 id_02      same              0 ""                                      
  10 colname_02 unique_x         NA ""                                      
  11 colname_05 unique_y         NA ""                                      
  12 colname_10 unique_y         NA ""                                      
```

``` r
> plot(z)
```

![](example_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

You can also pull out a list of comparisons by differing columns by
subsetting the to the `.$tidy` element of the tidy diff object.

``` r
> z$tidy[1]
```

    $colname_01
    # A tibble: 20 x 6
       variable   value.x value.y miss_index id_01   id_02
       <chr>      <chr>   <chr>        <int> <chr>   <int>
     1 colname_01 ivrxwx  bbwqwp       89811 vityfv 334304
     2 colname_01 zvvsus  zvwbmq      236178 aysevx 581581
     3 colname_01 uytdfq  bhhfim      274555 jtztxn 467022
     4 colname_01 msqpba  vgxncy      314342 qzlusg 617614
     5 colname_01 vvyacr  bxlyjj      315263 hibspe 927735
     6 colname_01 dtbbcu  kqhyua      335394 kjmqdp 245992
     7 colname_01 husacx  spwbns      377293 zoctir 264199
     8 colname_01 fcyazo  fxopnb      412980 acnauu 245644
     9 colname_01 jieklg  qwomat      449950 prrybx 546228
    10 colname_01 gafefm  ysgegq      548360 vqhijw 668317
    11 colname_01 yehfft  xkkhgf      554011 rzjipn 271982
    12 colname_01 dikmiu  mmvavb      612600 wsqafp 245644
    13 colname_01 mevppw  blquqa      616969 pndylb 630522
    14 colname_01 opfvky  wxjgwk      642041 ppwhcr  70742
    15 colname_01 jpzjta  uefyzj      687075 wlradh 582951
    16 colname_01 kdxkqc  jodijw      737504 rqjezx 522130
    17 colname_01 udaexy  rljkfi      841880 ziyjjm 668317
    18 colname_01 aftvey  raxpgk      889129 cyglvs 630522
    19 colname_01 bthabj  qhltpq      910756 ypbwac 915812
    20 colname_01 qlrksf  svojna      945483 fqvxbt 571297

The tidy diff object also includes a tidy dataframe with “diff”, “same”
or “unique” column values.

``` r
> z$diff
```

    # A tibble: 12 x 4
       variable   state    miss_count misses    
       <chr>      <chr>         <int> <list>    
     1 colname_01 diff             20 <int [20]>
     2 colname_03 diff             20 <int [20]>
     3 colname_04 diff             20 <int [20]>
     4 colname_06 diff             20 <int [20]>
     5 colname_07 diff             20 <int [20]>
     6 colname_08 diff             19 <int [19]>
     7 colname_09 diff             20 <int [20]>
     8 id_01      same              0 <int [0]> 
     9 id_02      same              0 <int [0]> 
    10 colname_02 unique_x         NA <int [0]> 
    11 colname_05 unique_y         NA <int [0]> 
    12 colname_10 unique_y         NA <int [0]> 

In terms of size

``` r
> pryr::object_size(x)
```

    168 MB

``` r
> pryr::object_size(y)
```

    168 MB

``` r
> pryr::object_size(z)
```

    31.2 kB
