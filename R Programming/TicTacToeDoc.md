---
title: "Tic-Tac-Toe"
author: "Herbert Barrientos"
date: "27 April 2021"
output: html_document
---
### Description

Pogram TicTacToe.R implements the tic-tac-toe game, whereby the computer (identified as ME) plays against a human opponent (identified as THEY). Initially, THEY is prompted if they want to play first.   

The first two moves are crucial for establishing a playing strategy. This applies when the ME plays first, as well as when it plays second. For this reason, these two moves are controlled in sequence.   

Starting on the third move, the process controls who plays next. The winner is eventually reported, just as when a draw is reached. At this point, THEY is prompted is they would like to play again. If so, the game starts anew.   

Strategy:  
1. As stated before, the first two moves are crucial, regardless of whether ME plays first or second.  
2. Starting on the third move, ME will apply the following strategies:  

  + *winGame*: ME's next move results in a win.  
  + *block*: ME blocks a threat from THEY.  
  + *createThreat*: ME's next move creates a simple threat or a fork. This strategy is also used to find out if THEY has posed a threat to ME.
  + *playAnywhere*: when all strategies have failed, ME plays the next move by choosing a cell position at random.  

### Working Environment
```
> sessionInfo()
R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                    LC_TIME=German_Germany.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] data.table_1.12.8

loaded via a namespace (and not attached):
 [1] compiler_3.6.2   rsconnect_0.8.16 htmltools_0.4.0  tools_3.6.2      yaml_2.2.1       Rcpp_1.0.4.6     rmarkdown_2.1    knitr_1.28       xfun_0.14        digest_0.6.25    rlang_0.4.6     
[12] evaluate_0.14   
>
```
