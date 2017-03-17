# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#hello <- function() {
#  print("Hello, world!")
#}
### test

#taken from John Fox http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
numbers2words <- function(x){

  helper <- function(x){

    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }


  makeNumber <- function(...)
    as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",

            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",

             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",

            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if(length(x) > 1) return(sapply(x, helper))
  helper(x)

}
numbers2words_cap1 <- function(x){

  helper <- function(x){

    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }


  makeNumber <- function(...)
    as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",

            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen",

             "Sixteen", " Seventeen", "Eighteen", "Nineteen")
  names(teens) <- 0:9
  tens <- c("Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty",

            "Ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if(length(x) > 1) return(sapply(x, helper))
  helper(x)

}
numbers2words_cap2 <- function(x){

  helper <- function(x){

    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }


  makeNumber <- function(...)
    as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "One", "Two", "Three", "Four", "Five", "Six", "Seven",

            "Eight", "Nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",

             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",

            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if(length(x) > 1) return(sapply(x, helper))
  helper(x)

}



library(car)

as_word <- function(x){
  recode(x, "1='one';2='two';3='three';4='four';5='five';6='six';7='seven';8='eight';9='nine';10='ten'")

}

#test
as_word_cap <-function(x){
  recode(x, "1='One';2='Two';3='Three';4='Four';5='Five';6='Six';7='Seven';8='Eight';9='Nine';10='Ten'")

}

table_per <- function(x){(table(x))/length(x)*100}

descriptives <- function(x){
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  len <- length(x)

  data.frame(mean, sd, min, max, len)

}

t_paragraph <- function(x,y, measure){
  t <- t.test(x~y)
  labels <- levels(y)
  tsl <- as.vector(t$statistic)
  ts <- round(tsl, digits = 3)
  tpl <- as.vector(t$p.value)
  tp <- round(tpl, digits = 3)

  d_fl <- as.vector(t$parameter)
  d_f <- round(d_fl, digits = 2)
  #d <- as.vector(tapply(x, Sample, descriptives, names=FALSE))
  ml <- as.vector(tapply(x, y, mean))
  m <- round(ml, digits = 2)
  sdl <- as.vector(tapply(x, y, sd))
  sd <- round(sdl, digits = 2)

  if(tp<.05) print(paste0("An independent samples t-test revealed a significant difference in ", measure," between the ", labels[1], " sample, (M = ", m[1],", SD = ", sd[1],"), and the ",labels[2], " sample, (M =", m[2],", SD =", sd[2],"), t(",d_f,") = ",ts,", p = ",tp,"."), quote = FALSE, digits = 2)
  if(tp>.05) print(paste0("An independent samples t-test revealed no difference in ", measure," between the ", labels[1], " sample, (M = ", m[1],", SD = ", sd[1],"), and the ",labels[2], " sample, (M = ", m[2],", SD =", sd[2],"), t(",d_f,") = ",ts,", p = ",tp,"."), quote = FALSE, digits = 2)

}
t_paired_paragraph <- function(x,y,measure){
  t <- t.test(x,y, paired = TRUE)
  labels <- measure
  tsl <- as.vector(t$statistic)
  ts <- round(tsl, digits = 3)
  tpl <- as.vector(t$p.value)
  tp <- round(tpl, digits = 3)


  d_fl <- as.vector(t$parameter)
  d_f <- round(d_fl, digits = 2)
  #d <- as.vector(tapply(x, Sample, descriptives, names=FALSE))
  mxl <- as.vector(mean(x))
  mx <- round(mxl, digits = 2)
  myl <- as.vector(mean(y))
  my <- round(myl, digits = 2)
  sdxl <- as.vector(sd(x))
  sdx <- round(sdxl, digits = 2)
  sdyl <- as.vector(sd(y))
  sdy <- round(sdyl, digits = 2)

  if(tp<.05) print(paste0("A paired samples t-test revealed a significant difference in ", measure," from time one, (M = ", mx,", SD = ", sdx,"), to time two, (M =", my,", SD = ", sdy,"), t(",d_f,") = ",ts,", p = ",tp,"."), quote = FALSE)
  if(tp>.05) print(paste0("A paired samples t-test revealed no difference in ", measure," from time one, (M = ", mx,", SD = ", sdx,"), to time two, (M =", my,", SD = ", sdy,"), t(",d_f,") = ",ts,", p = ",tp,"."), quote = FALSE)
  #cat("An independent samples t-test revealed no difference in initial rating between the", labels[1], "sample (M =", m[1],", SD =", sd[1],") and the ",labels[2], "sample (M =", m[2],", SD =", sd[2],") t(",d_f,") = ",t$statistic,", p = ",t$p.value,".", quote = FALSE)
}



toto <- function(){
  print("I hope this works")
  
}