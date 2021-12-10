
# The necessary functions for the main R code: CS_indivPaper_code.R

k<-1
l<-1

# Function to find index where "Brand" is present
find_same_brands <- function(k, l) {
  for (index in 1:length(data_TV_flat[[k]][["featuresMap"]])) {
    if (str_contains(names(data_TV_flat[[k]][["featuresMap"]][index]), "Brand", ignore.case = TRUE)) {
      brand1 <- data_TV_flat[[k]][["featuresMap"]][index]
      break
    } 
  }
  
  for (index in  1:length(data_TV_flat[[l]][["featuresMap"]])) {
    if (str_contains(names(data_TV_flat[[l]][["featuresMap"]][index]), "Brand", ignore.case = TRUE)) {
      brand2 <- data_TV_flat[[l]][["featuresMap"]][index]
      break
    } 
  }
  
  if (!exists("brand1") || !exists("brand2")) {
    return(FALSE)
  } else if (tolower(brand1) == tolower(brand2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function calculating number of non-overlaping tokens
qGramDistance <- function(qgram1, qgram2) {
  count = 0
  for (q in 1:length(qgram1)) {
    if (as.logical(qgram1[q] == qgram2[q])) {
      next
    } else {
      x <- abs(qgram1[q] - qgram2[q])
      count = count + x
    }
  }
  return(as.numeric(count))
}

i<-1
j<-1
# Function comparing the feature keys of candidate products
compare_keys <- function(i,j) {
  qgram_matrix <- qgrams(names(data_TV_flat[[k]][["featuresMap"]][i]), names(data_TV_flat[[l]][["featuresMap"]][j]), q=3)
  if (ncol(qgram_matrix) == 0) {
    qgram_matrix <- qgrams(names(data_TV_flat[[k]][["featuresMap"]][i]), names(data_TV_flat[[l]][["featuresMap"]][j]), q=2)
    if (ncol(qgram_matrix) == 0) {
      qgram_matrix <- qgrams(names(data_TV_flat[[k]][["featuresMap"]][i]), names(data_TV_flat[[l]][["featuresMap"]][j]), q=1)
      if (ncol(qgram_matrix) == 0) {
        return(0)
      }
    }
    keySim <- (sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]) - qGramDistance(qgram_matrix[1,], qgram_matrix[2,]))/(sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]))
  } else {
    keySim <- (sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]) - qGramDistance(qgram_matrix[1,], qgram_matrix[2,]))/(sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]))
  }
  
  return(as.numeric(keySim))
  
}


# Function comparing the feature values of candidate products
compare_values <- function(i,j) {
  qgram_matrix <- qgrams(data_TV_flat[[k]][["featuresMap"]][i], data_TV_flat[[l]][["featuresMap"]][j], q=3)
  if (ncol(qgram_matrix) == 0) {
    qgram_matrix <- qgrams(data_TV_flat[[k]][["featuresMap"]][i], data_TV_flat[[l]][["featuresMap"]][j], q=2)
    if (ncol(qgram_matrix) == 0) {
      qgram_matrix <- qgrams(data_TV_flat[[k]][["featuresMap"]][i], data_TV_flat[[l]][["featuresMap"]][j], q=1)
    }
    
    valueSim <- (sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]) - qGramDistance(qgram_matrix[1,], qgram_matrix[2,]))/(sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]))
  } else {
    valueSim <- (sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]) - qGramDistance(qgram_matrix[1,], qgram_matrix[2,]))/(sum(qgram_matrix[1,]) + sum(qgram_matrix[2,]))
  }
  
  return(as.numeric(valueSim))
  
}


# Functions extracting the model words from the values in the non-matching keys
exMW_k <- function(nmk_k) {
  MW_k <- list()
  for (key in nmk_k) {
    mw <- str_extract_all(data_TV_flat[[k]][["featuresMap"]][[key]],
                          "([a-zA-Z0-9]*(([0-9\".-:]+[^0-9-]+)|([^0-9-]+[0-9\".-:]+))[a-zA-Z0-9\"]*)|[0-9]+[a-zA-Z]+") 
    if (length(mw[[1]])==0) {
      next
    }
    
    for (j in 1:length(mw[[1]])) {
      MW_k <- union(MW_k, mw[[1]][[j]])
    }
    
  }
  return(MW_k)
   
}

exMW_l <- function(nmk_l) {
  MW_l <- list()
  for (key in nmk_l) {
    mw <- str_extract_all(data_TV_flat[[l]][["featuresMap"]][[key]],
                          "([a-zA-Z0-9]*(([0-9\".-:]+[^0-9-]+)|([^0-9-]+[0-9\".-:]+))[a-zA-Z0-9\"]*)|[0-9]+[a-zA-Z]+") 
    if (length(mw[[1]])==0) {
      next
    }
    
    for (j in 1:length(mw[[1]])) {
      MW_l <- union(MW_l, mw[[1]][[j]])
    }
    
  }
  return(MW_l)
  
}


# Function calculating percentage of matching model words

mw <- function(MW_k, MW_l) {
  matches <- 0
  count <- 0
  if (is_empty(MW_k) || is_empty(MW_l)) {
    return(0)
  }
  for (mw_k in MW_k) {
    for (mw_l in MW_l) {
      if (mw_k == mw_l) {
        matches = matches+1
      }
      count <- count+1
      
    }
    
  }
  frac <- matches/count
  return(frac)
}


# Function calculating similarity in the titles

TMWM <- function(k, l) {
  MW_k <- list()
  MW_l <- list()
  
  mw_k <- str_extract_all(data_TV_flat[[k]][["title"]],
                        "([a-zA-Z0-9]*(([0-9\".-:]+[^0-9-]+)|([^0-9-]+[0-9\".-:]+))[a-zA-Z0-9\"]*)|[0-9]+[a-zA-Z]+")  
  
  
  for (j in 1:length(mw_k[[1]])) {
    MW_k <- union(MW_k, mw_k[[1]][[j]])
  }
  
  mw_l <- str_extract_all(data_TV_flat[[l]][["title"]],
                          "([a-zA-Z0-9]*(([0-9\".-:]+[^0-9-]+)|([^0-9-]+[0-9\".-:]+))[a-zA-Z0-9\"]*)|[0-9]+[a-zA-Z]+")  
  
  
  for (j in 1:length(mw_l[[1]])) {
    MW_l <- union(MW_l, mw_l[[1]][[j]])
  }
  
  MW_all <- union(MW_k, MW_l)
  bin_matr <- matrix(0, nrow = 2, ncol = length(MW_all))
  i=1
  for (mw in MW_all) {
    if (mw %in% MW_k) {
      bin_matr[1, i] <- 1
    }
    
    if (mw %in% MW_l) {
      bin_matr[2, i] <- 1
    }
    i=i+1
  }
  
  
  return(cosine(bin_matr[1,], bin_matr[2,]))
}




