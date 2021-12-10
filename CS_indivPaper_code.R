
# Author: Jackson Tabe
# Date: December 10, 2021
# Title paper: A Scalable LSH-Based Product Duplicate Detection Method
# Course: Computer Science for Business Analytics - FEM21037
# Professor: Flavius Frasincar
# Institution: Erasmus School of Economics (Erasmus University Rotterdam)



# START AND LOADING NECESSARY PACKAGES ------------------------------------


rm(list = ls())
library(rjson)
library(stringr)
library(sjmisc)
library(psych)
library(stringdist)
library(qvalue)
#library(jaccard)
library(lsa)
library(foreach)
library(zeallot)

# Import the necessary functions
functions_directory <- 'C:/Users/jackson/Downloads/FunctionsComputerScienceAssignment.R'
source(functions_directory)

# IMPORTING AND MANIPULATING THE DATA -------------------------------------


# Import the data and make each element a list of length 1

input_file <- "C:/Users/jackson/Downloads/TVs-all-merged.json"
data_TV <- fromJSON(file = input_file)

# Creating new list with every product in separate sublist
i=1
data_TV_flat = list()
for (sublist in data_TV) {
  for (item in sublist) {
    data_TV_flat[[i]] = item
    i=i+1
  }
  
}

#Convert the part of the titles which contain '-Inch', 'inch', '\"' to \" and Hertz, Hz, etc. to 'hz'
#Convert the titles and the brand names in the featuresmap to lower case
for (i in 1:length(data_TV_flat)) {
  data_TV_flat[[i]][["title"]] =  tolower(data_TV_flat[[i]][["title"]])
  
  if (str_contains(data_TV_flat[[i]][["title"]], c(" inches", "inches", "-inch", " inch", "inch"), logic = "or")) {
    data_TV_flat[[i]][["title"]] = str_replace_all(data_TV_flat[[i]][["title"]], c(" inches|inches|-inch| inch|inch"), "\"")
    
  } 
  
  if (str_contains(data_TV_flat[[i]][["title"]], c(" hertz", " hz", "hertz", "hz", "-hz"), logic = "or")) {
    data_TV_flat[[i]][["title"]] = str_replace_all(data_TV_flat[[i]][["title"]], c(" hertz| hz|hertz|hz|-hz"), "hz")
  }
  
  if (!is.null(data_TV_flat[[i]][["featuresMap"]][["Brand"]]) || !is.null(data_TV_flat[[i]][["featuresMap"]][["Brand Name"]])) {
    if (!is.null(data_TV_flat[[i]][["featuresMap"]][["Brand"]])) {
      data_TV_flat[[i]][["featuresMap"]][["Brand"]] <- tolower(data_TV_flat[[i]][["featuresMap"]][["Brand"]])
    } else {
      data_TV_flat[[i]][["featuresMap"]][["Brand Name"]] <- tolower(data_TV_flat[[i]][["featuresMap"]][["Brand Name"]])
    }
  }
  
}

# Removing unnecessary characters in the key-value pairs and converting all pairs to lower case

for (i in 1:length(data_TV_flat)) { 
  for (j in 1:length(data_TV_flat[[i]][["featuresMap"]])) {
    data_TV_flat[[i]][["featuresMap"]][j] <- tolower(data_TV_flat[[i]][["featuresMap"]][j])
    data_TV_flat[[i]][["featuresMap"]][j] <- str_remove_all(data_TV_flat[[i]][["featuresMap"]][j], "[^0-9a-zA-Z.]") 
    
    if (str_contains(data_TV_flat[[i]][["featuresMap"]][j], c(" inches", "inches", "-inch", " inch", "inch"), logic = "or")) {
      data_TV_flat[[i]][["featuresMap"]][j] = str_replace_all(data_TV_flat[[i]][["featuresMap"]][j], c(" inches|inches|-inch| inch|inch"), "\"")
      
    } 
    
    if (str_contains(data_TV_flat[[i]][["featuresMap"]][j], c(" hertz", " hz", "hertz", "hz", "-hz"), logic = "or")) {
      data_TV_flat[[i]][["featuresMap"]][j] = str_replace_all(data_TV_flat[[i]][["featuresMap"]][j], c(" hertz| hz|hertz|hz|-hz"), "hz")
    }
    
  }
  names(data_TV_flat[[i]][["featuresMap"]]) <- tolower(names(data_TV_flat[[i]][["featuresMap"]]))
  names(data_TV_flat[[i]][["featuresMap"]]) <- str_remove_all(names(data_TV_flat[[i]][["featuresMap"]]), "[^0-9a-zA-Z]")
  
}






# CONSTRUCTING THE MODEL WORDS --------------------------------------------

mwConstruction <- function(data_TV_flat) {

# Extracting model words from the product titles and brand names from the featuresMap
MW = list()
for (i in 1:length(data_TV_flat)) {
#  mw <- str_extract_all(data_TV_flat[[i]][["title"]],
 #                       "[a-zA-Z]+[-.\":]*[0-9]+[a-zA-Z0-9]*|[0-9]+[-.\":]*[a-zA-Z]+[a-zA-Z0-9]*|[a-zA-Z0-9]+[-\":]+[a-zA-Z0-9]*|[0-9]+[.]+[0-9]*[-\":]*")
   
#  mw <- str_extract_all(data_TV_flat[[i]][["title"]],
#                       "[a-zA-Z0-9]*(([0-9]+[:punct:]+)|([:punct:]+[0-9\"]+))[a-zA-Z0-9]*")  
  
 mw <- str_extract_all(data_TV_flat[[i]][["title"]],
                 "([a-zA-Z0-9]*(([0-9\".-:]+[^0-9-]+)|([^0-9-]+[0-9\".-:]+))[a-zA-Z0-9\"]*)|[0-9]+[a-zA-Z]+")  
  
  
  for (j in 1:length(mw[[1]])) {
    MW <- union(MW, mw[[1]][[j]])
  }
  
  if (!is.null(data_TV_flat[[i]][["featuresMap"]][["Brand"]]) || !is.null(data_TV_flat[[i]][["featuresMap"]][["Brand Name"]])) {
    if (!is.null(data_TV_flat[[i]][["featuresMap"]][["Brand"]])) {
      brand <- data_TV_flat[[i]][["featuresMap"]][["Brand"]]
    } else {
      brand <- data_TV_flat[[i]][["featuresMap"]][["Brand Name"]]
    }
  } else {
    next
  }
  
  MW <- union(MW, brand)
}

return(MW)
}



# THE LSH ALGORITHM -------------------------------------------------------

LSH_alg <- function(MW, b, r, strap, data_TV_flat) {
# CONSTRUCTING THE BINARY PRODUCT VECTORS ---------------------------------


i=1
j=1
binary_matrix <- matrix(nrow = length(MW), ncol = length(data_TV_flat))
for (product in data_TV_flat) {
  for (mw in MW) {
    if (grepl("^[0-9]+$", mw)) { # See if model word only contains numeric values
      mw <- paste(" ", mw, " ", sep = "")
    }
    
    if (grepl(mw, product[["title"]], fixed = TRUE)){
      binary_matrix[i, j] = 1
    } else {
      binary_matrix[i, j] = 0
    }
    i = i+1
  }
  j = j+1
  i = 1
}
rownames(binary_matrix) <- MW



# MINHASHING --------------------------------------------------------------


#set.seed(123)
n <- 500
signature_matrix <- matrix(nrow = n, ncol = length(data_TV_flat))
permutations <- matrix(nrow = length(MW), ncol = n)
non_zero_indices <- apply(binary_matrix, MARGIN = 2, function(x) which (x != 0))
for (i in 1:n) {
  perm <- sample(length(MW))
  permutations[, i] <- perm
  for (j in 1:length(data_TV_flat)) {
    signature_matrix[i, j] <- min(perm[non_zero_indices[[j]]])
    
  }
  
}



# LOCALITY SENSITIVE HASHING ----------------------------------------------

# Construct the binary candidate duplicate matrix
candidate_matrix <- matrix(0, nrow = length(data_TV_flat), ncol = length(data_TV_flat))

for (i in 1:b) {
  bands <- signature_matrix[(1+(i-1)*r):(r*i),]
  for (k in 1:(length(data_TV_flat)-1)){
    for (l in (k+1):length(data_TV_flat)) {
      if (is.null(nrow(bands))) {
        if (all(bands[k] == bands[l])) {
          candidate_matrix[k,l] <- 1 
        }
      } else {
        if (all(bands[,k] == bands[,l])) {
          candidate_matrix[k,l] <- 1 
        }
      }
    }
  }
}



# EVALUATION MEASURES AFTER LSH -----------------------------------------------------


# Pair quality
n_duplicates_LSH = 0
n_comparisons_LSH = 0

for (i in 1:nrow(candidate_matrix)) {
  for (j in 1:ncol(candidate_matrix)) {
    if (candidate_matrix[i,j]==1 ) {
      n_comparisons_LSH = n_comparisons_LSH+1
      if (data_TV_flat[[i]][["modelID"]] == data_TV_flat[[j]][["modelID"]]) {
        n_duplicates_LSH = n_duplicates_LSH+1
      }
    }
  }
}


pair_quality_LSH = n_duplicates_LSH/n_comparisons_LSH

# Pair completeness
total_duplicates = 0
for (i in 1:(length(data_TV_flat)-1)) {
  for (j in (i+1):length(data_TV_flat)) {
    if (data_TV_flat[[i]][["modelID"]] == data_TV_flat[[j]][["modelID"]]) {
      total_duplicates = total_duplicates+1
    }
  }
  
}

pair_completeness_LSH <- n_duplicates_LSH/total_duplicates

# F_1* - measure
F1_star_LSH <- harmonic.mean(c(pair_quality_LSH, pair_completeness_LSH))



total_possible_comparisons <- length(data_TV_flat)*(length(data_TV_flat)-1)/2

string1 <- paste("For b =", b, ", r =", r, "and bootstrap", strap,  "------------------------------------------")
string2 <- paste("number of duplicates =", n_duplicates_LSH)
string3 <- paste("total duplicates =", total_duplicates)
string4 <- paste("number of comparisons =", n_comparisons_LSH)
string5 <- paste("total possible comparisons =", total_possible_comparisons)
string6 <- paste("PQ =", pair_quality_LSH)
string7 <- paste("PC =", pair_completeness_LSH)
string8 <- paste("F1_star =", F1_star_LSH)
string9 <- paste("fraction of comparisons =", n_comparisons_LSH/total_possible_comparisons)
cat(string1, string2, string3, string4, string5, string6, string7, string8, string9, sep = "\n")

output_list <- list(pair_quality_LSH, pair_completeness_LSH, F1_star_LSH, n_comparisons_LSH/total_possible_comparisons, candidate_matrix)


return(output_list)


}



# THE DUPLICATE DETECTION METHOD ------------------------------------------

MSM <- function(data_TV_flat, gamma, mu, candidate_matrix) {

dissim_matrix <- matrix(nrow = length(data_TV_flat), ncol = length(data_TV_flat))

for (k in 1:(length(data_TV_flat)-1)) {
  for (l in (k+1):length(data_TV_flat)) {
    if (candidate_matrix[k,l] == 0) {
      next
      # Set dissimilarity to infinity if the products have different brand names
    } else if (str_contains(names(data_TV_flat[[k]][["featuresMap"]]), "Brand", ignore.case = TRUE) && 
               str_contains(names(data_TV_flat[[l]][["featuresMap"]]), "Brand", ignore.case = TRUE)) {
      
      if (!find_same_brands(k,l)) {
        dissim_matrix[k,l] = Inf
        next
      } 
    }
    
    # Dissimilarity calculation for candidate duplicates with (most likely) the same brand
    sim <- 0
    avgSim <- 0
    w <- 0
    m <- 0
    nmk_k <- names(data_TV_flat[[k]][["featuresMap"]])
    nmk_l <- names(data_TV_flat[[l]][["featuresMap"]])
    
    for (i in 1:length(data_TV_flat[[k]][["featuresMap"]])) {
      for (j in 1:length(data_TV_flat[[l]][["featuresMap"]])) {
        keySim <- compare_keys(i,j)
        if (keySim > gamma) {
          valueSim <- compare_values(i,j)
          weight <- keySim
          sim <- sim + weight*valueSim
          m <- m + 1
          w <- w + weight
          nmk_k <- nmk_k[ - which(names(data_TV_flat[[k]][["featuresMap"]][i]) == nmk_k)]
          nmk_l <- nmk_l[ - which(names(data_TV_flat[[l]][["featuresMap"]][j]) == nmk_l)]
        } 
      }
    }
    
    if (w>0) {
      avgSim <- sim/w
    }
    
    if (is_empty(nmk_k) || is_empty(nmk_l)) {
      mwPerc <- 0
    } else {
      mwPerc <- mw(exMW_k(nmk_k), exMW_l(nmk_l))
    }
    
    titleSim <- TMWM(k,l)
    if (titleSim==0 && mwPerc==0) {
      hSim <- avgSim
    } else if (titleSim==0 && mwPerc != 0) {
      theta_1 <- m/min(length(data_TV_flat[[k]][["featuresMap"]]), length(data_TV_flat[[l]][["featuresMap"]]))
      theta_2 <- 1 - theta_1
      hSim <- theta_1*avgSim + theta_2*mwPerc
    } else if (titleSim!=0 && mwPerc==0) {
      theta_1 <- 1-mu
      hSim <- theta_1*avgSim + mu*titleSim
    } else {
      theta_1 <- (1-mu) * m/min(length(data_TV_flat[[k]][["featuresMap"]]), length(data_TV_flat[[l]][["featuresMap"]]))
      theta_2 <- 1-mu-theta_1
      hSim <- theta_1*avgSim + theta_2*mwPerc + mu*titleSim
      
    }
    
    
    dissim_matrix[k,l] = 1 - hSim
    if (is.nan(dissim_matrix[k,l])) {
      stop()
    }
    
  }
}

return(dissim_matrix)
}




# TRAINING ----------------------------------------------------------------

#Bootstrapping and determining evaluation measures for several threshold values for each bootstrap
# The training with approximately 63% of the data

# First part for LSH: calculate pair quality, pair completeness and F1*-measure for each bootstrap and threshold value
bootstraps <- list()
PQ_matrix <- matrix(nrow = 5, ncol = 9)
PC_matrix <- matrix(nrow = 5, ncol = 9)
F1_star_matrix <- matrix(nrow = 5, ncol = 9)
compFrac_matrix <- matrix(nrow = 5, ncol = 9)

set.seed(NULL)
for (strap in 1:5) {
  bandRowComb=1
  bootstrap <- unique(sample(data_TV_flat, replace = TRUE))
  bootstraps[[strap]] <- bootstrap
  MW <- mwConstruction(bootstrap)
  
  
  foreach(b = c(5,10,20,25,50,100,125,250,500), r = c(100,50,25,20,10,5,4,2,1)) %do% {
    
    cat("Start of LSH_alg ", sep = "\n")
    output_list <- LSH_alg(MW, b, r, strap, data_TV_flat = bootstrap)
    cat("End of LSH_alg ", sep = "\n")
    PQ_matrix[strap,bandRowComb] <- output_list[[1]]
    PC_matrix[strap,bandRowComb] <- output_list[[2]]
    F1_star_matrix[strap,bandRowComb] <- output_list[[3]]
    compFrac_matrix[strap,bandRowComb] <- output_list[[4]]
    
    bandRowComb=bandRowComb+1
    
  }
} 


# Second part for MSM: calculate F1-meausure for each bootstrap, threshold value and epsilon value
F1_matrix <- array(dim = c(5,6,99))
gamma <- 0.756
mu <- 0.650
for (strap in 1:5) {
  bandRowComb=1
  bootstrap <- bootstraps[[strap]]
  MW <- mwConstruction(bootstrap)
  
  
  foreach(b = c(5,10,20,25,50,100), r = c(100,50,25,20,10,5)) %do% {
    
    cat("Start of LSH_alg ", sep = "\n")
    output_list <- LSH_alg(MW, b, r, strap, data_TV_flat = bootstrap)
    cat("End of LSH_alg ", sep = "\n")

    
    cat("Start of MSM ", sep = "\n")
    dissim_matrix <- MSM(data_TV_flat = bootstrap, gamma, mu, candidate_matrix = output_list[[5]])
    cat("End of MSM ", sep = "\n")
    
    # Grid search of the epsilon values
    eps<-1
    for (epsilon in seq(0.01,0.99,0.01)) {
      # Pair quality
      n_duplicates_DDM = 0
      n_comparisons_DDM = 0
      
      for (i in 1:(nrow(dissim_matrix)-1)) {
        for (j in (i+1):ncol(dissim_matrix)) {
          if (is.na(dissim_matrix[i,j])) {
            next
          } else {
            if (dissim_matrix[i,j] < epsilon & dissim_matrix[i,j] != Inf) {
              n_comparisons_DDM = n_comparisons_DDM+1
              if (bootstrap[[i]][["modelID"]] == bootstrap[[j]][["modelID"]]) {
                n_duplicates_DDM = n_duplicates_DDM+1
              }
            }
          }
        }
      }
      
      
      pair_quality_DDM = n_duplicates_DDM/n_comparisons_DDM
      
      # Pair completeness
      total_duplicates = 0
      for (i in 1:(length(bootstrap)-1)) {
        for (j in (i+1):length(bootstrap)) {
          if (bootstrap[[i]][["modelID"]] == bootstrap[[j]][["modelID"]]) {
            total_duplicates = total_duplicates+1
          }
        }
        
      }
      pair_completeness_DDM <- n_duplicates_DDM/total_duplicates
      
      # F_1 - measure
      F1_DDM <- harmonic.mean(c(pair_quality_DDM, pair_completeness_DDM))
      
      F1_matrix[strap,bandRowComb,eps] <- F1_DDM
      eps = eps+1
    }
    cat("Maximum F1 measure of the epsilon values:", max(F1_matrix[strap, bandRowComb,]), which.max(F1_matrix[strap, bandRowComb,]), sep = "\n")
    cat("End of band/row comb.", bandRowComb, "################################################################",sep = "\n")
    bandRowComb=bandRowComb+1
  } # end of foreach band/row
  
  
  cat("End of bootstrap", strap, "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&", sep = "\n")
  if (strap ==5){
    cat("END OF TRAINING ", sep = '\n')
  }
}

# Determine best epsilon value for each band/row combination
avg_epsilons <- colMeans(apply(F1_matrix, 1:2, which.max))
avg_epsilons <- avg_epsilons/100

# TESTING -----------------------------------------------------------------

# Using the optimal average epsilon values found in training on the test set

compFrac_matrix_test <- matrix(nrow = 5, ncol = 6)
F1_matrix_test <- matrix(nrow = 5, ncol = 6)
for (strap in 1:5) {
  bandRowComb=1
  bootstrap <- bootstraps[[strap]]
  test_set <- setdiff(data_TV_flat, bootstrap)
  bootstrap <- test_set
  
  MW <- mwConstruction(bootstrap)

  foreach(b = c(5,10,20,25,50,100), r = c(100,50,25,20,10,5)) %do% {
    
    cat("Start of LSH_alg ", sep = "\n")
    output_list <- LSH_alg(MW, b, r, strap, data_TV_flat = bootstrap)
    cat("End of LSH_alg ", sep = "\n")

    compFrac_matrix_test[strap,bandRowComb] <- output_list[[4]]
    
    
    cat("Start of MSM ", sep = "\n")
    dissim_matrix <- MSM(data_TV_flat = bootstrap, gamma, mu, candidate_matrix = output_list[[5]])
    cat("End of MSM ", sep = "\n")
    
    epsilon <- avg_epsilons[bandRowComb]
    
    n_duplicates_DDM = 0
    n_comparisons_DDM = 0
    
    # Pair quality
    for (i in 1:(nrow(dissim_matrix)-1)) {
      for (j in (i+1):ncol(dissim_matrix)) {
        if (is.na(dissim_matrix[i,j])) {
          next
        } else {
          if (dissim_matrix[i,j] < epsilon & dissim_matrix[i,j] != Inf) {
            n_comparisons_DDM = n_comparisons_DDM+1
            if (bootstrap[[i]][["modelID"]] == bootstrap[[j]][["modelID"]]) {
              n_duplicates_DDM = n_duplicates_DDM+1
            }
          }
        }
      }
    }
    
    
    pair_quality_DDM = n_duplicates_DDM/n_comparisons_DDM
    
    # Pair completeness
    total_duplicates = 0
    for (i in 1:(length(bootstrap)-1)) {
      for (j in (i+1):length(bootstrap)) {
        if (bootstrap[[i]][["modelID"]] == bootstrap[[j]][["modelID"]]) {
          total_duplicates = total_duplicates+1
        }
      }
      
    }
    pair_completeness_DDM <- n_duplicates_DDM/total_duplicates
    
    # F_1 - measure
    F1_DDM_test <- harmonic.mean(c(pair_quality_DDM, pair_completeness_DDM))
    
    F1_matrix_test[strap,bandRowComb] <- F1_DDM_test
    
    
    cat("F1 measure:", F1_DDM_test, sep = "\n")
    cat("End of band/row comb.", bandRowComb, "################################################################",sep = "\n")
    bandRowComb=bandRowComb+1
  } # end of foreach band/row
  
  
  cat("End of bootstrap", strap, "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&", sep = "\n")
  if (strap ==5){
    cat("END OF TESTING ", sep = '\n')
  }
}






# THE PLOTS -----------------------------------------------

#Plots after LSH
par(mfrow = c(2,3), pty='s', mar = c(4,4,0.1,0.1))
plot(colMeans(compFrac_matrix), colMeans(PC_matrix), xlab = "", ylab = "Pair completeness", type = 'l')
plot(colMeans(compFrac_matrix), colMeans(PQ_matrix), xlab = "", ylab = "Pair quality", type = 'l')
plot(colMeans(compFrac_matrix), colMeans(F1_star_matrix), xlab = "", ylab = "F1-measure", type = 'l')


plot(colMeans(compFrac_matrix[,1:5]), colMeans(PC_matrix[,1:5]), xlab = "Fraction of comparisons", ylab = "Pair completeness", type = 'l')
plot(colMeans(compFrac_matrix[,1:5]), colMeans(PQ_matrix[,1:5]), xlab = "Fraction of comparisons", ylab = "Pair quality", type = 'l')
plot(colMeans(compFrac_matrix[,1:5]), colMeans(F1_star_matrix[,1:5]), xlab = "Fraction of comparisons", ylab = "F1-measure", type = 'l')


#Plot after MSM
par(mfrow = c(1,1), pty='s', mar = c(4,4,0.7,0.1))
plot(colMeans(compFrac_matrix_test), colMeans(F1_matrix_test), xlab = "Fraction of comparisons", ylab = "F1-measure", type = 'l')



