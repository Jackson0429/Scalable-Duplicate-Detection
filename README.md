# Scalable-Duplicate-Detection
The R code for the computer science individual assignment. 

The link to the data: https://personal.eur.nl/frasincar/datasets/TVs-all-merged.zip

## Description of the assignment
This repository contains the R code for the indivdual assignment of the course Computer Science for Business Analytics (FEM21037) at the Erasmus University Rotterdam. 
In this assignment I created a scalable duplicate detection algorithm using Locality Sensitive Hashing (LSH) and a modified version of the Multi-Component Similarity Method (MMSM). 
The data consists of 1624 television descriptions of four different Web shops. The link to this data is given above. 

## The code
There are two R files uploaded, CS_indivPaper_code.R is the main code and FunctionsComputerScienceAssignment.R is the file with all the necessary functions. I will explain each section in the main code file below. Basically all code needs to be run from top to bottom.

### START AND LOADING NECESSARY PACKAGES
In this part the workspace is reset and all necessary libraries are loaded. The functions from FunctionsComputerScienceAssignment.R are loaded as well and the directory must be changed for your device.

### IMPORTING AND MANIPULATING THE DATA
This section is for importing the data (again, the directory must be changed) and manipulating and cleaning the data.

### CONSTRUCTING THE MODEL WORDS
This part contains the function which constructs the model words and outputs a vector of these model words.

### THE LSH ALGORITHM
A function which performs the whole LSH algorithm. The output is a list with the cbinary candidate matrix and the metrics needed for evaluation.

### THE DUPLICATE DETECTION METHOD
The function that performs mine modified Multi-Component Similarity Method. The output is a matrix with dissimilarity values for each candidate pair.

### TRAINING
In the first part the output for the LSH is constructed for the different bootstraps and threshold values t. In the second part the whole algorithm is run for different bootstraps, threshold values t and epsilon values.

### TESTING
This is where the whole algorithm is tested with the optimal epsilon values found in the TRAINING section.

### THE PLOTS
Finally, in this section the plots given in the paper are plotted.



