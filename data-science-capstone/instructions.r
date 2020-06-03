#' Data Science Capstone Project
#' 
#' Welcome to the Capstone Project
#' 
#' SwiftKey - Predictive Text Models
#' 
#' Project OVerview :
#' 
#' for example ... 
#' 
#'     I went to the .......
#'     
#'            - gym
#'            - store
#'            - restaurant
#'            
#' Start with the basics - Analyzing large corpus of text documents 
#'                               - find structure
#'                               - how words are put together
#'                       - Cleaning and analyzing text data
#'                       - Building and sampling from a predictive text model           
#'                       - Predictive Text Product
#'                       
#' In general : Analysis of data & natural language processing 
#' 
#' Evaluated by:
#' 
#' 1 .- Introductory quiz to test if downloaded and manipulated the data
#' 2 .- Intermediate R markdown report describing in plain language exploratory
#'         analysis of the course data
#' 3 .- Two natural language processing quizes, applying the predictive model
#'         to the data to check how it is working
#' 4 .- A shiny app that takes as input a phrase (multiple words) and predicts
#' 5 .- A 5 slide deck created with R presentations pitching the algorithm.
#' 
#' 
#' 
#' Some Resources
#' 
#' https://www.coursera.org/learn/data-science-project/supplement/FrBtO/project-overview
#' 
#' 
#' 
#' Course Tasks:
#' 
#' 1.- Understanding the Problem
#' 2.- Data Acquisition and cleaning
#' 3.- Exploratory Analysis
#' 4.- Statistical Modeling
#' 5.- Predictive Modeling
#' 6.- Creative Exploration
#' 7.- Creating a data product
#' 8.- Creating a short slide deck pitching your product.
#' 
#' Assesment and Grading:
#' 
#' To successfully complete the capstone project, you must receive a passing
#' grade on all of the following assignments:
#' 
#' 1. Quiz 1: Getting Started
#' 2. Milestone Report: exploratory analysis of the data set + evaluation of 
#'      at leat three classmate submissions.
#' 3. Quiz 2: Natural Language Processing I
#' 4. Quiz 3: Natural Language Processing II
#' 5. Final Project: Your data product and presentation describing your final
#'      data product + evaluation of at least three classmate submissions
#'      
#'   (quizes will be multiple choice. The others graded by peers)
#'   
#' Final Grade calculated as follows:
#' 
#'      Quiz 1           -  5%
#'      Milestone Report - 20%
#'      Quiz II          - 10%
#'      Quiz III         - 10%
#'      Final Project    - 55%     
#'      
#' Course dataset:
#' 
#' This is the training data to get you started that will be the basis for most of the capstone. You must download the data from the link below and not from external websites to start.
#' https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
#' Later in the course you may use external data sets to augment your model 
#'    as you see fit.          
#' 
#' 
#' ---------------------------------------------------
#' INTRODUCTION TO TASK 0 : UNDERSTANDING THE PROBLEM
#' ---------------------------------------------------
#
# SYNTAX: 
#
# Here are some syntax techniques that can be used:
#
#     Lemmatization: It entails reducing the various inflected forms of a word into a single form for easy analysis.
#     Morphological segmentation: It involves dividing words into individual units called morphemes.
#     Word segmentation: It involves dividing a large piece of continuous text into distinct units.
#     Part-of-speech tagging: It involves identifying the part of speech for every word.
#     Parsing: It involves undertaking grammatical analysis for the provided sentence.
#     Sentence breaking: It involves placing sentence boundaries on a large piece of text.
#     Stemming: It involves cutting the inflected words to their root form. 
#

#' ---------------------------------------------------
#' MODELING
#' ---------------------------------------------------
# Hints, tips, and tricks
# 
# As you develop your prediction model, two key aspects that you will have to keep in mind are the size and runtime of the algorithm. These are defined as:
#     
#     Size: the amount of memory (physical RAM) required to run the model in R
# Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input
# Your goal for this prediction model is to minimize both the size and runtime of the model in order to provide a reasonable experience to the user.
# 
# Keep in mind that currently available predictive text models can run on mobile phones, which typically have limited memory and processing power 
# compared to desktop computers. Therefore, you should consider very carefully (1) how much memory is being used by the objects in your workspace; 
# and (2) how much time it is taking to run your model. Ultimately, your model will need to run in a Shiny app that runs on the shinyapps.io server.
# 
# Tips, tricks, and hints
# 
# Here are a few tools that may be of use to you as you work on their algorithm:
#     
#     object.size(): this function reports the number of bytes that an R object occupies in memory
# Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package 
# (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
# gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being 
# used by R.
# There will likely be a tradeoff that you have to make in between size and runtime. For example, an algorithm that requires a lot of memory, may 
# run faster, while a slower algorithm may require less memory. You will have to find the right balance between the two in order to provide a good 
# experience to the user.
