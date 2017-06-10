library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)

#setwd("~/13.-SocialMedia/ejercicio/pan-ap17-bigdata")

NuevaColumna <- function(palabras="",contador="",line="",freq="") {
  for (pal in palabras) {
    if (length(freq[freq$WORD==pal,"FREQ"])>0) {
      contador <- contador+freq[freq$WORD==pal,"FREQ"]
    }  
  }
  line <- paste(line, ",", contador, sep="")
  return (line)
}
lectorCampos <- function(path, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, accents=TRUE,swlang = "", swlist = "", verbose = TRUE){
  prueba<-scan(path,what="character",sep=",") 
  if (lowcase) {
    prueba <- tolower(prueba)
  }
  
  if (punctuations) {
    prueba <- removePunctuation(prueba)
  }
  if(accents){
    prueba<- chartr("áéíóú", "aeiou", prueba)
  }
  
  if (numbers) {
    prueba <- removeNumbers(prueba)
  }
  
  if (whitespaces) {
    prueba <- stripWhitespace(prueba)
  }
  return (unique(prueba))
}
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE,accents=TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE, variedad = "") {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  corpus.raw <- NULL
  i <- 0
  #files=files[0:1]
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    if (variedad==variety || variedad=="") {
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    }
    
    i <- i + 1
    if (verbose) print(paste(i, " ", file))   
  }
  
  corpus.preprocessed <- corpus.raw
  
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }   
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")       
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  if(accents){
    if(verbose)print("Removing accents...")
    corpus.preprocessed <- chartr("áéíóú", "aeiou", corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")    {
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}
GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE,accents=TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  FicheroVenezuela <- lectorCampos(pathVenezuela)
  FicheroColombia <- lectorCampos(pathColombia)
  FicheroArgentina <- lectorCampos(pathArgentina)
  FicheroSpain <- lectorCampos(pathSpain)
  FicheroChile <- lectorCampos(pathChile)
  FicheroMexico<- lectorCampos(pathMexico)
  FicheroPeru <- lectorCampos(pathPeru)
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  files = list.files(pattern="*.xml")
  #files=files[0:10]
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    if(accents){
      txtdata <- chartr("áéíóú", "aeiou", txtdata)
    }
