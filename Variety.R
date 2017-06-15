library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)

# Funci?n que a?ade una columna nueva a la bolsa de palabras. Dicha columna contiene la 
# frecuencia de aparaci?n del conjunto de palabras en los tweets del autor.

NuevaColumna <- function(palabras="",contador="",line="",freq="") {
  for (pal in palabras) {
    if (length(freq[freq$WORD==pal,"FREQ"])>0) {
      contador <- contador+freq[freq$WORD==pal,"FREQ"]
    }  
  }
  line <- paste(line, ",", contador, sep="")
  return (line)
}

# Funci?n que lee de un fichero y devuelve una lista de dichos elementos no repetidos.
# Dicha lista es previamente preprocesada

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
  # Leemos de los ficheros para obtener listas
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
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    line <- author
    
    tamaño=0
    
    for(palabra in strsplit(txtdata," ")){
          tamaño=tamaño+length(palabra)
    
    }
        
    
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq/tamaño, sep="")
    }
    
    
    #Añadimos 7 columnas. Una por variedad. Cuenta el número de apariciones de las palabras más frecuentes de cada variedad.
    
    cuentaVenezuela <- 0
    line <- NuevaColumna(FicheroVenezuela,cuentaVenezuela/tamaño,line,freq)
    cuentaColombia <- 0
    line <- NuevaColumna(FicheroColombia,cuentaColombia/tamaño,line,freq)
    cuentaArgentina <- 0
    line <- NuevaColumna(FicheroArgentina,cuentaArgentina/tamaño,line,freq)
    cuentaSpain <- 0
    line <- NuevaColumna(FicheroSpain,cuentaSpain/tamaño,line,freq)
    cuentaChile <- 0
    line <- NuevaColumna(FicheroChile,cuentaChile/tamaño,line,freq)
    cuentaMexico <- 0
    line <- NuevaColumna(FicheroMexico,cuentaMexico/tamaño,line,freq)
    cuentaPeru <- 0
    line <- NuevaColumna(FicheroPeru,cuentaPeru/tamaño,line,freq)

    # A?adimos una octava columna con el total de palabras de los tweets del autor.
    line <- paste(line,",", tamaño,sep="")
    
    if (class=="variety") {
      line <- paste(variety, ",",line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    
    bow <- rbind(bow, line)
    
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}



# INICIO -- Generamos los ficheros con las 500 palabras más frecuentes por variedad.
# Guardamos el resultado en un fichero.csv que posteriormente modificamos generando un fichero.txt

vocabularyVenezuela <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="venezuela")
vocabularyVenezuela <- vocabularyVenezuela[1]
vocabularyVenezuela
write.csv(vocabularyVenezuela,file="venezuela.csv")

vocabularyColombia <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="colombia")
vocabularyColombia <- vocabularyColombia[1]
vocabularyColombia
write.csv(vocabularyColombia,file="colombia.csv")

vocabularyArgentina <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="argentina")
vocabularyArgentina <- vocabularyArgentina[1]
vocabularyArgentina
write.csv(vocabularyArgentina,file="argentina.csv")

vocabularySpain <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="spain")
vocabularySpain <- vocabularySpain[1]
vocabularySpain
write.csv(vocabularySpain,file="spain.csv")

vocabularyChile <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="chile")
vocabularyChile <- vocabularyChile[1]
vocabularyChile
write.csv(vocabularyChile,file="chile.csv")

vocabularyMexico <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="mexico")
vocabularyMexico <- vocabularyMexico[1]
vocabularyMexico
write.csv(vocabularyMexico,file="mexico.csv")

vocabularyPeru <- GenerateVocabulary(path_training, n=500, swlang="es",variedad="peru")
vocabularyPeru <- vocabularyPeru[1]
vocabularyPeru
write.csv(vocabularyPeru,file="peru.csv")

# FIN -- Generamos los ficheros de palabras más frecuentes por variedad


n <- 1000
path_training <- "~/13.-SocialMedia/ejercicio/pan-ap17-bigdata/training"  # Your training path
path_test <- "~/13.-SocialMedia/ejercicio/pan-ap17-bigdata/test"  		# Your test path

# Path con los ficheros .txt que se han procesado previamente como .csv y se han modificado.

pathVenezuela<-"../Diccionarios/Variety/venezuela.txt"
pathColombia<-"../Diccionarios/Variety/colombia.txt"
pathArgentina<-"../Diccionarios/Variety/argentina.txt"
pathSpain<-"../Diccionarios/Variety/spain.txt"
pathChile<-"../Diccionarios/Variety/chile.txt"
pathMexico<-"../Diccionarios/Variety/mexico.txt"
pathPeru<-"../Diccionarios/Variety/peru.txt"

vocabulary <- GenerateVocabulary(path_training, n=1000, swlang="es")
bow_training <- GenerateBoW(path_training, vocabulary, n, class="variety")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="variety")

training <- cSplit(bow_training, "V1", ",") # Hace la trasposicion de la matriz para que cada fila será un autor y las columnas sus palabras
test <- cSplit(bow_test, "V1", ",")

training <- training[,-2]
names(training)[1] <- "class"
truth  <- unlist(test[,1]) # vector con los resultados de la clase a predecir con valores reales
test <-test[,-1:-2]

# Fijamos la semilla

set.seed(100)

# A continuaci?n mostramos los 3 m?todos utilizados. Ejecutar el deseado.

# SVMLinear
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")

# Random Forest
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= training, trControl = train_control, ntree=100, method = "rf")

# CV con Penalized Multinomial Regression
train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3)
model_SVM <- train( class~., data= training, trControl = train_control, method = "multinom")


#print(model_SVM)
pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)
