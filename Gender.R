library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)

# Función que añade una columna nueva a la bolsa de palabras. Dicha columna contiene la 
# frecuencia de aparación del conjunto de palabras en los tweets del autor.

NuevaColumna <- function(palabras="",contador="",line="",freq="",tamaño=1) {
  for (pal in palabras) {
    if (length(freq[freq$WORD==pal,"FREQ"])>0) {
      contador <- contador+freq[freq$WORD==pal,"FREQ"]
    }   
  }
  line <- paste(line, ",", contador/tamaño, sep="")
  return (line)
}    

# Función que lee de un fichero y devuelve una lista de dichos elementos no repetidos.
# Dicha lista es previamente preprocesada

lectorCampos <- function(path, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, accents=TRUE,swlang = "", swlist = "", verbose = TRUE){
  prueba<-scan(path,what="character",sep=",")  

  if (lowcase) {
    prueba <- tolower(prueba)
  }
  
  #if (punctuations) {
  #    prueba <- removePunctuation(prueba)
  #  }
  #  if(accents){
  #    prueba<- chartr("áéíóú", "aeiou", prueba)
  #  }
  
  if (numbers) {
    prueba <- removeNumbers(prueba)
  }
  
  if (whitespaces) {
    prueba <- stripWhitespace(prueba)
    
  }
  #prueba<-removeWords(prueba, stopwords("es"))
  
  return (unique(prueba))
}

GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE,accents=TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
	
	files = list.files(pattern="*.xml")
	corpus.raw <- NULL
	i <- 0
	for (file in files) {
	 
	  xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
	
	      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
     
	  i <- i + 1
	  if (verbose) print(paste(i, " ", file))	
	}

	corpus.preprocessed <- corpus.raw
	
	
	if (lowcase) {
		if (verbose) print("Tolower...")
		corpus.preprocessed <- tolower(corpus.preprocessed)
	}	
	
	#if (punctuations) {
	#	if (verbose) print("Removing punctuations...")		
	#	corpus.preprocessed <- removePunctuation(corpus.preprocessed)
	#}
  #if(accents){
  #  if(verbose)print("Removing accents...")
  #  corpus.preprocessed <- chartr("áéíóú", "aeiou", corpus.preprocessed)
  #}

	if (numbers) {
		if (verbose) print("Removing numbers...")
		corpus.preprocessed <- removeNumbers(corpus.preprocessed)
	}

	if (whitespaces) {
		if (verbose) print("Stripping whitestpaces...")
		corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
	}

	#if (swlang!="")	{
	#	if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
	#	corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
	#}
	
#	if (swlist!="") {
#		if (verbose) print("Removing provided stopwords...")
#		corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
#	}

	if (verbose) print("Generating frequency terms")
	
	corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
	if (verbose) plot(corpus.frequentterms)

	return (corpus.frequentterms)
}

GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE,accents=TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
	setwd(path)

	truth <- read.csv("truth.txt", sep=":", header=FALSE)
	truth <- truth[,c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

	i <- 0
	bow <- NULL
	files = list.files(pattern="*.xml")

	# Leemos de los ficheros para obtener listas
  
	adjetivos<-lectorCampos(path_tematico2)
  sentimientos<-lectorCampos(path_tematico)
  hombres<-lectorCampos(path_tematico3)
  mujeres<-lectorCampos(path_tematico4)
  pronombres<-lectorCampos(path_tematico5)
	horoscopos<-lectorCampos(path_tematico6)
  superpops<-lectorCampos(path_tematico7)
 
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
    diferente=0
    prueba=strsplit(txtdata," ")
    prueba<-unlist(prueba)
    diferente<-length(unique(prueba))
    
    
		freq <- freq_terms(txtdata, n)
		for (word in vocabulary$WORD) {
			thefreq <- 0
			if (length(freq[freq$WORD==word,"FREQ"])>0) {
				thefreq <- freq[freq$WORD==word,"FREQ"]
			} 
			line <- paste(line, ",", thefreq/tamaño, sep="")
		}
		
    # Añadimos una columna para contar el número de apariciones de los 100 adjetivos más típicos.
    #La mujer tiene tendencia a utilizar más adjetivos
    
		#Añadimos 7 columnas. Son la frecuencia de apariciones de las siguientes listas:
    
    #  - Adjetivos
		#  - Pronombres
		#  - Palabras cariñosas
		#  - Palabras más frecuentes en mujeres
		#  - Palabras más frecuentes en hombres
		#  - Horóscopos
		#  - Palabras revista Super Pop
    
    cuentaAdj <- 0
    line <- NuevaColumna(adjetivos,cuentaAdj,line,freq,tamaño)
		cuentaPronombres <- 0
		line <- NuevaColumna(pronombres,cuentaPronombres,line,freq,tamaño)
    cuentaÑoño<-0
		line <- NuevaColumna(sentimientos,cuentaÑoño,line,freq,tamaño)
    cuentaMujer<-0
	  line <- NuevaColumna(mujeres,cuentaMujer,line,freq,tamaño)
    cuentaHombre<-0
	  line <- NuevaColumna(hombres,cuentaHombre,line,freq,tamaño)
    cuentaHoroscopo<-0
    line <- NuevaColumna(horoscopos,cuentaHoroscopo,line,freq,tamaño)
    cuentaSuperpop<-0
    line<- NuevaColumna(superpops,cuentaSuperpop,line,freq,tamaño)


		# Añadimos una octava columna con el total de palabras de los tweets del autor.
    
		line <- paste(line, ",", tamaño, sep="")

	
    
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


contador<-0
n <- 499

path_training <- "~/13.-SocialMedia/ejercicio/pan-ap17-bigdata/training"  # Your training path
path_test <- "~/13.-SocialMedia/ejercicio/pan-ap17-bigdata/test"    	# Your test path

path_tematico<-"../Diccionarios/Gender/sentimientos.txt"
path_tematico2<-"../Diccionarios/Gender/adjetivos.txt"
path_tematico3<-"../Diccionarios/Gender/hombres.txt"
path_tematico4<-"../Diccionarios/Gender/mujeres.txt"
path_tematico5<-"../Diccionarios/Gender/pronombres.txt"
path_tematico6<-"../Diccionarios/Gender/horoscopo.txt"
path_tematico7<-"../Diccionarios/Gender/superpop.txt"

vocabulary <- GenerateVocabulary(path_training, n, swlang="es")

bow_training <- GenerateBoW(path_training, vocabulary, n, class="gender")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="gender")

training <- cSplit(bow_training, "V1", ",") # Hace la trasposicion de la matriz para que cada fila será un autor y las columnas sus palabras
test <- cSplit(bow_test, "V1", ",")

training <- training[,-2]
#training<-training[,-100:-500]
names(training)[1] <- "class"
truth  <- unlist(test[,1]) # vector con los resultados de la clase a predecir con valores reales
test <-test[,-1:-2]



#Cross validation
train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 

#No cross Validation
#train_control <- trainControl(method="none")
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")


#PMR sin cross validation
#train_control <- trainControl(method="none")
set.seed(389)
#Penalized Multinomial Regression
model_SVM <- train( class~., data= training, trControl = train_control, method = "multinom")
#model_SVM <- train( class~., data= training, trControl = train_control,ntree=100, method = "rf")

#print(model_SVM)

pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)



