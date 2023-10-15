# CODIGO TFG REVIEWS TRUSTEDSHOPS

rm (list = ls())

# ACTIVAMOS LAS LIBRERIAS ----

library (tidytext) 
library (tidyverse)
library (textcat)
library (cld2)
library (quanteda)
library (textstem)
library (tm)



# IMPORT CSV ----

# Importamos las reseñas limpiadas
# DAMOS FORMATO FECHA PARA LUBRIDATE

library(readr)

resenas <- read_delim("resenas export utf.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                                 trim_ws = TRUE)

################################################################################ 


# SEPARAR FECHA EN COLUMNAS ---- 

library (dplyr)
library (lubridate)

resenas = resenas %>%  
  
  mutate(month = lubridate:::month(Fecha, label = TRUE, abbr = TRUE)) %>%
  mutate(day = lubridate::day(Fecha)) %>% 
  mutate(wday = lubridate::wday(Fecha, label = TRUE, abbr = FALSE)) %>%
  mutate(year = lubridate::year(Fecha))


################################################################################ 

################################################################################                                                                                                                  
#                                                                              #
#                    TRATAMIENTO DE LISTAS DE ST Y REVIEWS----                 #
#                                                                              #
################################################################################

# CARGA ARCHIVOS ST, NOMBRES, ADVERBIOS 

st = data.table::fread(file="stop_words_english.txt", header = FALSE)

nombres = data.table::fread(file="nombres_propios.txt", header = FALSE)

adverbs = data.table::fread(file="adverbs_libmind_6k.txt", header = FALSE)

sttm = tm::stopwords(kind = "es")


#LOS PONEMOS EN MINUSCULAS

st$V1 = tolower(st$V1)

nombres$V1 = tolower(nombres$V1)

adverbs$V1 = tolower(adverbs$V1)

resenas$Review = tolower (resenas$Review)

# SUSTITUIR CARACTERES CON TILDE DE NOMBRES

nombres$V1 = textclean::replace_non_ascii(nombres$V1,
                                          remove.nonconverted = FALSE)

# SUSTITUIR APOSTROFES

resenas$Review = gsub("'"," ",resenas$Review)

# ELIMINAMOS Stop Words

resenas$Review  = tm::removeWords(resenas$Review, st$V1)

resenas$Review  = tm::removePunctuation(resenas$Review)

resenas$Review = tm::removeWords (resenas$Review, sttm)

resenas$Review = textclean::replace_non_ascii(resenas$Review, remove.nonconverted = FALSE)

library(stringi)

resenas$Review = stri_trans_general(resenas$Review,"Latin-ASCII")

################################################################################     


# LIMPIAR CONTRACCIONES Y TILDES

resenas$Review = textclean::replace_contraction(
  resenas$Review)

resenas$Review = textclean::replace_non_ascii(
  resenas$Review, remove.nonconverted = FALSE)

negative_words = c("2448","muy", "dosfarma", "pharmacius", "despues", "todo", "los",
                   "pedidos", "del", "buen", "pedido", "bien", "gracias", "las", "mas")
preposiciones = c("a", "ante","bajo","con","contra", "de", "desde", "hacia", "hasta",
                  "para", "por", "segun", "sin","sobre","tras", "durante", "mediante",
                  "llego", "esta", "como", "pero","esta","como","son")
stopwords_2 = c("farmacia","super", "comprar",
                "compra", "gracias",
                "llegar","hacer", "poder", "mucho", "cliente",
                "adema", "paquetir",  "dudar",
                "volverir",  "genial", "mejor", 
                "enviar", "siempre","muestra",
                "primero", "casa", "mostrar","pedido","mejorar", "decir",
                "gustar", "poner","competitivo", "hora", "problema", "menos",
                "contentar", "pedir", "mandar", "experiencia", "seguro", "online",
                "alguno", "venir", "siguiente", "gasto", "solo", "empresa",
                "embalar", "correo", "pagina", "pedi", "algun", "mismo", "cajar", "saludo", "dejar",
                "bueno")

resenas$Review = gsub("[0-9]"," ",resenas$Review)

# LEMMATIZACION MANUAL

resenas$Review = gsub("rapidez","rapido",resenas$Review)
resenas$Review = gsub("rapida","rapido",resenas$Review)


# BORRADO STOPWORDS----

resenas$Review = tm::removeWords (resenas$Review, negative_words)
resenas$Review = tm::removeWords (resenas$Review, preposiciones)
resenas$Review = tm::removeWords (resenas$Review, stopwords_2)


################################



#LEMATIZAR


devtools::install_github("quanteda/spacyr", build_vignettes = FALSE)
library(spacyr)
library (dplyr)
library (quanteda)
library (stringr)

# PREPARAMOS SPACY R PARA LEMATIZAR EN ESPAÑOL

spacyr::spacy_download_langmodel(model = "es")


spacy_initialize()
spacy_download_langmodel("es")


spacy_finalize()
spacy_initialize(model = "es_core_news_sm")


# DIVIDIMOS EN TOKLENS

datos_palabras <- resenas %>%
 unnest_tokens(word, Review)

#LEMATIZAMOS CON SPACYR

datos_palabras_lematizadas = spacy_parse(datos_palabras$word)


#YA ESTA LEMATIZADO, EJECUTAR DESDE AQUI

# save(tabla_unida, file = "Tabla_Lemma_TFG") -> V.1.0
save(datos_palabras_lematizadas, file = "Tabla_Lemma_TFG2") -> V.2.0


load("Tabla_Lemma_TFG2.RData")

# APARECEN TOKENS NUMERICOS EXTRA EN ALGUNA PALABRA, LOS ELIMINAMOS 

datos_palabras_lematizadas = datos_palabras_lematizadas %>%

filter(token_id == 1)

unique(datos_palabras_lematizadas$token_id)

#CORREGIMOS PALABRAS MAL ESCRITAS

datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "diar","dia")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "diir","dia")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "podriar","podria")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "llego","llegar")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "rapidisimo","rapido")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "varios","variedad")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "recomeir","recomendable")
datos_palabras_lematizadas$lemma = stringr::str_replace_all(datos_palabras_lematizadas$lemma, "condición","condicion")



tabla_unida = cbind (datos_palabras, lemma = datos_palabras_lematizadas$lemma)

# save (tabla_unida, file = "Tabla_palabras_lemma_TFG.RData") #V1.0
save (tabla_unida, file = "Tabla_palabras_lemma_TFG2.RData") #V2.0

# TABLA LEMATIZADA

load ("Tabla_palabras_lemma_TFG2.RData")


#EMPEZAMOS ANALISIS

library (syuzhet)
library (wordcloud)
library (RColorBrewer)
library (tm)
library (NLP)

# SELECCION NUMEROS ALEATORIOS

# (5,2,14) ,19, 43, 44, 17, 23, 26, 41, 9, 27, 30, 21, 10, 40, 7


# EXTRAER SENTIMIENTOS CON SYUZHET Y DICCIONARIO NRC
# USAR ARCHIVO GUARDADO PARA EVITAR HACER PROCESO ENTERO


sentimientos_df <- get_nrc_sentiment(tabla_unida$lemma, lang="spanish")

# save (sentimientos_df, file ="sentimientos_df_TFG.RData") -> V1.0
save (sentimientos_df, file ="sentimientos_df_TFG2.RData") -> V2.0


# 

load ("sentimientos_df_TFG2.RData")

summary(sentimientos_df)

barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 1.5,
  main = "'Reviews Farmacias Online",
  sub = "Elaboración propia",
  xlab="emociones", ylab = NULL)


################################################################################
#                                                                              #
#                 PALABRAS MAS REPETIDAS----                     #
#                                                                              #
################################################################################


library (ggplot2)
library(qdap)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggthemes)
library(wordcloud)

################################################################################
#                                                                              #
#                 PALABRAS MAS REPETIDAS FILTRADAS                             #
#                                                                              #
################################################################################
stopwords_prueba = c("llego", "rapidisimo","varios", "recomeir","condición")



stopwords_repeticion = c("encantado","excelente","gustado","bien","buen",
                         "perfecto", "eficaz", "recibir","correcto", "esperar",
                         "condicion")


most_common_word = freq_terms(text.var=tabla_unida$lemma,top=25, at.least = 4,
                              stopwords = c(stopwords_prueba, stopwords_2,
                                            stopwords_repeticion))


ggplot(most_common_word,aes(x = fct_reorder(WORD, FREQ, .desc=FALSE),y=FREQ))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()





#Generamos los listados de palabras

palabras_enfado = tabla_unida$lemma[sentimientos_df$anger > 1]
palabras_anticipacion = tabla_unida$lemma[sentimientos_df$anticipation > 1]
palabras_asco = tabla_unida$lemma[sentimientos_df$disgust > 1]
palabras_miedo = tabla_unida$lemma[sentimientos_df$fear > 1]
palabras_alegria = tabla_unida$lemma[sentimientos_df$joy > 1]
palabras_tristeza <- tabla_unida$lemma[sentimientos_df$sadness > 1]
palabras_sorpresa = tabla_unida$lemma[sentimientos_df$surprise > 1]
palabras_confianza = tabla_unida$lemma[sentimientos_df$trust > 1]



# Contamos repeticiones y Ordenamos los listados

palabras_enfado_orden <- sort(table(unlist(palabras_enfado)), decreasing = TRUE)
palabras_anticipacion_orden <- sort(table(unlist(palabras_anticipacion)), decreasing = TRUE)
palabras_asco_orden <- sort(table(unlist(palabras_asco)), decreasing = TRUE)
palabras_miedo_orden <- sort(table(unlist(palabras_miedo)), decreasing = TRUE)
palabras_alegria_orden <- sort(table(unlist(palabras_alegria)), decreasing = TRUE)
palabras_tristeza_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
palabras_sorpresa_orden <- sort(table(unlist(palabras_sorpresa)), decreasing = TRUE)
palabras_confianza_orden <- sort(table(unlist(palabras_confianza)), decreasing = TRUE)

head(palabras_enfado_orden, n = 12)
head(palabras_anticipacion_orden, n = 12)
head(palabras_asco_orden, n = 12)
head(palabras_miedo_orden, n = 12)
head(palabras_sorpresa_orden, n = 12)
head(palabras_confianza_orden, n = 12)

most_common_word


# Enfado


palabras_enfado_orden2 <- as.data.frame(palabras_enfado_orden)

palabras_enfado_orden2 = palabras_enfado_orden2 %>%
  filter(Var1 != 'ira')

palabras_enfado_orden2 <- as.data.frame(head(palabras_enfado_orden2, 10))

ggplot(palabras_enfado_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_enfado_orden2



# Anticipacion

palabras_anticipacion_orden2 <- as.data.frame(palabras_anticipacion_orden)

palabras_anticipacion_orden2 = palabras_anticipacion_orden2 %>%
  filter(Var1 != 'esperar él')%>%
  filter(Var1 != 'defensa')%>%
  filter(Var1 != 'peso')%>%
  filter(Var1 != 'fiesta')%>%
  filter(Var1 != 'pagar él')

palabras_anticipacion_orden2 <- as.data.frame(head(palabras_anticipacion_orden2, 10))


ggplot(palabras_anticipacion_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_anticipacion_orden2

# asco

palabras_asco_orden2 <- as.data.frame(head(palabras_asco_orden, 10))


ggplot(palabras_asco_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_asco_orden2

# miedo

palabras_miedo_orden2 <- as.data.frame(head(palabras_miedo_orden, 10))


ggplot(palabras_miedo_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_miedo_orden2

# alegria



palabras_alegria_orden2 <- as.data.frame(palabras_alegria_orden)

palabras_alegria_orden2 = palabras_alegria_orden2 %>%
  
  filter(Var1 != 'pagar él')

palabras_alegria_orden2 <- as.data.frame(head(palabras_alegria_orden2, 10))

ggplot(palabras_alegria_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_alegria_orden2

#tristeza



palabras_tristeza_orden2 <- as.data.frame(head(palabras_tristeza_orden, 10))


ggplot(palabras_tristeza_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_tristeza_orden2


#sorpresa

palabras_sorpresa_orden2 <- as.data.frame(palabras_sorpresa_orden)

palabras_sorpresa_orden2 = palabras_sorpresa_orden2 %>%
  
  filter(Var1 != 'santo')%>%
  filter(Var1 != 'mago')%>%
  filter(Var1 != 'fiesta')

palabras_sorpresa_orden2 <- as.data.frame(head(palabras_sorpresa_orden2, 10))


ggplot(palabras_sorpresa_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_sorpresa_orden2

#confianza

palabras_confianza_orden2 <- as.data.frame(head(palabras_confianza_orden))


ggplot(palabras_confianza_orden2,aes(x = fct_reorder(Var1, Freq, .desc=FALSE),y=Freq))+
  geom_bar(stat = "identity",fill = "grey")+
  xlab("Palabra")+
  ylab("Frecuencia")+
  theme_classic()+
  coord_flip()

palabras_confianza_orden2


# AHora vamos a ver los topicos

### HACEMOS EL TF-IDF

################################################################################
#                                                                              #
#                        TF - IDF                        #
#                                                                              #
################################################################################


prueba_tf_idf = tabla_unida %>%
  
  # SELECCIONAR LAS VARIABLES  
  
  dplyr::select('ID Farmacia',lemma)
names (prueba_tf_idf) = c('ID', 'lemma')


# ELIMINAMOS NOMBRES Y NEGATIVES
prueba_tf_idf = prueba_tf_idf %>%
  
  dplyr::mutate(lemma = ifelse(lemma %in% nombres$V1, NA, lemma)) %>%
  dplyr::mutate(lemma = ifelse(lemma %in% negative_words,
                               NA, lemma)) %>%
  dplyr::mutate(lemma = ifelse(lemma %in% preposiciones,
                               NA, lemma)) %>%
  
  # ELIMINAR PALABRAS CORTAS (2 O MENOS)
  
  mutate(lemma = ifelse(nchar(lemma) <= 2, NA, lemma)) %>%
  
  # ELIMINAR NA
  
  tidyr::drop_na(lemma) %>%
  
  # CONTAMOS WORD AGRUPANDO EN LA VARIABLE
  
  dplyr::count(ID, lemma,  sort = TRUE) %>%
  
  
  # TF-IDF DE TIDYLO
  
  
  tidylo::bind_log_odds(ID, lemma, n,) %>% 
  
  # SOLO PESO FINITO
  
  
  filter(is.finite(log_odds_weighted)) %>%
  
  # QUE NO SEAN NA
  
  filter(!is.na(log_odds_weighted)) %>%
  
  
  # ORDENAR DE MAYOR A MENOR
  
  arrange(-log_odds_weighted) %>%
  
  
  # DIVISION EN GRUPOS POR PESO
  
  mutate(tf_ntile = ntile(log_odds_weighted, 100)) %>%
  
  # FILTRAMOS NTILE MAYORES
  
  filter(tf_ntile >= 60) 

# FIN TF-IDF 




###############################################################################


################################################################################
#
#                       PRUEBA STM CON REVIEWS----
# 
################################################################################

# CARGAMOS LIBRERIAS

library (igraph)
library (reshape2)
library(quanteda)
library(stm)

# CREAMOS PRUEBA STM A PARTIR DE PRUEBA TF-IDF
# USAMOS TIDYTEXT CAST DFM

prueba_stm = prueba_tf_idf %>%
  tidytext::cast_dfm(ID, lemma, n)


# PRUEBA STM TOPICS

# K = numero de topics
# verbose = mostrar proceso en pantalla
# max.em.its = numero maximo de iteraciones
# seed = semilla a usar

topic_model = stm::stm(prueba_stm, K = 20, verbose = TRUE, max.em.its = 300, seed = 1234)

# MUESTRA EL MODELO DE TOPICS

stm::labelTopics(topic_model)

plot(topic_model, type = "summary", xlim = c(0, 0.2))


###############################################################################
#
#                           GRAFICO DEL TOPIC MODEL---- 
#
###############################################################################

# Pintamos un grafico, con topic_model, tipo de etiqueta frez y n=7 
# (numero palabras por topic)

plot(topic_model, labeltype = "frex", n = 7)

stm::findTopic(topic_model, n = 7, c("envio", "atencion", "descuentos", "detalles",
                                     "producto", "opciones", "transporte", "variedad",
                                     "asesoramiento", "comunicacion y seguimiento")) 


plotcorr = stm::topicCorr(topic_model, cutoff = 0.01, verbose = TRUE)

set.seed(4321)
stm::plot.topicCorr(plotcorr)




# NUBE DE PALABRAS PRIMERAS 4

nube_emociones_vector <- c(
  paste(tabla_unida$lemma[sentimientos_df$sadness > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$joy > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$surprise > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$anticipation > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('Tristeza', 'Alegría', 'Sorpresa', 'Anticipacion')

head(nube_tdm)


set.seed(1234) # puede ser cualquier número
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)




# NUBE DE PALABRAS OTRAS 4

nube_emociones_vector <- c(
  paste(tabla_unida$lemma[sentimientos_df$anger > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$disgust > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$fear > 0], collapse = " "),
  paste(tabla_unida$lemma[sentimientos_df$trust > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)


colnames(nube_tdm) = c("Enfado", "Asco", "Miedo", "Confianza") 

set.seed(1234) # puede ser cualquier número
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)


# CALCULO VALENCIA

sentimientos_valencia <- (sentimientos_df$negative *-1) + sentimientos_df$positive

simple_plot(sentimientos_valencia)





################################################################################
#
#                           ANALISIS DE SENTIMIENTOS  ----
#
###############################################################################

library(syuzhet) # Hace analisis de sentimientos
library(RColorBrewer) # Añade paleta de colores
library(wordcloud) # Hace nube de palabras
library(tm)


################################################################################
#                                                                              #
#                       EJECUTAMOS EL TEST DE SENTIMIENTOS ----                #
#                                                                              #
################################################################################
#                                                                              #
#             OJO TARDA MAS DE 45 MINUTOS, CARGAR ARCHIVO MAS ABAJO            #
#                                                                              #
################################################################################


# Hacemos la prueba de sentimientos con syuzhet

sentimientos_df = get_nrc_sentiment(prueba_tf_idf$lemma)

barplot(colSums(prop.table(sentimientos_df[, 1:8])))

palabras_tristeza <- texto_palabras[sentimientos_df$sadness> 0]


# Guardamos el archivo porque tarda en hacerlo 

save (sentimientos_df, file = "archivotfg/sentimientos.RData")


################################################################################
#                                                                              #
#               CARGA DEL ARCHIVO DEL TEST DE SENTIMIENTO ----                 #
#                                                                              #
################################################################################
#                                                                              #
#             EJECUTARLO PARA EVITAR HACER EL TEST DE 45 MINUTOS               #
#                                                                              #
################################################################################

load ("archivotfg/sentimientos.RData")


################################################################################
#                                                                              #
#                 PRESENTACION DE LA PRUEBA DE SENTIMIENTOS----                #
#                                                                              #
################################################################################


#Presentamos la prueba

summary(sentimientos_df)

#Representamos las barras con la proporcion de sentimientos 
# de la columna 1 a la 8

barplot(colSums(prop.table(sentimientos_df[, 1:10])))




################################################################################
#                                                                              #
#                 PRESENTACION DE LA NUBE DE EMOCIONES----                     #
#                                                                              #
################################################################################


# Creamos el vector de nube de emociones
# añadimos las palabras que hayan dado alguno de los adjetivos
# en su respectivo, con espacio entre las palabras

nube_emociones_vector <- c(
  
  paste(prueba_tf_idf$word[sentimientos_df$sadness> 0], collapse = " "),
  paste(prueba_tf_idf$word[sentimientos_df$joy > 0], collapse = " "),
  paste(prueba_tf_idf$word[sentimientos_df$anger > 0], collapse = " "),
  paste(prueba_tf_idf$word[sentimientos_df$fear > 0], collapse = " "))


#Usamos iconv para convertir el vector de nube de emociones a latin y UTF


nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

# Creamos el objeto nube_corpus con las palabras de nube emociones vector

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

# Creamos la matriz termino-documento

nube_tdm <- TermDocumentMatrix(nube_corpus)

# transformamos a matriz
nube_tdm <- as.matrix(nube_tdm)

#mostramos la matriz
head(nube_tdm)


# ASIGNAMOS EMOCIONES DE LA NUBE

colnames(nube_tdm) = c('tristeza', 'felicidad', 'enfado', 'confianza')

set.seed(4321) #SEMILLA



# MOSTRAMOS LA NUBE DE EMOCIONES ----

library (tidytext) 
library (tidyverse)
library (dplyr)
library (textcat)
library (cld2)
library (quanteda)
library (textstem)
library (lubridate)
library (tm)
library (wordcloud)
library (igraph)
library (reshape2)
library(quanteda)
library(stm)
library (psych)
library (car)
library (ggplot2)
library(syuzhet) # Hace analisis de sentimientos
library(RColorBrewer) # Añade paleta de colores
library(wordcloud) # Hace nube de palabras
library(tm)

comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

################################################################################
#                                                                              #
#                 EVOLUCION DE EMOCIONES POSITIVAS Y NEGATIVAS----             #
#                                                                              #
################################################################################


sentimientos = (sentimientos_df$negative *-1) + sentimientos_df$positive


simple_plot(sentimientos) # Tarda un poquito, a veces R da un error.

# Reiniciando R y ejecutandolo de nuevo funciona














