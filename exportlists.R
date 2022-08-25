# Prepare highlighter word lists

# Check if pacman installed to (install and) load other packages
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
  library("pacman")
} else {
  library("pacman")
}

# Load required packages using pacman
pacman::p_load(shiny, shinyWidgets, shinyjs, 
               googledrive, googlesheets4, 
               tidyverse, DT, data.table, 
               quanteda, glmnet)

setwd("C:/Users/Richard/OneDrive/Studies/Internships/UA/Code/RichardAudienciaClassifier/HighlighterAudiencias/highlighterAudiencias")

############################################
# Open pre-coded audiencias for highlighter
############################################

highlighterRecordsURL <- "https://github.com/rdwilkinson/highlighterAudiencias/blob/main/highlighterAudiencias.csv?raw=true"
highlighterRecords <- readr::read_csv(highlighterRecordsURL)

# Remove quotes
highlighterRecords <- highlighterRecords %>%
  mutate(across(c(topic, outcome), ~ gsub('"', "", ., perl = T))) 

# Filter out non-coded records
highlighterRecords <- highlighterRecords %>%
  filter(!(is.na(occurred)))

# Strings to find
highlighterRecords$mergedTexts <- paste(highlighterRecords$topic, highlighterRecords$representados)

highlighterRecords <- highlighterRecords %>%
  # Requestor type
  mutate(business = ifelse(str_detect(typeRequestor, "empresa"), 1, 0)) %>%
  mutate(org = ifelse(str_detect(typeRequestor, "organización"), 1, 0)) %>%
  mutate(instit = ifelse(str_detect(typeRequestor, "institución"), 1, 0)) %>%
  # Solicitud type
  mutate(ayudaSoc = ifelse(str_detect(typeSolicitud, "ayuda social/económica"), 1, 0)) %>%
  mutate(infra = ifelse(str_detect(typeSolicitud, "infraestructura urbana/rural"), 1, 0)) %>%
  mutate(req.info = ifelse(str_detect(typeSolicitud, "información"), 1, 0)) %>%
  mutate(permiso = ifelse(str_detect(typeSolicitud, "permiso/autorización"), 1, 0)) %>%
  mutate(empleo = ifelse(str_detect(typeSolicitud, "empleo"), 1, 0)) %>%
  mutate(vivienda = ifelse(str_detect(typeSolicitud, "vivienda"), 1, 0)) %>%
  mutate(legal = ifelse(str_detect(typeSolicitud, "asistencia legal"), 1, 0)) %>%
  mutate(transport = ifelse(str_detect(typeSolicitud, "transporte"), 1, 0)) %>%
  mutate(education = ifelse(str_detect(typeSolicitud, "educación"), 1, 0)) %>%
  mutate(subvention = ifelse(str_detect(typeSolicitud, "subvención"), 1, 0)) %>%
  mutate(intercesion = ifelse(str_detect(typeSolicitud, "intercesión"), 1, 0)) %>%
  mutate(fisc = ifelse(str_detect(typeSolicitud, "fiscalización"), 1, 0)) %>%
  mutate(reclamo = ifelse(str_detect(typeSolicitud, "reclamo") | str_detect(typeSolicitud, "denuncia"), 1, 0)) %>%
  # Offer type
  mutate(offer.info = ifelse(str_detect(typeOffer, "información"), 1, 0)) %>%
  mutate(offer.goods = ifelse(str_detect(typeOffer, "bien/servicio"), 1, 0)) %>%
  mutate(offer.project = ifelse(str_detect(typeOffer, "proyecto") | str_detect(typeSolicitud, "convenio"), 1, 0)) %>%
  # Deriv
  mutate(deriv = ifelse(is.na(highlighterRecords$typeDerivacion), 0, 1)) %>%
  # NotOccurred
  mutate(notOccurred = ifelse(highlighterRecords$occurred == 1 | highlighterRecords$occurred == T, 0, 1)) %>%
  # SaludoProt
  mutate(saludoCoded = ifelse(highlighterRecords$saludoProt == 1 | highlighterRecords$saludoProt == T, 1, 0)) %>%
  # Queja
  mutate(quejaCoded = ifelse(highlighterRecords$quejaMuni == 1 | highlighterRecords$quejaMuni == T, 1, 0))

# Replace NAs with 0
highlighterRecords <- highlighterRecords %>%
  mutate_at(vars(business:quejaCoded), ~replace(., is.na(.), 0))

audienciaRecords.forModel <- highlighterRecords
# audienciaRecords.forModel <- highlighterRecords %>%
#   filter(!is.na(business), !is.na(saludoCoded), !is.na(ayudaSoc),
#          !is.na(org), !is.na(instit), !is.na(infra), !is.na(deriv),
#          !is.na(offer.info), !is.na(offer.goods), !is.na(offer.project),
#          !is.na(notOccurred))

# DFM type
audiencias.tokens <- tokens(corpus(audienciaRecords.forModel, 
                                   text_field = 'mergedTexts'), 
                            remove_punct = T, 
                            remove_numbers = T) 

audiencias.ngrams <- tokens_ngrams(audiencias.tokens, 
                                   n = 1:5, 
                                   concatenator = " ")

audiencias.dfm <- dfm(tokens_select(audiencias.ngrams,
                                    min_nchar = 2,
                                    pattern = stopwords("es"), 
                                    selection = "remove"))
# 
# # DFM topic
# audiencias.tokens.topic <- tokens(corpus(audienciaRecords.forModel, 
#                                    text_field = 'topic'), 
#                             remove_punct = T, 
#                             remove_numbers = T) 
# 
# audiencias.ngrams.topic <- tokens_ngrams(audiencias.tokens, n = 1:5)
# 
# audiencias.dfm.topic <- dfm(tokens_select(audiencias.ngrams.topic, 
#                                     pattern = stopwords("es"), 
#                                     selection = "remove"))
# 
# DFM deriv
audiencias.tokens.deriv <- tokens(corpus(audienciaRecords.forModel,
                                         text_field = 'outcome'),
                                  remove_punct = T, 
                                  remove_numbers = T)

audiencias.ngrams.deriv <- tokens_ngrams(audiencias.tokens.deriv, 
                                         n = 1:5, 
                                         concatenator = " ")

audiencias.dfm.deriv <- dfm(tokens_select(audiencias.ngrams,
                                          min_nchar = 2,
                                          pattern = stopwords("es"),
                                          selection = "remove"))

# Highlighter colour keyword list
highlighterColourList <- c("red", "lightsalmon", "darkkhaki",
                           "olivedrab", "turquoise", "lightsteelblue",
                           "pink", "palevioletred", "navajowhite",
                           "orangered", "peru", "gold", "silver",
                           "lightcoral", "lightblue", "plum",
                           "springgreen", "chocolate", "crimson", "mediumaquamarine",
                           "mistyrose", "rosybrown",
                           "span", "style", "background", "color")

# Lasso model function
runLassoModel <- function(outcomeVar, sourceDFM) {
  
  # Check if enough annotations for model
  if(length(audienciaRecords.forModel[[outcomeVar]][audienciaRecords.forModel[[outcomeVar]] == 1]) > 10) {
    
    print(paste("Running model for:", outcomeVar, 
                "(", 
                length(audienciaRecords.forModel[[outcomeVar]][audienciaRecords.forModel[[outcomeVar]] == 1]),
                ")"))
    
    lasso.fit <- cv.glmnet(x = `sourceDFM`,
                           y = docvars(`sourceDFM`)[[outcomeVar]],
                           alpha = 1, 
                           family='binomial', maxit = 10^4)
    
    small.lambda.index <- which(lasso.fit$lambda == lasso.fit$lambda.min)
    small.lambda.betas <- lasso.fit$glmnet.fit$beta[,small.lambda.index]
    
    output.List <- data.frame(names = names(small.lambda.betas), vals = unname(small.lambda.betas))
    
    if(sum(output.List$vals) > 0) {
      output.List <- output.List %>%
        filter(vals > 0) %>%
        slice_max(vals, n = 100) %>%
        select(names) %>%
        filter(!(names %in% highlighterColourList)) %>%
        mutate(names = enc2utf8(names)) %>%
        #mutate(names = gsub(pattern = "_", replacement = " ", x = names)) %>%
        as.list()
      
      print(output.List)
    } else {
      output.List <- list("NO ELEMENTS IN LIST")
    }
  }
  # Not enough annotations
  else {
    print(paste("Not enough annotations yet for model of: ", outcomeVar,
                "(", 
                length(audienciaRecords.forModel[[outcomeVar]][audienciaRecords.forModel[[outcomeVar]] == 1]),
                ")"))
    output.List <- "NOT ENOUGH ANNOTATIONS YET"
  }
  
  return(output.List)
}

# Occurred
occurred.List <- runLassoModel("notOccurred", audiencias.dfm) # silver

# Requestor type
business.List <- runLassoModel("business", audiencias.dfm) # lightblue
org.List <- runLassoModel("org", audiencias.dfm) # springgreen
instit.List <- runLassoModel("instit", audiencias.dfm) # lightcoral

# Solicitud type
infra.List <- runLassoModel("infra", audiencias.dfm) #
ayudaSoc.List <- runLassoModel("ayudaSoc", audiencias.dfm) #
permiso.List <- runLassoModel("permiso", audiencias.dfm)
empleo.List <- runLassoModel("empleo", audiencias.dfm)
vivienda.List <- runLassoModel("vivienda", audiencias.dfm)
legal.List <- runLassoModel("legal", audiencias.dfm) # lightsalmon
req.info.List <- runLassoModel("req.info", audiencias.dfm) # palevioletred
transport.List <- runLassoModel("transport", audiencias.dfm) # olivedrab
education.List <- runLassoModel("education", audiencias.dfm) # chocolate
subvention.List <- runLassoModel("subvention", audiencias.dfm) # crimson
intercesion.List <- runLassoModel("intercesion", audiencias.dfm) # mediumaquamarine
fisc.List <- runLassoModel("fisc", audiencias.dfm) # mistyrose
reclamo.List <- runLassoModel("reclamo", audiencias.dfm) # rosybrown

# Propuestas
offer.project.List <- runLassoModel("offer.project", audiencias.dfm) # turquoise
offer.goods.List <- runLassoModel("offer.goods", audiencias.dfm) # lightsteelblue
offer.info.List <- runLassoModel("offer.info", audiencias.dfm) # pink

# Saludo Prot
saludoProt.List <- runLassoModel("saludoCoded", audiencias.dfm) # plum

# Queja
queja.List <- runLassoModel("quejaCoded", audiencias.dfm) #

# Derivacion
deriv.List <- runLassoModel("deriv", audiencias.dfm.deriv) #

# Write lists
writeList <- function(listname) {
  
  con <- file(paste0(deparse(substitute(listname)), ".txt"), open = "w", encoding = "native.enc")
  
  listname <- paste(unlist(listname), collapse = "|") # silver
  
  # Use useBytes = TRUE
  writeLines(toString(listname), con = con, useBytes = TRUE)
  
  
  # Close connection
  close(con)
}

writeList(occurred.List)

# Requestor type
writeList(business.List) 
writeList(org.List)
writeList(instit.List)

# Solicitud type
writeList(infra.List)
writeList(ayudaSoc.List)
writeList(permiso.List)
writeList(empleo.List)
writeList(vivienda.List)
writeList(legal.List)
writeList(req.info.List)
writeList(transport.List)
writeList(education.List)
writeList(subvention.List)
writeList(intercesion.List)
writeList(fisc.List)
writeList(reclamo.List)

# Propuestas
writeList(offer.project.List)
writeList(offer.goods.List)
writeList(offer.info.List)

# Saludo Prot
writeList(saludoProt.List)

# Queja
writeList(queja.List)

# Derivacion
writeList(deriv.List)
