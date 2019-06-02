# Predicting Wine Ratings
# MSBX 5415 Final Project
# Authors: Ali Chen, Matt Hardwick, Annie Ludlow, Hunter Rush

##### Data Cleansing #####
wine <- read.csv("winemag-data-130k-v2.csv", na.strings = c('', 'NA'))

# remove irrelevant columns
wine$X <- NULL
wine$taster_twitter_handle <- NULL
wine$designation <- NULL

# US subset
us <- subset(wine, country == 'US')
us$country <- NULL
us$region_2 <- NULL
us$winery <- NULL
us$region_1 <- NULL

# extract year from title
library(stringr)
us$year <- str_extract(us$title, '(19\\d{2})|(20\\d{2})')
us$year <- as.numeric(us$year)
us$title <- NULL

# impute year
us$year <- ifelse(is.na(us$year), median(us$year, na.rm=TRUE), us$year)

# impute price
summary(us$price)
us <- subset(us, price < 1000) # remove outlier/data error?
us$price <- ifelse(is.na(us$price), median(us$price), us$price) # use median because of skew/outliers

# California dummy variable
us$california <- ifelse(us$province == 'California', 1, 0)

# group tasters
table(us$taster_name)

us$taster_name <- ifelse(is.na(us$taster_name), 'None', as.character(us$taster_name))

taster <- c('Lee C. Iijima', 'Jim Gordon', 'Matt Kettmann', 'Paul Gregutt', 'Sean P. Sullivan', 'Virginie Boone', 'None')

us$taster_name <- ifelse(us$taster_name %in% taster, as.character(us$taster_name), 'Other')

# group variety
length(unique(us$variety))
sort(table(droplevels(us$variety)))

riesling <- c('Riesling', 'Gewürztraminer', 'Chenin Blanc', 'Moscato', 'Muscat Canelli', 'Muscat', 'White Riesling', 'Vignoles', 'Traminette', 'Johannisberg Riesling', 'Torrontés', 'Trousseau Gris', 'Gewürztraminer-Riesling', 'Muscadine', 'Moscato Giallo', 'Melon', 'Greco', 'Apple')

pinot_gris <- c('Pinot Gris', 'Pinot Grigio', 'Albariño', 'Rkatsiteli')

sauvignon_blanc <- c('Sauvignon Blanc', 'Grüner Veltliner', 'Fumé Blanc', 'Vermentino', 'Semillon-Sauvignon Blanc', 'Sauvignon Blanc-Semillon', 'Picpoul', 'Verdejo', 'Ribolla Gialla')

chardonnay <- c('Chardonnay', 'Viognier', 'Pinot Blanc', 'Sémillon', 'Auxerrois', 'Aligoté', 'Chardonnay-Viognier', 'Chardonnay-Semillon', 'Viognier-Chardonnay', 'Marsanne-Viognier', 'Chardonnay-Albariño', 'Viognier-Valdiguié', 'Pinot Auxerrois')

pinot_noir <- c('Pinot Noir', 'Pinot Meunier', 'Gamay Noir', 'Baco Noir', 'Valdiguié', 'Sagrantino', 'Gamay', 'Pinot Nero', 'Mondeuse')

zinfandel <- c('Zinfandel', 'Grenache', 'Tempranillo', 'Mourvèdre', 'Primitivo', 'Carignan', 'Negroamaro')

syrah <- c('Syrah', 'Petite Sirah', 'Malbec', 'Shiraz', 'Syrah-Grenache', 'Grenache-Syrah', 'Syrah-Petite Sirah', 'Syrah-Mourvèdre', 'Syrah-Viognier', 'Syrah-Tempranillo', 'Malbec-Syrah')

cabernet_sauvignon <- c('Cabernet Sauvignon', 'Merlot', 'Cabernet Franc', 'Carmenère', 'Cabernet Sauvignon-Merlot', 'Claret', 'Merlot-Cabernet Franc', 'Norton', 'Cabernet Sauvignon-Cabernet Franc', 'Merlot-Cabernet Sauvignon', 'Cabernet Merlot', 'Lagrein', 'Merlot-Cabernet', 'Cabernet Franc-Cabernet Sauvignon', 'Cabernet')

red_blend <- c('Red Blend', 'Bordeaux-style Red Blend', 'Rhône-style Red Blend', 'Sangiovese', 'Meritage', 'Barbera', 'Petit Verdot', 'G-S-M', 'Cabernet Sauvignon-Syrah', 'Nebbiolo', 'Tannat', 'Dolcetto', 'Lemberger', 'Cinsault', 'Syrah-Cabernet Sauvignon', 'Carignane', 'Charbono', 'Petite Verdot', 'Cabernet Blend', 'Montepulciano', 'Chambourcin', 'Teroldego', 'Blaufränkisch', 'Aglianico', 'Graciano', 'Garnacha', 'Pinotage', 'Counoise', 'Zweigelt', 'Syrah-Cabernet', 'Monastrell', 'Mission', 'Tempranillo Blend', 'Sangiovese-Cabernet Sauvignon', 'Malbec-Merlot', 'Cabernet-Syrah', 'Cabernet Sauvignon-Sangiovese', 'Alicante Bouschet', 'Sangiovese-Syrah', 'Grenache-Mourvèdre', 'Grenache-Carignan', 'Dornfelder', 'Syrah-Cabernet Franc', 'Sangiovese Cabernet', 'Saperavi', 'Grenache Blend', 'Jacquez', 'Mataro', 'Cabernet Sauvignon-Malbec', 'Viognier-Marsanne', 'Tempranillo-Garnacha', 'Tempranillo-Cabernet Sauvignon', 'Roussanne-Marsanne', 'Pinot Noir-Syrah', 'Negrette', 'Mourvèdre-Syrah', 'Marquette', 'Malbec-Tannat', 'Grenache Noir', 'Debit', 'Chelois', 'Chancellor', 'Carignan-Grenache', 'Cabernet Pfeffer', 'Cabernet Franc-Lemberger', 'Touriga Nacional Blend', 'Tempranillo-Syrah', 'Tannat-Syrah', 'Syrah-Petit Verdot', 'Syrah-Merlot', 'Syrah-Grenache-Viognier', 'St. Vincent', 'Sangiovese Grosso', 'Refosco', 'Merlot-Petite Verdot', 'Merlot-Malbec', 'Malbec-Cabernet Sauvignon', 'Madeira Blend', 'Diamond', 'Cabernet-Shiraz', 'Cabernet Sauvignon-Tempranillo', 'Cabernet Sauvignon-Shiraz', 'Cabernet Sauvignon-Carmenère', 'Cabernet Sauvignon-Barbera', 'Barbera', 'Barbera-Nebbiolo', 'Nero d\'Avola')

white_blend <- c('White Blend', 'Rhône-style White Blend', 'Roussanne', 'Grenache Blanc', 'Bordeaux-style White Blend', 'Marsanne', 'Verdelho', 'Petit Manseng', 'Marsanne-Roussanne', 'Seyval Blanc', 'Tocai Friulano', 'Arneis', 'Roussanne-Viognier', 'Malvasia Bianca', 'Colombard', 'Chenin Blanc-Viognier', 'Müller-Thurgau', 'Fiano', 'Viognier-Roussanne', 'Pinot-Chardonnay', 'Blanc du Bois', 'Semillon-Chardonnay', 'Vidal', 'Ugni Blanc', 'Trebbiano', 'Edelzwicker', 'Viognier-Grenache Blanc', 'Viognier-Gewürztraminer', 'Falanghina', 'Cortese', 'Cayuga', 'Siegerrebe', 'Silvaner', 'Sauvignon Blanc-Chenin Blanc', 'Roussanne-Grenache Blanc', 'Riesling-Chardonnay', 'Piquepoul Blanc', 'Pinot Gris-Gewürztraminer', 'Pinot Grigio-Sauvignon Blanc', 'Pinot Blanc-Viognier', 'Pinot Blanc-Chardonnay', 'Muscat Blanc à Petits Grains', 'Malvasia', 'Kerner', 'Clairette', 'Chardonnay-Riesling', 'Chardonnay-Pinot Blanc', 'Chardonel', 'Alvarinho', 'Alvarelhão', 'Abouriou')

rose <- c('Rosé', 'Rosato', 'Sauvignon Gris', 'Scheurebe', 'Madeleine Angevine', 'Rosado', 'Pinot Blanc-Pinot Noir', 'Garnacha Blanca')

sparkling <- c('Sparkling Blend', 'Champagne Blend', 'Prosecco')

dessert <- c('Port', 'Vidal Blanc', 'Orange Muscat', 'Touriga Nacional', 'Muscat Blanc', 'Black Muscat', 'Muskat Ottonel', 'Symphony', 'Touriga', 'Souzao', 'Sherry', 'Muscat Hamburg', 'Morio Muskat', 'White Port', 'Black Monukka', 'Valvin Muscat', 'Tinta Madeira', 'Tinta Amarela', 'Sauvignon Musqué', 'Palomino', 'Muskat', 'Muscat d\'Alexandrie')

us$type <- ifelse(us$variety %in% riesling, 'Reisling', as.character(us$type))
us$type <- ifelse(us$variety %in% pinot_gris, 'Pinot Gris', as.character(us$type))
us$type <- ifelse(us$variety %in% sauvignon_blanc, 'Sauvignon Blanc', as.character(us$type))
us$type <- ifelse(us$variety %in% chardonnay, 'Chardonnay', as.character(us$type))
us$type <- ifelse(us$variety %in% pinot_noir, 'Pinot Noir', as.character(us$type))
us$type <- ifelse(us$variety %in% zinfandel, 'Zinfadel', as.character(us$type))
us$type <- ifelse(us$variety %in% syrah, 'Syrah', as.character(us$type))
us$type <- ifelse(us$variety %in% cabernet_sauvignon, 'Cabernet Sauvignon', as.character(us$type))
us$type <- ifelse(us$variety %in% red_blend, 'Red Blend', as.character(us$type))
us$type <- ifelse(us$variety %in% white_blend, 'White Blend', as.character(us$type))
us$type <- ifelse(us$variety %in% rose, 'Rose', as.character(us$type))
us$type <- ifelse(us$variety %in% sparkling, 'Sparkling', as.character(us$type))
us$type <- ifelse(us$variety %in% dessert, 'Dessert', as.character(us$type))

# grouping variety further
us$type2 <- NA
us$type2 <- ifelse(us$type == "Reisling" | us$type == "Pinot Gris" | us$type == "Sauvignon Blanc" | us$type == "Chardonnay", "White", us$type2)
us$type2 <- ifelse(us$type == "Pinot Noir" | us$type == "Zinfadel" | us$type == "Syrah" | us$type == "Cabernet Sauvignon", "Red", us$type2)
us$type2 <- ifelse(us$type == "Rose" | us$type == "Sparkling" | us$type == "Dessert", "Sparkling", us$type2)
us$type2 <- ifelse(us$type == "Red Blend", "Red Blend", us$type2)
us$type2 <- ifelse(us$type == "White Blend", "White Blend", us$type2)

# remove remaining rows
us$province <- NULL
us$variety <- NULL
us$type <- NULL

# topic model description
#load required packages
library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(tidytext)
library(stringr)

#Convert data to dataframe
us = as.data.frame(us)

#Transform data into corpus
corpus = Corpus(VectorSource(us$description))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("the", "and", "bit", "way", "firm", "will", "still", "good", "giving", "now", "-", "end", "yet", "thats", "ready", "set", "feel", "time", "give", 
                                       "back", "just", "around", "lead", "many", "one", "adds", "long", "seems", "quite", "sip", "add", "next", "make", "also", "now-", "without", "-", stopwords("english")))
corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1])

#Document Matrix
dtm <- DocumentTermMatrix(corpus)
dtm

#Remove sparse terms on 0.99 significance level
dtm = removeSparseTerms(dtm, 0.99)
dtm

#Output to see each document (description) individual
inspect(dtm[1,1:20])

#Define frequent terms
freq_term= findFreqTerms(dtm, 1000)
freq_term

#Frequent terms word cloud
freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=20, colors=brewer.pal(1, "Dark2"))

#Document Matrix with TFIDF on 0.95% significance level
dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
dtm_tfidf = removeSparseTerms(dtm_tfidf, 0.95)
dtm_tfidf

#Define top most frequent terms (73 in total)
N <-20
terms <- findFreqTerms(dtm_tfidf, N)
terms

#Set M as matrix of DTM
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)
maxterms<-apply(dtm_tfidf, 1, which.max)
dtm_tfidf$dimnames$Terms[maxterms]

#Output to see each document (description) individual w/ TFIDF
inspect(dtm_tfidf[2,terms])

#Create new matrix with TFIDF scores
m2 <- as.matrix(dtm_tfidf)

#Remove columns that are not in the top "terms" list
m2[, which(colnames(m2) != "terms")]

#Remove non-descriptive columns
m2 <- as.data.frame(m2)
m2$wine <- NULL
m2$finish <- NULL
m2$like <- NULL
m2$cabernet <- NULL
m2$merlot <- NULL
m2$offers <- NULL
m2$style <- NULL
m2$years <- NULL
m2$blend <- NULL
m2$nose <- NULL
m2$red <- NULL
m2$sauvignon <- NULL
m2$pinot <- NULL
m2$drink <- NULL
m2$made <- NULL
m2$m2 <- NULL
m2$shows <- NULL
m2$vineyard <- NULL
m2$theres <- NULL
m2$white <- NULL
m2$balanced <- NULL
m2$balance <- NULL
m2$chardonnay <- NULL
m2$syrah <- NULL
m2$flavor <- NULL
m2$texture <- NULL
m2$well <- NULL
m2$notes <- NULL
m2$flavors <- NULL

# combining terms
m2$tannic <- ifelse(m2$tannic == 1 | m2$tannins == 1, 1, 0)
m2$tannins <- NULL

m2$fullbodied <- ifelse(m2$full == 1 | m2$fullbodied == 1, 1, 0)
m2$full <- NULL

m2$spicy <- ifelse(m2$spicy == 1 | m2$spice == 1, 1, 0)
m2$spice <- NULL

#Combine original US dataset with matrix
new_data = cbind(us,m2)
new_data$description <- NULL

##### Modeling #####
model <- lm(points ~ ., data = new_data)
summary(model)

model_df <- as.data.frame(summary(model)$coefficients)
model_df <- round(model_df, 3)
model_df <- subset(model_df, model_df$`Pr(>|t|)` < 0.05)

interaction_model <- lm(points ~ .^2, data = new_data)
summary(interaction_model)

interaction_df <- as.data.frame(summary(interaction_model)$coefficients)
interaction_df <- round(interaction_df, 3)
interaction_df <- subset(interaction_df, interaction_df$`Pr(>|t|)` < 0.05)