# Biblioteki
install.packages("tm")
library(tm)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
install.packages("caret")
library(caret)
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(magrittr)

# Wczytanie danych
opinie <- read.csv2(file.choose(), encoding = "UTF-8")
#️ Szybki podgląd
head(opinie)
str(opinie)

# Czyszczenie tekstu
stopwords_pl <- c("się", "i", "że", "to", "jest", "na", "w", "z", "jak", 
                  "nie", "do", "ale", "o", "ten", "co", "czy", "tak", 
                  "dla", "po", "od", "mi", "go", "za", "już", "bo", "by")

opinie$czysty <- tolower(opinie$opinia)
opinie$czysty <- removePunctuation(opinie$czysty)
opinie$czysty <- gsub("-", " ", opinie$czysty)
opinie$czysty <- removeNumbers(opinie$czysty)
opinie$czysty <- removeWords(opinie$czysty, stopwords_pl)
opinie$czysty <- stripWhitespace(opinie$czysty)

# 🌥 Chmura słów
corpus <- Corpus(VectorSource(opinie$czysty))
dtm_full <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm_full))
wordcloud(names(freq), freq, max.words = 100, colors = brewer.pal(8, "Dark2"))

# ✂️ Podział danych (80/20)
set.seed(123)
indeksy <- sample(1:nrow(opinie), size = 0.8 * nrow(opinie))
train <- opinie[indeksy, ]
test <- opinie[-indeksy, ]

# DTM dla zbiorów
dtm_train <- DocumentTermMatrix(Corpus(VectorSource(train$czysty)))
dtm_test <- DocumentTermMatrix(Corpus(VectorSource(test$czysty)), 
                               control = list(dictionary = Terms(dtm_train)))

# Konwersja DTM do data.frame
train_matrix <- as.data.frame(as.matrix(dtm_train))
train_matrix$poleca <- factor(train$poleca, levels = c("No", "Yes"))

test_matrix <- as.data.frame(as.matrix(dtm_test))
test_labels <- factor(test$poleca, levels = c("No", "Yes"))

# Regresja logistyczna
model_glm <- glm(poleca ~ ., data = train_matrix, family = "binomial")

# Predykcja
prob <- predict(model_glm, newdata = test_matrix, type = "response")
pred <- ifelse(prob > 0.5, "Yes", "No") %>% factor(levels = c("No", "Yes"))

# Ocena modelu
confusionMatrix(pred, test_labels)

