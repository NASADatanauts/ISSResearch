library(tm)
library(tidytext)
library(dplyr)
library(tesseract)
library(tidyverse)

#########Do Not Run############################

files <- list.files(path = "/Users/dmeza/Box Sync/Datanauts/ISSpubs", pattern = "pdf", full.names = TRUE)

lapply(files, function(i){
    # convert pdf to ppm (an image format), just pages 1-10 of the PDF
    # but you can change that easily, just remove or edit the 
    # -f 1 -l 10 bit in the line below
    pngfile <- pdftools::pdf_convert(i, dpi = 600)
    # convert ppm to tif ready for tesseract
    text <- tesseract::ocr(pngfile)
    # write as text file
    write.csv(text, paste0(i, ".txt" ))
    # delete tif file
    file.remove(paste0(i, ".png" ))
})

###########################################

# Get list of files in folder
files <- list.files(path = "/cloud/project/data/ISSPubs_txt", pattern = "txt", full.names = TRUE)
Rpdf <- readPDF(engine = "pdftools")

ISSPubs_corpus <- Corpus(URISource(files), readerControl = list(reader = readPlain))
ISSPubs_corpus_tidy <- tidy(ISSPubs_corpus)

ISSPubs_dtm <- DocumentTermMatrix(ISSPubs_corpus, control = list(removePunctuation = TRUE, stopwords = TRUE, removeNumbers = TRUE))
ISSPubs_dtm <- tidy(ISSPubs_dtm)

total_by_term <- ISSPubs_dtm %>% 
    group_by(term) %>% 
    summarize(total = sum(count))

total_words <- ISSPubs_dtm %>% 
    group_by(document) %>% 
    summarize(total = sum(count))

ISSPubs_words <- left_join(ISSPubs_dtm, total_words)
ISSPubs_words$document <- factor(ISSPubs_words$document)

ISSPubs_tf_idf <- ISSPubs_dtm %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))

ISSPubs_tf_idf$document <- factor(ISSPubs_tf_idf$document)