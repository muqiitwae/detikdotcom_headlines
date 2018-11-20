####################################
#TEXT MINING DETIK HEADLINES (Imitated from https://github.com/walkerag/bbc_headlines)
#Name: Andriansyah Muqiit (211709554@stis.ac.id)
#Date: November 2018
#Purpose: Analyze scraped Detik headline data using tidy text mining approach
####################################

library(tidytext)
library(ggplot2)
library(purrr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(scales)
library(igraph)
library(ggraph)
library(widyr)
library(broom)
library(grid)
library(gridExtra)
library(gtable)



# Read Raw File
detik <- readxl::read_excel("C:/Users/Muqiit/Documents/Detik Headlines/Data/Detik 2018-01.xlsx")
 

# meng-kategorikan berita
detik[grepl("berita-ekonomi-bisnis",detik$X__3),"kategori"]<-"ekonomi bisnis"
detik[grepl("ekonomi-bisnis",detik$X__3),"kategori"]<-"ekonomi bisnis"
detik[grepl("energi",detik$X__3),"kategori"]<-"energi"
detik[grepl("sosok",detik$X__3),"kategori"]<-"sosok"
detik[grepl("properti",detik$X__3),"kategori"]<-"properti"
detik[grepl("infrastruktur",detik$X__3),"kategori"]<-"infrastruktur"
detik[grepl("moneter",detik$X__3),"kategori"]<-"moneter"
detik[grepl("infografis",detik$X__3),"kategori"]<-"infografis"
detik[grepl("bursa",detik$X__3),"kategori"]<-"bursa"
detik[grepl("market-research",detik$X__3),"kategori"]<-"market research"
detik[grepl("industri",detik$X__3),"kategori"]<-"industri"
detik[grepl("lowongan-kerja",detik$X__3),"kategori"]<-"loker"
detik[grepl("detiktv",detik$X__3),"kategori"]<-"detik tv"
detik[grepl("perencanaan-keuangan",detik$X__3),"kategori"]<-"perencanaan keuangan"
detik[grepl("wawancara-khusus",detik$X__3),"kategori"]<-"wawancara khusus"
detik[grepl("solusiukm",detik$X__3),"kategori"]<-"solusi ukm"

# menghilangkan missing value
detik <- drop_na(detik)
names(detik) <- c("Exported data","Judul","Tanggal","Link","Kategori")

# kategori berita terbanyak
ggplot(detik, aes(x=Kategori)) + geom_bar()

# mengubah ke tidy format
text_detik<-detik %>%
  unnest_tokens(input=Judul, word,to_lower=FALSE,drop=FALSE)
text_detik$word<-tolower(text_detik$word)

# mengidentifikasi stop words, word yang tidak berguna
path <- 'C:/Users/Muqiit/Documents/Detik Headlines/' #dapat diganti sesuai direktori file
stop_words_id <- read.table(file = paste0(path, "id.stopwords.02.01.2016.txt"))
names(stop_words_id)<-"word"
detik_stop_words <- text_detik %>%
  inner_join(stop_words_id) %>%
  group_by(word) %>% summarise(count=n()) %>% arrange(desc(count))

# menghilangkan stop words
text_detik <- text_detik %>%
  anti_join(stop_words_id)

# Detik top words count
text_detik %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="forestgreen") +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Detik Top Words") +
  theme(text = element_text(size = 20,family="Calibri")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) +
  ylab("Word Count")

# Detik top ten words 
counts<-
  text_detik %>%
  group_by(word) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
colnames(counts)<-c("Word","Count")
g <-tableGrob(counts[1:10,],rows=NULL,theme=ttheme_default(
  base_colour = "darkcyan"
  ,core = list(fg_params=list(fontsize=22)
               ,bg_params=list(fill="white"))
  
  ,colhead = list(bg_params=list(fill="white")
                  ,fg_params=list(fontsize=24))
  
  ,rowhead = list(fg_params=list(fontsize=20)
                  ,bg_params=list(col="black"))))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=1, l = 2, r = ncol(g),b=nrow(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=3, l = 1, r = ncol(g),b=nrow(g)-1)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=4, l = 1, r = ncol(g),b=nrow(g)-2)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=5, l = 1, r = ncol(g),b=nrow(g)-3)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=6, l = 1, r = ncol(g),b=nrow(g)-4)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=6, l = 1, r = ncol(g),b=nrow(g)-5)
grid.draw(g)

#Save formatted data
path <- 'C:/Users/Muqiit/Documents/Detik Headlines/Data/'
saveRDS(text_detik,file = paste0(path,"text_detik.rds"))
