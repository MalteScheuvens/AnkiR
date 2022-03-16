library(readr)
library(dplyr)
library(tidyverse)
library(stringr)

path <- file.path("~", "Downloads", "NextVocabSetTrans.csv")
file <- read_delim(path, delim = ";", na = "", col_types = "ccccccccccc")

#Entfernen von empty rows
fileTrans <- file %>%
  drop_na(VokabelIT)
  
#Entfernen von noch nicht formatierten Werten
fileTrans <- slice_head(fileTrans,n=which(str_detect(fileTrans$Lautschrift,"Stopwort")))

# Lautschrift trennen
fileTrans <- fileTrans %>% 
  # Unnötige Umbüche entfernen und leading-Leerzeichen entfernen
  mutate(VokabelIT=str_replace(VokabelIT, regex("\\n",dotall=TRUE)," ")) %>%
  mutate(VokabelDE=str_replace(VokabelDE, regex("^",multiline = TRUE),"")) %>%
  # Lautschriftzeile erstellen und aus VokabelIT löschen
  mutate(Lautschrift=str_extract(VokabelIT, regex("(?=\\[).*",dotall=TRUE))) %>%
  mutate(VokabelIT=str_replace(VokabelIT, regex("(?=\\[).*",dotall=TRUE),"")) %>%
  # Lautschrift Zeichen korrigieren
  mutate(Lautschrift=str_replace_all(Lautschrift, "®", "\\\\textltailn ")) %>%
  mutate(Lautschrift=str_replace_all(Lautschrift, "ù", "\\\\textturny ")) %>%
  mutate(Lautschrift=str_replace_all(Lautschrift, "’", "\"")) %>%
  mutate(Lautschrift=str_replace_all(Lautschrift, "'", "\"")) %>%
  mutate(Lautschrift=str_replace_all(Lautschrift, "\\[‚", "[\"")) %>% 
  mutate(Lautschrift=str_replace_all(Lautschrift, ";irr", "; irr")) %>%
  mutate(Lautschrift=paste("[latex]\\begin{IPA}\\tiny",Lautschrift,"\\end{IPA}[/latex]"))

# Leading Leerzeichen aus VokabelDE entfernen
fileTrans <- fileTrans %>% 
  mutate(VokabelDE=str_replace(VokabelDE, regex("^",multiline = TRUE),""))


# Beispielsätze formatieren
fileTrans <- fileTrans %>% 
  # ¡ mit " ersetzen und unnötige leading Leerzeichen entfernen
  mutate(SatzIT=str_replace_all(SatzIT,regex(" *¡ *",multiline = TRUE),"\"")) %>%
  mutate(SatzIT=str_replace_all(SatzIT, regex("$",multiline = TRUE),"\"")) %>%
  mutate(SatzDE=str_replace_all(SatzDE,regex(" *¡ *",multiline = TRUE),"\"")) %>%
  mutate(SatzDE=str_replace_all(SatzDE, regex("$",multiline = TRUE),"\""))
  
# Zusatzinfos formatieren
fileTrans <- fileTrans %>% 
  # Unnötige/Doppelte Leerzeichen und Æ mit leading/following Leerzeichen entfernen
  mutate(Zusatzinfo=str_replace_all(Zusatzinfo,regex(" +",multiline = TRUE)," ")) %>%
  mutate(Zusatzinfo=str_replace_all(Zusatzinfo,regex(" *Æ *",multiline = TRUE),""))

# Typ erstellen
fileTrans <- fileTrans %>% 
  # Vokabeltyp hinzufügen
  mutate(Typ=case_when(str_detect(Lautschrift, "\\] n") ~ paste("Nomen"),
                       str_detect(Lautschrift, "\\] v") ~ "Verb",
                       str_detect(Lautschrift, "\\] adj") ~ "Adjektiv",
                       str_detect(Lautschrift, "\\] phrase") ~ "Phrase")) %>%
  mutate(Typ=case_when(str_detect(Lautschrift, "; irr") ~ paste(Typ,"Irregulario",sep="::"),
                       TRUE ~ Typ))


#Tags erstellen
fileTrans <- fileTrans %>%
  # Unnötige/Doppelte Leerzeichen und Æ mit leading/following Leerzeichen entfernen
  mutate(Tags=paste(Level,Oberkategorie,Unterkategorie,Typ,sep="::"))

#NA entfernen
fileTrans <- fileTrans %>%
  mutate_all(str_replace_all, "NA", "")

#Unique ID erstellen
fileTrans <- fileTrans %>%
  # Unnötige/Doppelte Leerzeichen und Æ mit leading/following Leerzeichen entfernen
  mutate(ID=paste(VokabelIT,VokabelDE,sep="_")) %>%
  select(ID, everything())


path2 <- file.path("~", "Downloads", "VocabFormatResult.csv")
write.csv(fileTrans, path2, na = "",row.names = FALSE)