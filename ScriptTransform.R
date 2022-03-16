library(readr)
library(dplyr)
library(tidyverse)
library(stringr)

pathLoad <- file.path("~", "Downloads", "Langenscheidt Grundwortschatz Italienisch", "Blatt 2-Tabelle 1.csv")
pathSave <- file.path("~", "Downloads", "UltimateVocabSetTrans.csv")

columns = c("VokabelIT","SatzIT","VokabelDE","SatzDE","Zusatzinfo","Oberkategorie","Unterkategorie","Level")
file <- read_delim(pathLoad, delim = ";", col_types = "cccccccc", col_names = columns, na = "")


fileTrans <- file %>% 
  # Zusatzinfo aus VocabIT herausfilter (Muster: Æ...)
  # SatzIT aus VokabelIT herausfiltern (Muster: ¡...)
  # SatzDE aus VokabelDE herausfiltern (Muster: ¡...)
  mutate(Zusatzinfo=case_when(is.na(str_extract(VokabelIT, "(?=Æ).*")) ~ Zusatzinfo,
                              TRUE ~ str_extract(VokabelIT, "(?=Æ).*"))) %>%
  mutate(SatzIT=case_when(is.na(str_extract(VokabelIT, "(?=¡).*")) ~ SatzIT,
                          TRUE ~ str_extract(VokabelIT, "(?=¡).*"))) %>%
  mutate(SatzDE=case_when(is.na(str_extract(VokabelDE, "(?=¡).*")) ~ SatzDE,
                          TRUE ~ str_extract(VokabelDE, "(?=¡).*"))) %>%
  # Gefilterte Sätze löschen
  mutate(VokabelIT=str_replace(VokabelIT, "(?=¡).*","")) %>%
  mutate(VokabelDE=str_replace(VokabelDE, "(?=¡).*","")) %>%
  mutate(VokabelIT=str_replace(VokabelIT, "(?=Æ).*",""))


fileTrans <- fileTrans %>%
  # Sprachen die über Leerzeichen getrennt sind herausfilter
  mutate(VokabelDE=case_when(is.na(str_extract(VokabelIT, "(?=\\s{6,}).*")) ~ VokabelDE,
                             TRUE ~ str_extract(VokabelIT, "(?=\\s{6,}).*"))) %>%
  mutate(SatzDE=case_when(is.na(str_extract(SatzIT, "(?=\\s{6,}).*")) ~ SatzDE,
                          TRUE ~ str_extract(SatzIT, "(?=\\s{6,}).*"))) %>%
  # Dublikate/Reste entfernen
  mutate(VokabelIT=str_replace(VokabelIT, "(?=\\s{6,}).*","")) %>%
  mutate(SatzIT=str_replace(SatzIT, "(?=\\s{6,}).*","")) %>%
  # Überflüssige Leerzeichen entfernen
  mutate(VokabelIT=str_replace(VokabelIT, "(\\s{3,})","")) %>%
  mutate(SatzIT=str_replace(SatzIT, "(\\s{3,})","")) %>%
  mutate(VokabelDE=str_replace(VokabelDE, "(\\s{3,})"," ")) %>%
  mutate(SatzDE=str_replace(SatzDE, "(\\s{3,})"," "))

fileTrans <- fileTrans %>%
  # Reste von Querverweisen auf andere Vokabeln entfernen (In VokabelIT und in Zusatzinfo), "irr" behalten
  mutate(VokabelIT=str_replace(VokabelIT, "(?=\\}).*","irr")) %>%
  mutate(Zusatzinfo=case_when(is.na(str_extract(Zusatzinfo, "(\\})")) ~ Zusatzinfo,
                              TRUE ~ ""))


fileTrans <- fileTrans %>% 
  #Filtern nach doppelten Umbrüchen, übertrag in SatzIT
  mutate(SatzIT=case_when(is.na(str_extract(VokabelIT, regex("(?=\\n{2,}).*",dotall=TRUE))) ~ SatzIT,
                          TRUE ~ paste(SatzIT,str_extract(VokabelIT, regex("(?=\\n{2,}).*",dotall=TRUE))))) %>%
  # Entfernen der doppelten Umbrüche in VokabelIT und SatzIT
  mutate(VokabelIT=str_replace(VokabelIT, regex("(?=\\n{2,}).*",dotall=TRUE),"")) %>%
  mutate(SatzIT=str_replace(SatzIT, regex("(\\n{2,})",dotall=TRUE),"")) %>%
  #Suche nach erstem Satzzeichen, wenn danach neuer Satz beginnt, Übertrag nach SatzDE, da Teil von deutschem Satz (wenn es ein weiterer IT Satz wäre, wäre umbruch dazwischen; zweiteilige IT Sätze müssen händisch gefiltert werden...)
  mutate(SatzDE=case_when(is.na(str_extract(SatzIT, "[\\.].*")) ~ SatzDE,
                          TRUE ~ paste(SatzDE,str_extract(SatzIT, "[\\.].*")))) %>%
  # Ersetzen des überflüssigen Satzes mit Punkt
  mutate(SatzIT=str_replace(SatzIT, "[\\.].*",".")) %>%
  # Entfernen der doppelten Punkte (durch übertrag von SatzIT) aus SatzDE
  mutate(SatzDE=str_replace(SatzDE, "[\\. ]{2}","")) %>%
  # Entfernen von Bindestrichen aus SatzDE
  mutate(SatzDE=str_replace(SatzDE, "[- ]{2}","")) %>%
  # Entfernen überflüssiger Umbrüche in VokabelIT/DE (oft Umbruch über Vokabel oder erster Umbruch unnötig (e.g. Lautschirft in neuer Zeile))
  mutate(VokabelIT=str_replace(VokabelIT, "\\n"," ")) %>%
  mutate(VokabelDE=str_replace(VokabelDE, "\\n"," ")) %>%
  
  #Erkennen einzelner Wörter in VokabelIT spalte (ohne Lautschrift, daher teil von SatzIT) und übertrag nach Satz IT; im Anschluss auch for VokabelDE und SatzDE auf Basis von Lautschrift IT.
  mutate(SatzIT=case_when(is.na(str_extract(VokabelIT, regex("\\[",dotall=TRUE))) ~ case_when(is.na(str_extract(VokabelIT, ".*")) ~ SatzIT, TRUE ~ paste(SatzIT,str_extract(VokabelIT, ".*"), sep="")),
                          TRUE ~ SatzIT)) %>%
  mutate(SatzDE=case_when(is.na(str_extract(VokabelIT, regex("\\[",dotall=TRUE))) ~
                            case_when(is.na(str_extract(VokabelDE, ".*")) ~ SatzDE, TRUE ~ paste(SatzDE,str_extract(VokabelDE, ".*"), sep="")),
                          TRUE ~ SatzDE)) %>%
  #Löschen der Übertragenen Werte
  mutate(VokabelDE=case_when(is.na(str_extract(VokabelIT, regex("\\[",dotall=TRUE))) ~ "",
                             TRUE ~ VokabelDE)) %>%
  mutate(VokabelIT=case_when(is.na(str_extract(VokabelIT, regex("\\[",dotall=TRUE))) ~ "",
                             TRUE ~ VokabelIT))



fileTrans <- fileTrans %>% 
  # SatzDE: Zweites Ausrufezeichen (in erster Zeile! -> (. ¡) ist Muster) ist italienischer Satz
  # (mit „\n“ einfügen für Zeilenumbruch in SatzDE)
  mutate(SatzIT=case_when(is.na(str_extract(SatzDE, "(?=\\. ¡).*")) ~ SatzIT,
                          TRUE ~ paste(SatzIT,"\n",str_extract(SatzDE, "(?=\\. ¡).*"), sep=""))) %>%
  # in SatzIT (. ¡) zu (.) und in SatzDE Muster-Satz mit (.) ersetzen)
  mutate(SatzDE=str_replace(SatzDE, "(?=\\. ¡).*",".")) %>%
  mutate(SatzIT=str_replace(SatzIT, "\\. ¡","¡")) %>%
  # VokabelDE: Zweite Zeile mit „¡“ ist deutscher Satz 
  # (mit „\n“ einfügen für Zeilenumbruch in SatzDE)
  mutate(SatzDE=case_when(is.na(str_extract(VokabelDE, regex("(?=¡).*",dotall=TRUE))) ~ SatzDE,
                          TRUE ~ paste(SatzDE,"\n",str_extract(VokabelDE, regex("(?=¡).*",dotall=TRUE)), sep=""))) %>%
  mutate(VokabelDE=str_replace(VokabelDE, regex("(?=\\n¡).*",dotall=TRUE),"")) %>%
  # Unnötige Umbrüche entfernen in SatzDE
  mutate(SatzDE=str_replace(SatzDE, regex("\\n(?=[:alpha:])",dotall=TRUE)," "))

write.csv(fileTrans, pathSave, na = "")
