# Datei basierend auf WPAReader.R --> zuerst einlesen!

# DER ZENSUS ZUR BEREINIGUNG WIRD HIER BERÜCKSICHTIGT

# Der Quellcode in dieser Datei beschäftigt sich ausschließlich mit den Zusammenhangsmaßen zwischen
# HVM mit dem Alter. Ergänzungen bitte direkt in den Aufgaben Tracker auf Notion schreiben.


# WIR SOLLTEN UNS GGF MIT KI GEDANKEN ZU ALTERNATIVER DARSTELLUNG NEBEN BALKENDIAGRAMMEN MACHEN

#==========
# HIER BITTE CMD+A // STRG+A DRÜCKEN
# abrufen über 
# View(plots_WA2)
#                 dann rechts auf die weiße box mit grünem Pfeil klicken
#                 in der Konsole Enter drücken

# oder z.B. plots_WB3[["V_ALTER x VM_4"]] mit den entsprechenden Indizes

# selber Spaß bei correlation !!


# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# ohne Index -->  u500Flach
# 2 -->           u500Huegelig
# 3 -->           ü500Flach













#==========
# Schritt 1 Daten bereinigen, vorallem NAs sowie E_WEG_GUELTIG 

OZu500FlachW = OZu500FlachW %>% 
  filter(E_WEG_GUELTIG =="WAHR")

Age = OZu500FlachW %>% # Alles in einen Datensatz
  full_join(OZu500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500FlachP, by = c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(HHNR,E_HVM,E_HVM_5,E_HVM_4,V_ALTER, E_ALTER_3, E_ALTER_4, E_ALTER_5, E_ALTER_7,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

age = Age %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
  mutate(VM= case_when(
    E_HVM == 1 ~ "Zu Fuß",
    E_HVM == 2 ~ "Fahrrad (konventionell)",
    E_HVM == 3 ~ "Elektrofahrrad",
    E_HVM == 4 ~ "Leihfahrrad",
    E_HVM == 5 ~ "Elektroleihfahrrad",
    E_HVM == 6 ~ "Lastenfahrrad",
    E_HVM == 7 ~ "Elektro-Lastenfahrrad",
    E_HVM == 8 ~ "Elektrotretroller (E-Scooter)",
    E_HVM == 9 ~ "Moped/Motorrad/Motorroller",
    E_HVM == 10 ~ "Elektro-Moped/Motorrad/Motorroller",
    E_HVM == 11 ~ "Pkw als Fahrer/-in im Haushalts-Pkw",
    E_HVM == 12 ~ "Pkw als Fahrer/-in im Carsharing-Pkw",
    E_HVM == 13 ~ "Pkw als Fahrer/-in im anderen Pkw",
    E_HVM == 14 ~ "Pkw als Mitfahrer/-in im Haushalts-Pkw",
    E_HVM == 15 ~ "Pkw als Mitfahrer/-in im Carsharing-Pkw",
    E_HVM == 16 ~ "Pkw als Mitfahrer/-in im anderen Pkw",
    E_HVM == 17 ~ "Bus",
    E_HVM == 18 ~ "Straßenbahn/Tram",
    E_HVM == 19 ~ "U-Bahn",
    E_HVM == 20 ~ "S-Bahn",
    E_HVM == 21 ~ "Nahverkehrszug",
    E_HVM == 22 ~ "Fernverkehrszug",
    E_HVM == 23 ~ "Taxi",
    E_HVM == 24 ~ "Fernbus",
    E_HVM == 70 ~ "Anderes Verkehrsmittel",
    E_HVM == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  filter(E_HVM != -7) %>% 
  mutate(VM_5 = case_when(
    E_HVM_5 == 1 ~ "Zu Fuß",
    E_HVM_5 == 2 ~ "Fahrrad",
    E_HVM_5 == 3 ~ "MIV Fahrer/-in",
    E_HVM_5 == 4 ~ "MIV Mitfahrer/-in",
    E_HVM_5 == 5 ~ "ÖV",
    E_HVM_5 == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  mutate(VM_4 = case_when(
    E_HVM_4 == 1 ~ "Zu Fuß",
    E_HVM_4 == 2 ~ "Fahrrad",
    E_HVM_4 == 3 ~ "MIV",
    E_HVM_4 == 4 ~ "ÖV",
    E_HVM_4 == -10 ~ "Unplausibel",
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  )) %>% 
  mutate(Alter_3 = case_when(
    E_ALTER_3 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_3 == 2 ~ "18 bis 64 Jahre",
    E_ALTER_3 == 3 ~ "65 Jahre und älter"
  )) %>% 
  mutate(Alter_4 = case_when(
    E_ALTER_4 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_4 == 2 ~ "18 bis 35 Jahre",
    E_ALTER_4 == 3 ~ "36 bis 60 Jahre",
    E_ALTER_4 == 4 ~ "61 Jahre und älter"
  )) %>% 
  mutate(Alter_5 = case_when(
    E_ALTER_5 == 1 ~ "0 bis 14 Jahre",
    E_ALTER_5 == 2 ~ "15 bis 24 Jahre",
    E_ALTER_5 == 3 ~ "25 bis 44 Jahre",
    E_ALTER_5 == 4 ~ "45 bis 64 Jahre",
    E_ALTER_5 == 5 ~ "65 Jahre und älter",
  )) %>% 
  mutate(Alter_7 = case_when(
    E_ALTER_7 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_7 == 2 ~ "18 bis 25 Jahre",
    E_ALTER_7 == 3 ~ "26 bis 35 Jahre",
    E_ALTER_7 == 4 ~ "36 bis 50 Jahre",
    E_ALTER_7 == 5 ~ "51 bis 60 Jahre",
    E_ALTER_7 == 6 ~ "61 bis 70 Jahre",
    E_ALTER_7 == 7 ~ "71 Jahre und älter",
  ))

age = age %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Alter_3, .after = E_ALTER_3) %>% 
  relocate(Alter_4, .after = E_ALTER_4) %>% 
  relocate(Alter_5, .after = E_ALTER_5) %>% 
  relocate(Alter_7, .after = E_ALTER_7)

age = age %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Age)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
ageWA = age[age$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben
alter_vars = c("V_ALTER", "Alter_3", "Alter_4", "Alter_5", "Alter_7") # Vektor mit allen Variablen Age
vm_vars = c("VM", "VM_4", "VM_5") # Vektor mit allen Variablen VM

gewicht = ageWA %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA[[key]] <- xtabs(f, data = ageWA)
  }
}
correlationWA <- lapply(tables_WA, assocstats)



# === WB/BW 
ageWB = age[age$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = ageWB %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA[[key]] <- xtabs(f, data = ageWB)
  }
}
correlationWB <- lapply(tables_WA, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WA[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWA, aes(x = .data[[a]], fill = .data[[v]],
                        weight = .data[["GEWICHT_HH_ZENSUS"]])) + # Gewicht rein
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Arbeit & Arbeit – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}


plots_WB = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WB[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWB, aes(x = .data[[a]], fill = .data[[v]],
                    weight = .data[["GEWICHT_HH_ZENSUS"]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Bildung & Bildung – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}



# Schritt 1 Daten bereinigen, vorallem NAs sowie E_WEG_GUELTIG 

OZu500HuegeligW = OZu500HuegeligW %>% 
  filter(E_WEG_GUELTIG =="WAHR")

Age2 = OZu500HuegeligW %>% # Alles in einen Datensatz
  full_join(OZu500HuegeligH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500HuegeligP, c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(HHNR,E_HVM,E_HVM_5,E_HVM_4,V_ALTER, E_ALTER_3, E_ALTER_4, E_ALTER_5, E_ALTER_7,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

age2 = Age2 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
  mutate(VM= case_when(
    E_HVM == 1 ~ "Zu Fuß",
    E_HVM == 2 ~ "Fahrrad (konventionell)",
    E_HVM == 3 ~ "Elektrofahrrad",
    E_HVM == 4 ~ "Leihfahrrad",
    E_HVM == 5 ~ "Elektroleihfahrrad",
    E_HVM == 6 ~ "Lastenfahrrad",
    E_HVM == 7 ~ "Elektro-Lastenfahrrad",
    E_HVM == 8 ~ "Elektrotretroller (E-Scooter)",
    E_HVM == 9 ~ "Moped/Motorrad/Motorroller",
    E_HVM == 10 ~ "Elektro-Moped/Motorrad/Motorroller",
    E_HVM == 11 ~ "Pkw als Fahrer/-in im Haushalts-Pkw",
    E_HVM == 12 ~ "Pkw als Fahrer/-in im Carsharing-Pkw",
    E_HVM == 13 ~ "Pkw als Fahrer/-in im anderen Pkw",
    E_HVM == 14 ~ "Pkw als Mitfahrer/-in im Haushalts-Pkw",
    E_HVM == 15 ~ "Pkw als Mitfahrer/-in im Carsharing-Pkw",
    E_HVM == 16 ~ "Pkw als Mitfahrer/-in im anderen Pkw",
    E_HVM == 17 ~ "Bus",
    E_HVM == 18 ~ "Straßenbahn/Tram",
    E_HVM == 19 ~ "U-Bahn",
    E_HVM == 20 ~ "S-Bahn",
    E_HVM == 21 ~ "Nahverkehrszug",
    E_HVM == 22 ~ "Fernverkehrszug",
    E_HVM == 23 ~ "Taxi",
    E_HVM == 24 ~ "Fernbus",
    E_HVM == 70 ~ "Anderes Verkehrsmittel",
    E_HVM == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  filter(E_HVM != -7) %>% 
  mutate(VM_5 = case_when(
    E_HVM_5 == 1 ~ "Zu Fuß",
    E_HVM_5 == 2 ~ "Fahrrad",
    E_HVM_5 == 3 ~ "MIV Fahrer/-in",
    E_HVM_5 == 4 ~ "MIV Mitfahrer/-in",
    E_HVM_5 == 5 ~ "ÖV",
    E_HVM_5 == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  mutate(VM_4 = case_when(
    E_HVM_4 == 1 ~ "Zu Fuß",
    E_HVM_4 == 2 ~ "Fahrrad",
    E_HVM_4 == 3 ~ "MIV",
    E_HVM_4 == 4 ~ "ÖV",
    E_HVM_4 == -10 ~ "Unplausibel",
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  )) %>% 
  mutate(Alter_3 = case_when(
    E_ALTER_3 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_3 == 2 ~ "18 bis 64 Jahre",
    E_ALTER_3 == 3 ~ "65 Jahre und älter"
  )) %>% 
  mutate(Alter_4 = case_when(
    E_ALTER_4 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_4 == 2 ~ "18 bis 35 Jahre",
    E_ALTER_4 == 3 ~ "36 bis 60 Jahre",
    E_ALTER_4 == 4 ~ "61 Jahre und älter"
  )) %>% 
  mutate(Alter_5 = case_when(
    E_ALTER_5 == 1 ~ "0 bis 14 Jahre",
    E_ALTER_5 == 2 ~ "15 bis 24 Jahre",
    E_ALTER_5 == 3 ~ "25 bis 44 Jahre",
    E_ALTER_5 == 4 ~ "45 bis 64 Jahre",
    E_ALTER_5 == 5 ~ "65 Jahre und älter",
  )) %>% 
  mutate(Alter_7 = case_when(
    E_ALTER_7 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_7 == 2 ~ "18 bis 25 Jahre",
    E_ALTER_7 == 3 ~ "26 bis 35 Jahre",
    E_ALTER_7 == 4 ~ "36 bis 50 Jahre",
    E_ALTER_7 == 5 ~ "51 bis 60 Jahre",
    E_ALTER_7 == 6 ~ "61 bis 70 Jahre",
    E_ALTER_7 == 7 ~ "71 Jahre und älter",
  ))

age2 = age2 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Alter_3, .after = E_ALTER_3) %>% 
  relocate(Alter_4, .after = E_ALTER_4) %>% 
  relocate(Alter_5, .after = E_ALTER_5) %>% 
  relocate(Alter_7, .after = E_ALTER_7)

age2 = age2 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Age2)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
ageWA2 = age2[age2$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = ageWA2 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA2 <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA2[[key]] <- xtabs(f, data = ageWA2)
  }
}
correlationWA2 <- lapply(tables_WA, assocstats)



# === WB/BW 
ageWB2 = age2[age2$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = ageWB2 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB2 <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WB2[[key]] <- xtabs(f, data = ageWB2)
  }
}
correlationWB2 <- lapply(tables_WA, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA2 = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WA2[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWA2, aes(x = .data[[a]], fill = .data[[v]],
    weight = .data[["GEWICHT_HH_ZENSUS"]]))+
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Arbeit & Arbeit – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}


plots_WB2 = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WB2[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWB2, aes(x = .data[[a]], fill = .data[[v]],
                         weight = .data[["GEWICHT_HH_ZENSUS"]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Bildung & Bildung – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}

# Schritt 1 Daten bereinigen, vorallem NAs sowie E_WEG_GUELTIG 

OZüber500FlachW = OZüber500FlachW %>% 
  filter(E_WEG_GUELTIG =="WAHR")

Age3 = OZüber500FlachW %>% # Alles in einen Datensatz
  full_join(OZüber500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZüber500FlachP, c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(HHNR,E_HVM,E_HVM_5,E_HVM_4,V_ALTER, E_ALTER_3, E_ALTER_4, E_ALTER_5, E_ALTER_7,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

age3 =  Age3 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
  mutate(VM= case_when(
    E_HVM == 1 ~ "Zu Fuß",
    E_HVM == 2 ~ "Fahrrad (konventionell)",
    E_HVM == 3 ~ "Elektrofahrrad",
    E_HVM == 4 ~ "Leihfahrrad",
    E_HVM == 5 ~ "Elektroleihfahrrad",
    E_HVM == 6 ~ "Lastenfahrrad",
    E_HVM == 7 ~ "Elektro-Lastenfahrrad",
    E_HVM == 8 ~ "Elektrotretroller (E-Scooter)",
    E_HVM == 9 ~ "Moped/Motorrad/Motorroller",
    E_HVM == 10 ~ "Elektro-Moped/Motorrad/Motorroller",
    E_HVM == 11 ~ "Pkw als Fahrer/-in im Haushalts-Pkw",
    E_HVM == 12 ~ "Pkw als Fahrer/-in im Carsharing-Pkw",
    E_HVM == 13 ~ "Pkw als Fahrer/-in im anderen Pkw",
    E_HVM == 14 ~ "Pkw als Mitfahrer/-in im Haushalts-Pkw",
    E_HVM == 15 ~ "Pkw als Mitfahrer/-in im Carsharing-Pkw",
    E_HVM == 16 ~ "Pkw als Mitfahrer/-in im anderen Pkw",
    E_HVM == 17 ~ "Bus",
    E_HVM == 18 ~ "Straßenbahn/Tram",
    E_HVM == 19 ~ "U-Bahn",
    E_HVM == 20 ~ "S-Bahn",
    E_HVM == 21 ~ "Nahverkehrszug",
    E_HVM == 22 ~ "Fernverkehrszug",
    E_HVM == 23 ~ "Taxi",
    E_HVM == 24 ~ "Fernbus",
    E_HVM == 70 ~ "Anderes Verkehrsmittel",
    E_HVM == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  filter(E_HVM != -7) %>% 
  mutate(VM_5 = case_when(
    E_HVM_5 == 1 ~ "Zu Fuß",
    E_HVM_5 == 2 ~ "Fahrrad",
    E_HVM_5 == 3 ~ "MIV Fahrer/-in",
    E_HVM_5 == 4 ~ "MIV Mitfahrer/-in",
    E_HVM_5 == 5 ~ "ÖV",
    E_HVM_5 == -7 ~ "Berechnung nicht möglich",
  )) %>% 
  mutate(VM_4 = case_when(
    E_HVM_4 == 1 ~ "Zu Fuß",
    E_HVM_4 == 2 ~ "Fahrrad",
    E_HVM_4 == 3 ~ "MIV",
    E_HVM_4 == 4 ~ "ÖV",
    E_HVM_4 == -10 ~ "Unplausibel",
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  )) %>% 
  mutate(Alter_3 = case_when(
    E_ALTER_3 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_3 == 2 ~ "18 bis 64 Jahre",
    E_ALTER_3 == 3 ~ "65 Jahre und älter"
  )) %>% 
  mutate(Alter_4 = case_when(
    E_ALTER_4 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_4 == 2 ~ "18 bis 35 Jahre",
    E_ALTER_4 == 3 ~ "36 bis 60 Jahre",
    E_ALTER_4 == 4 ~ "61 Jahre und älter"
  )) %>% 
  mutate(Alter_5 = case_when(
    E_ALTER_5 == 1 ~ "0 bis 14 Jahre",
    E_ALTER_5 == 2 ~ "15 bis 24 Jahre",
    E_ALTER_5 == 3 ~ "25 bis 44 Jahre",
    E_ALTER_5 == 4 ~ "45 bis 64 Jahre",
    E_ALTER_5 == 5 ~ "65 Jahre und älter",
  )) %>% 
  mutate(Alter_7 = case_when(
    E_ALTER_7 == 1 ~ "0 bis 17 Jahre",
    E_ALTER_7 == 2 ~ "18 bis 25 Jahre",
    E_ALTER_7 == 3 ~ "26 bis 35 Jahre",
    E_ALTER_7 == 4 ~ "36 bis 50 Jahre",
    E_ALTER_7 == 5 ~ "51 bis 60 Jahre",
    E_ALTER_7 == 6 ~ "61 bis 70 Jahre",
    E_ALTER_7 == 7 ~ "71 Jahre und älter",
  ))

age3 = age3 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Alter_3, .after = E_ALTER_3) %>% 
  relocate(Alter_4, .after = E_ALTER_4) %>% 
  relocate(Alter_5, .after = E_ALTER_5) %>% 
  relocate(Alter_7, .after = E_ALTER_7)

age3 = age3 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Age3)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R


# === WA/AW
ageWA3 = age3[age3$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = ageWA3 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA3 <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA3[[key]] <- xtabs(f, data = ageWA3)
  }
}
correlationWA3 <- lapply(tables_WA, assocstats)



# === WB/BW 
ageWB3 = age3[age3$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = ageWB3 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB3 <- list()
for (a in alter_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WB3[[key]] <- xtabs(f, data = ageWB3)
  }
}
correlationWB3 <- lapply(tables_WA, assocstats)



# Schritt 3 Grafiken erzeugen
plots_WA3 = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WA3[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWA3, aes(x = .data[[a]], fill = .data[[v]],
                         weight = .data[["GEWICHT_HH_ZENSUS"]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Arbeit & Arbeit – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}



plots_WB3 = list()
for (a in alter_vars) {
  for (v in vm_vars) {
    plots_WB3[[paste(a, v, sep = " x ")]] <-
      ggplot(ageWB3, aes(x = .data[[a]], fill = .data[[v]],
                         weight = .data[["GEWICHT_HH_ZENSUS"]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = a, y = "Anteil", fill = v,
        title    = paste("Verkehrsmittel nach", a),
        subtitle = "QZG: Wohnen – Bildung & Bildung – Wohnen"
      ) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
}


rm(list = ls()[!grepl("^(plot|correlati)", ls(), ignore.case = TRUE)])















