# Datei basierend auf WPAReader.R --> zuerst einlesen!
# Der Quellcode in dieser Datei beschäftigt sich ausschließlich mit den Zusammenhangsmaßen zwischen
# HVM mit dem Geschlecht. Ergänzungen bitte direkt in den Aufgaben Tracker auf Notion schreiben.


# WIR SOLLTEN UNS GGF MIT KI GEDANKEN ZU ALTERNATIVER DARSTELLUNG NEBEN BALKENDIAGRAMMEN MACHEN

#==========
# HIER BITTE CMD+A // STRG+A DRÜCKEN
# abrufen über 
# View(plots_WA2)
#                 dann rechts auf die weiße box mit grünem Pfeil klicken
#                 in der Konsole Enter drücken

# oder z.B. plots_WA3[["Geschlecht_3 x VM_4"]] mit den entsprechenden Indizes

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

Gender = OZu500FlachW %>% # Alles in einen Datensatz
  full_join(OZu500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500FlachP, by = c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4, V_GESCHLECHT,E_GESCHLECHT_3,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

gender = Gender %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  )) %>% mutate(Geschlecht = case_when(
    V_GESCHLECHT == 1 ~ "Männlich",
    V_GESCHLECHT == 2 ~ "Weiblich",
    V_GESCHLECHT == 3 ~ "Divers",
    V_GESCHLECHT == 4 ~ "keine Angabe im Geburtenregister"
  )) %>% mutate(Geschlecht_3 = case_when(
    E_GESCHLECHT_3 == 1 ~ "Männlich",
    E_GESCHLECHT_3 == 2 ~ "Weiblich",
    E_GESCHLECHT_3 == 3 ~ "Divers/keine Angabe im Geburtenregister"
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  ))

gender = gender %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  relocate(Geschlecht, .after = V_GESCHLECHT) %>% 
  relocate(Geschlecht_3, .after = E_GESCHLECHT_3) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17)

gender = gender %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Gender)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
genderWA = gender[gender$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben
gender_vars = c("Geschlecht","Geschlecht_3") # Vektor mit allen Variablen Age
vm_vars = c("VM", "VM_4", "VM_5") # Vektor mit allen Variablen VM

tables_WA <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WA[[paste(a, v, sep = " x ")]] <- table(genderWA[[a]], genderWA[[v]])
  }
}

correlationWA = lapply(tables_WA, assocstats)



# === WB/BW 
genderWB = gender[gender$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

tables_WB <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WB[[paste(a, v, sep = " x ")]] <- table(genderWB[[a]], genderWB[[v]])
  }
}

correlationWB = lapply(tables_WB, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA = list()
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WA[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWA, aes(x = .data[[a]], fill = .data[[v]])) +
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
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WB[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWB, aes(x = .data[[a]], fill = .data[[v]])) +
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

Gender2 = OZu500HuegeligW %>% # Alles in einen Datensatz
  full_join(OZu500HuegeligH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500HuegeligP, c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4, V_GESCHLECHT,E_GESCHLECHT_3,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

gender2 = Gender2 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  )) %>% mutate(Geschlecht = case_when(
    V_GESCHLECHT == 1 ~ "Männlich",
    V_GESCHLECHT == 2 ~ "Weiblich",
    V_GESCHLECHT == 3 ~ "Divers",
    V_GESCHLECHT == 4 ~ "keine Angabe im Geburtenregister"
  )) %>% mutate(Geschlecht_3 = case_when(
    E_GESCHLECHT_3 == 1 ~ "Männlich",
    E_GESCHLECHT_3 == 2 ~ "Weiblich",
    E_GESCHLECHT_3 == 3 ~ "Divers/keine Angabe im Geburtenregister"
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  ))


gender2 = gender2 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  relocate(Geschlecht, .after = V_GESCHLECHT) %>% 
  relocate(Geschlecht_3, .after = E_GESCHLECHT_3) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17)

gender2 = gender2 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Gender2)

gender2 = gender2 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Gender2)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
genderWA2 = gender2[gender2$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

tables_WA2 <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WA2[[paste(a, v, sep = " x ")]] <- table(genderWA2[[a]], genderWA2[[v]])
  }
}

correlationWA2 = lapply(tables_WA2, assocstats)



# === WB/BW 
genderWB2 = gender2[gender2$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

tables_WB2 <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WB2[[paste(a, v, sep = " x ")]] <- table(genderWB2[[a]], genderWB2[[v]])
  }
}

correlationWB2 = lapply(tables_WB2, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA2 = list()
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WA2[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWA2, aes(x = .data[[a]], fill = .data[[v]])) +
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
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WB2[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWB2, aes(x = .data[[a]], fill = .data[[v]])) +
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

Gender3 = OZüber500FlachW %>% # Alles in einen Datensatz
  full_join(OZüber500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZüber500FlachP, c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4, V_GESCHLECHT,E_GESCHLECHT_3,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

gender3 = Gender3 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  )) %>% mutate(Geschlecht = case_when(
    V_GESCHLECHT == 1 ~ "Männlich",
    V_GESCHLECHT == 2 ~ "Weiblich",
    V_GESCHLECHT == 3 ~ "Divers",
    V_GESCHLECHT == 4 ~ "keine Angabe im Geburtenregister"
  )) %>% mutate(Geschlecht_3 = case_when(
    E_GESCHLECHT_3 == 1 ~ "Männlich",
    E_GESCHLECHT_3 == 2 ~ "Weiblich",
    E_GESCHLECHT_3 == 3 ~ "Divers/keine Angabe im Geburtenregister"
  )) %>% 
  mutate(QZG = case_when(
    E_QZG_17 == 1 ~ "Wohnen–Arbeiten (WA)",
    E_QZG_17 == 3 ~ "Wohnen–Bildung (WB)",
    E_QZG_17 == 8 ~ "Arbeiten–Wohnen (AW)",
    E_QZG_17 == 10 ~ "Bildung–Wohnen (BW)"
  ))


gender3 = gender3 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  relocate(Geschlecht, .after = V_GESCHLECHT) %>% 
  relocate(Geschlecht_3, .after = E_GESCHLECHT_3) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17)

gender3 = gender3 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))

rm(Gender3)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R


# === WA/AW
genderWA3 = gender3[gender3$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

tables_WA3 <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WA3[[paste(a, v, sep = " x ")]] <- table(genderWA3[[a]], genderWA3[[v]])
  }
}

correlationWA3 = lapply(tables_WA3, assocstats)



# === WB/BW 
genderWB3 = gender3[gender3$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

tables_WB3 <- list()
for (a in gender_vars) {
  for (v in vm_vars) {
    tables_WB3[[paste(a, v, sep = " x ")]] <- table(genderWB3[[a]], genderWB3[[v]])
  }
}

correlationWB3 = lapply(tables_WB3, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA3 = list()
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WA3[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWA3, aes(x = .data[[a]], fill = .data[[v]])) +
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
for (a in gender_vars) {
  for (v in vm_vars) {
    plots_WB3[[paste(a, v, sep = " x ")]] <-
      ggplot(genderWB3, aes(x = .data[[a]], fill = .data[[v]])) +
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















