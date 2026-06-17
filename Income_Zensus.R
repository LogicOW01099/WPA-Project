# Datei basierend auf WPAReader.R --> zuerst einlesen!

# DER ZENSUS ZUR BEREINIGUNG WIRD HIER BERÜCKSICHTIGT

# Der Quellcode in dieser Datei beschäftigt sich ausschließlich mit den Zusammenhangsmaßen zwischen
# HVM mit dem Einkommen. Ergänzungen bitte direkt in den Aufgaben Tracker auf Notion schreiben.


# WIR SOLLTEN UNS GGF MIT KI GEDANKEN ZU ALTERNATIVER DARSTELLUNG NEBEN BALKENDIAGRAMMEN MACHEN

#==========
# HIER BITTE CMD+A // STRG+A DRÜCKEN
# abrufen über 
# View(plots_WA2)
#                 dann rechts auf die weiße box mit grünem Pfeil klicken
#                 in der Konsole Enter drücken

# oder z.B. plots_WB3[["V_EINK x VM_4"]] mit den entsprechenden Indizes

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

Income = OZu500FlachW %>% # Alles in einen Datensatz
  full_join(OZu500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500FlachP, by = c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4,V_EINK,E_EINK_5,E_OEK_STATUS,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

income = Income %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  mutate(Eink = case_when(
    V_EINK == 1 ~ "Unter 500 €",
    V_EINK == 2 ~ "500 bis unter 900 €",
    V_EINK == 3 ~ "900 bis unter 1.500 €",
    V_EINK == 4 ~ "1.500 bis unter 2.000 €",
    V_EINK == 5 ~ "2.000 bis unter 2.600 €",
    V_EINK == 6 ~ "2.600 bis unter 3.000 €",
    V_EINK == 7 ~ "3.000 bis unter 3.600 €",
    V_EINK == 8 ~ "3.600 bis unter 4.600 €",
    V_EINK == 9 ~ "4.600 bis unter 5.600 €",
    V_EINK == 10 ~ "5.600 bis unter 6.600 €",
    V_EINK == 11 ~ "6.600 € und mehr",
  )) %>% 
  mutate(Eink_5 = case_when(
    E_EINK_5 == 1 ~ "Unter 1.500 €",
    E_EINK_5 == 2 ~ "1.500 bis unter 2.600 €",
    E_EINK_5 == 3 ~ "2.600 bis unter 3.600",
    E_EINK_5 == 4~ "3.600 bis unter 5.600 €",
    E_EINK_5 == 5 ~ "5.600 € und mehr",
  )) %>% 
  mutate(ÖkStat = case_when(
    E_OEK_STATUS == 1 ~ "Sehr niedrig",
    E_OEK_STATUS == 2 ~ "Niedrig",
    E_OEK_STATUS == 3 ~ "Mittel",
    E_OEK_STATUS == 4 ~ "Hoch",
    E_OEK_STATUS == 5 ~ "Sehr hoch",
  ))

income=income %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Eink, .after = V_EINK) %>% 
  filter(!(V_EINK %in% c(-10, -9, -5))) %>% 
  relocate(Eink_5, .after = E_EINK_5) %>% 
  filter(!(E_EINK_5 %in% c(-10, -9, -5))) %>% 
  relocate(ÖkStat, .after = E_OEK_STATUS) %>% 
  filter(!(E_OEK_STATUS %in% c(-7)))
  

income = income %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Income)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
incomeWA = income[income$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben
income_vars = c("V_EINK","Eink_5","ÖkStat") # Vektor mit allen Variablen Age
vm_vars = c("VM", "VM_4", "VM_5") # Vektor mit allen Variablen VM

gewicht = incomeWA %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA[[key]] <- xtabs(f, data = incomeWA)
  }
}
correlationWA <- lapply(tables_WA, assocstats)

# === WB/BW 
incomeWB =income[income$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = incomeWB %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WB[[key]] <- xtabs(f, data = incomeWB)
  }
}
correlationWB <- lapply(tables_WB, assocstats)
# Schritt 3 Grafiken erzeugen
plots_WA = list()
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WA[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWA, aes(x = .data[[a]], fill = .data[[v]],
                           weight = .data[["GEWICHT_HH_ZENSUS"]])) + # gewicht rein
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
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WB[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWB, aes(x = .data[[a]], fill = .data[[v]],
                           weight = .data[["GEWICHT_HH_ZENSUS"]])) + # gewicht rein+
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

Income2 = OZu500HuegeligW %>% # Alles in einen Datensatz
  full_join(OZu500HuegeligH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500HuegeligP, by = c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4,V_EINK,E_EINK_5,E_OEK_STATUS,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

income2 = Income2 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  mutate(Eink = case_when(
    V_EINK == 1 ~ "Unter 500 €",
    V_EINK == 2 ~ "500 bis unter 900 €",
    V_EINK == 3 ~ "900 bis unter 1.500 €",
    V_EINK == 4 ~ "1.500 bis unter 2.000 €",
    V_EINK == 5 ~ "2.000 bis unter 2.600 €",
    V_EINK == 6 ~ "2.600 bis unter 3.000 €",
    V_EINK == 7 ~ "3.000 bis unter 3.600 €",
    V_EINK == 8 ~ "3.600 bis unter 4.600 €",
    V_EINK == 9 ~ "4.600 bis unter 5.600 €",
    V_EINK == 10 ~ "5.600 bis unter 6.600 €",
    V_EINK == 11 ~ "6.600 € und mehr",
  )) %>% 
  mutate(Eink_5 = case_when(
    E_EINK_5 == 1 ~ "Unter 1.500 €",
    E_EINK_5 == 2 ~ "1.500 bis unter 2.600 €",
    E_EINK_5 == 3 ~ "2.600 bis unter 3.600",
    E_EINK_5 == 4~ "3.600 bis unter 5.600 €",
    E_EINK_5 == 5 ~ "5.600 € und mehr",
  )) %>% 
  mutate(ÖkStat = case_when(
    E_OEK_STATUS == 1 ~ "Sehr niedrig",
    E_OEK_STATUS == 2 ~ "Niedrig",
    E_OEK_STATUS == 3 ~ "Mittel",
    E_OEK_STATUS == 4 ~ "Hoch",
    E_OEK_STATUS == 5 ~ "Sehr hoch",
  ))

income2=income2 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Eink, .after = V_EINK) %>% 
  filter(!(V_EINK %in% c(-10, -9, -5))) %>% 
  relocate(Eink_5, .after = E_EINK_5) %>% 
  filter(!(E_EINK_5 %in% c(-10, -9, -5))) %>% 
  relocate(ÖkStat, .after = E_OEK_STATUS) %>% 
  filter(!(E_OEK_STATUS %in% c(-7)))


income2 = income2 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Income2)


# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
incomeWA2 = income2[income2$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = incomeWA2 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA2 <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA2[[key]] <- xtabs(f, data = incomeWA2)
  }
}
correlationWA2 <- lapply(tables_WA2, assocstats)

# === WB/BW 
incomeWB2 =income2[income2$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = incomeWB2 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB2 <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WB2[[key]] <- xtabs(f, data = incomeWB2)
  }
}
correlationWB2 <- lapply(tables_WB2, assocstats)


# Schritt 3 Grafiken erzeugen
plots_WA2 = list()
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WA2[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWA2, aes(x = .data[[a]], fill = .data[[v]],
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


plots_WB2 = list()
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WB2[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWB, aes(x = .data[[a]], fill = .data[[v]],
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


Income3 = OZüber500FlachW %>% # Alles in einen Datensatz
  full_join(OZüber500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZüber500FlachP, by = c("HHNR","PNR")) %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(PNR, HHNR,E_HVM,E_HVM_5,E_HVM_4,V_EINK,E_EINK_5,E_OEK_STATUS,E_QZG_17,GEWICHT_HH_ZENSUS) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
  filter(E_QZG_17 %in% c(1, 3, 8, 10))

income3 = Income3 %>% # für die übersicht, will das gar nicht weiter kommentieren, ist auch bisschen vorbereiten fürs plotten
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
  mutate(Eink = case_when(
    V_EINK == 1 ~ "Unter 500 €",
    V_EINK == 2 ~ "500 bis unter 900 €",
    V_EINK == 3 ~ "900 bis unter 1.500 €",
    V_EINK == 4 ~ "1.500 bis unter 2.000 €",
    V_EINK == 5 ~ "2.000 bis unter 2.600 €",
    V_EINK == 6 ~ "2.600 bis unter 3.000 €",
    V_EINK == 7 ~ "3.000 bis unter 3.600 €",
    V_EINK == 8 ~ "3.600 bis unter 4.600 €",
    V_EINK == 9 ~ "4.600 bis unter 5.600 €",
    V_EINK == 10 ~ "5.600 bis unter 6.600 €",
    V_EINK == 11 ~ "6.600 € und mehr",
  )) %>% 
  mutate(Eink_5 = case_when(
    E_EINK_5 == 1 ~ "Unter 1.500 €",
    E_EINK_5 == 2 ~ "1.500 bis unter 2.600 €",
    E_EINK_5 == 3 ~ "2.600 bis unter 3.600",
    E_EINK_5 == 4~ "3.600 bis unter 5.600 €",
    E_EINK_5 == 5 ~ "5.600 € und mehr",
  )) %>% 
  mutate(ÖkStat = case_when(
    E_OEK_STATUS == 1 ~ "Sehr niedrig",
    E_OEK_STATUS == 2 ~ "Niedrig",
    E_OEK_STATUS == 3 ~ "Mittel",
    E_OEK_STATUS == 4 ~ "Hoch",
    E_OEK_STATUS == 5 ~ "Sehr hoch",
  ))

income3=income3 %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  filter(VM_5 != "Berechnung nicht möglich") %>% 
  relocate(QZG, .after = E_QZG_17) %>% 
  relocate(Eink, .after = V_EINK) %>% 
  filter(!(V_EINK %in% c(-10, -9, -5))) %>% 
  relocate(Eink_5, .after = E_EINK_5) %>% 
  filter(!(E_EINK_5 %in% c(-10, -9, -5))) %>% 
  relocate(ÖkStat, .after = E_OEK_STATUS) %>% 
  filter(!(E_OEK_STATUS %in% c(-7)))


income3 = income3 %>%
  filter(if_all(everything(), ~ is.na(.x) | .x != -7))
rm(Income3)


# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# zu chisq_tests von links nach rechts: X² (Likelihood Ratio); X² (Pearson); df (Likelihood Ratio); 
# df (Pearson);p-Wert (Likelihood Ratio);p-Wert (Pearson)

# === WA/AW
incomeWA3 = income3[income3$E_QZG_17 %in% c(1, 8), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = incomeWA3 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WA3 <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WA3[[key]] <- xtabs(f, data = incomeWA3)
  }
}
correlationWA3 <- lapply(tables_WA3, assocstats)

# === WB/BW 
incomeWB3 =income3[income3$E_QZG_17 %in% c(3, 10), ] # DF subsetten und so ausschließlich WA und AW drin haben

gewicht = incomeWB3 %>% 
  select(GEWICHT_HH_ZENSUS)

tables_WB3 <- list()
for (a in income_vars) {
  for (v in vm_vars) {
    key <- paste(a, v, sep = " x ")
    f   <- as.formula(paste(gewicht, "~", a, "+", v))
    tables_WB3[[key]] <- xtabs(f, data = incomeWB3)
  }
}
correlationWB3 <- lapply(tables_WB3, assocstats)



# Schritt 3 Grafiken erzeugen
plots_WA3 = list()
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WA3[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWA3, aes(x = .data[[a]], fill = .data[[v]],
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
for (a in income_vars) {
  for (v in vm_vars) {
    plots_WB3[[paste(a, v, sep = " x ")]] <-
      ggplot(incomeWB, aes(x = .data[[a]], fill = .data[[v]],
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
