# erstmal für OZu500Flach und schauen wie das läuft
bb = c("OZu500HuegeligH","OZu500HuegeligP","OZu500HuegeligW","OZüber500FlachH","OZüber500FlachP","OZüber500FlachW")
rm(list=bb, bb)

# Schritt 1 Daten bereinigen, vorallem NAs sowie E_WEG_GUELTIG 

OZu500FlachW = OZu500FlachW %>% 
  filter(E_WEG_GUELTIG =="WAHR")

Gender = OZu500FlachW %>% # Alles in einen Datensatz
  full_join(OZu500FlachH, by = "HHNR") %>% #fulljoin mit sortieren anhand der "Haushaltsnummer"
  full_join(OZu500FlachP, by = "HHNR") %>% 
  filter(E_PERS_GUELTIG =="WAHR") %>% # Personen mit ausschließlich gültigen wegen am Stichtag
  filter(E_WEG_GUELTIG =="WAHR") %>% # Gültiger Weg (Angaben zu Dauer und Länge vorhanden, Länge < 100 km)
  select(HHNR,E_HVM,E_HVM_5,E_HVM_4, V_GESCHLECHT,E_GESCHLECHT_3,E_QZG_17) %>%  # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl
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
rm(Gender)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# Format der Tabellen immer table35 für 3er Einteilung Geschlecht und 5er Einteilung HVM usw
# in tables steht a für Arbeit und b für Bildung

# ==== 2a Wohnen Arbeit // Arbeit Wohnen -- geschlecht in 3er einteilung und HVM in 5er
table35a = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(1,8)], gender$E_HVM_5[gender$E_QZG_17 %in% c(1,8)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table35a) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(1, 8)) %>%
  count(Geschlecht_3, VM_5) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM_5)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Fahrrad"                  = "brown4",
    "MIV Fahrer/-in"           = "forestgreen",
    "MIV Mitfahrer/-in"        = "skyblue",
    "ÖV"                       = "orange",
    "Zu Fuß"                   = "pink2"
  )) +
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Arbeit & Arbeit - Wohnen",
    caption  = "SrV 2023"
  )

# ==== 2b Wohnen Arbeit // Arbeit Wohnen geschlecht in 3er einteilung und HVM in 4er
table34a = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(1,8)], gender$E_HVM_4[gender$E_QZG_17 %in% c(1,8)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table34a) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(1, 8)) %>%
  count(Geschlecht_3, VM_4) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM_4)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Fahrrad"                  = "brown4",
    "MIV"                      = "forestgreen",
    "ÖV"                       = "orange",
    "Zu Fuß"                   = "pink2"
  )) +
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Arbeit & Arbeit - Wohnen",
    caption  = "SrV 2023"
  )
# ==== 2c Wohnen Arbeit // Arbeit Wohnen geschlecht in 3er einteilung und HVM keiner
table30a = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(1,8)], gender$E_HVM[gender$E_QZG_17 %in% c(1,8)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table30a) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(1, 8)) %>%
  count(Geschlecht_3, VM) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Zu Fuß"                                       = "pink2",
    "Fahrrad (konventionell)"                      = "brown4",
    "Elektrofahrrad"                               = "firebrick3",
    "Leihfahrrad"                                  = "indianred3",
    "Elektro-Leihfahrrad"                          = "indianred1",
    "Lastenfahrrad"                                = "salmon3",
    "Elektro-Lastenfahrrad"                        = "salmon1",
    "Elektrotretroller (E-Scooter)"                = "mediumpurple1",
    "Moped/Motorrad/Motorroller"                   = "purple4",
    "Elektro-Moped/Motorrad/Motorroller"           = "mediumorchid3",
    "Pkw als Fahrer/-in im Haushalts-Pkw"          = "darkgreen",
    "Pkw als Fahrer/-in im Carsharing-Pkw"         = "forestgreen",
    "Pkw als Fahrer/-in im anderen Pkw"            = "olivedrab3",
    "Pkw als Mitfahrer/-in im Haushalts-Pkw"       = "skyblue4",
    "Pkw als Mitfahrer/-in im Carsharing-Pkw"      = "skyblue3",
    "Pkw als Mitfahrer/-in im anderen Pkw"         = "skyblue1",
    "Bus"                                          = "orange4",
    "Straßenbahn/Tram"                             = "orange3",
    "U-Bahn"                                        = "orange2",
    "S-Bahn"                                        = "orange1",
    "Nahverkehrszug"                               = "darkorange",
    "Fernverkehrszug"                              = "goldenrod3",
    "Taxi"                                         = "gold2",
    "Fernbus"                                      = "khaki3",
    "Anderes Verkehrsmittel"                       = "grey60"
  ))+
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Arbeit & Arbeit - Wohnen",
    caption  = "SrV 2023"
  )

# ==== 2d Wohnen - Bildung // Bildung - Wohnen -- geschlecht in 3er einteilung und HVM in 5er
table35b = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(3,10)], gender$E_HVM_5[gender$E_QZG_17 %in% c(3,10)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table35b) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(3, 10)) %>%
  count(Geschlecht_3, VM_5) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM_5)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Fahrrad"                  = "brown4",
    "MIV Fahrer/-in"           = "forestgreen",
    "MIV Mitfahrer/-in"        = "skyblue",
    "ÖV"                       = "orange",
    "Zu Fuß"                   = "pink2"
  )) +
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Bildung & Bildung - Wohnen",
    caption  = "SrV 2023"
  )

# ==== 2e Wohnen Arbeit // Arbeit Wohnen geschlecht in 3er einteilung und HVM in 4er
table34b = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(3,10)], gender$E_HVM_4[gender$E_QZG_17 %in% c(3,10)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table34b) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(3, 10)) %>%
  count(Geschlecht_3, VM_4) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM_4)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Fahrrad"                  = "brown4",
    "MIV"                      = "forestgreen",
    "ÖV"                       = "orange",
    "Zu Fuß"                   = "pink2"
  )) +
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Bildung & Bildung - Wohnen",
    caption  = "SrV 2023"
  )
# ==== 2f Wohnen Arbeit // Arbeit Wohnen geschlecht in 3er einteilung und HVM keiner
table30b = table(gender$E_GESCHLECHT_3[gender$E_QZG_17 %in% c(3,10)], gender$E_HVM[gender$E_QZG_17 %in% c(3,10)]) # subsetting und erstellen einer verwertbaren Tabelle
assocstats(table30b) # Berechnung verschiener Zusammenhangsmaße

gender %>%
  filter(E_QZG_17 %in% c(3, 10)) %>%
  count(Geschlecht_3, VM) %>%
  group_by(Geschlecht_3) %>%
  mutate(anteil = n / sum(n)) %>%
  ggplot(aes(x = Geschlecht_3, y = anteil, fill = VM)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Zu Fuß"                                       = "pink2",
    "Fahrrad (konventionell)"                      = "brown4",
    "Elektrofahrrad"                               = "firebrick3",
    "Leihfahrrad"                                  = "indianred3",
    "Elektro-Leihfahrrad"                          = "indianred1",
    "Lastenfahrrad"                                = "salmon3",
    "Elektro-Lastenfahrrad"                        = "salmon1",
    "Elektrotretroller (E-Scooter)"                = "mediumpurple1",
    "Moped/Motorrad/Motorroller"                   = "purple4",
    "Elektro-Moped/Motorrad/Motorroller"           = "mediumorchid3",
    "Pkw als Fahrer/-in im Haushalts-Pkw"          = "darkgreen",
    "Pkw als Fahrer/-in im Carsharing-Pkw"         = "forestgreen",
    "Pkw als Fahrer/-in im anderen Pkw"            = "olivedrab3",
    "Pkw als Mitfahrer/-in im Haushalts-Pkw"       = "skyblue4",
    "Pkw als Mitfahrer/-in im Carsharing-Pkw"      = "skyblue3",
    "Pkw als Mitfahrer/-in im anderen Pkw"         = "skyblue1",
    "Bus"                                          = "orange4",
    "Straßenbahn/Tram"                             = "orange3",
    "U-Bahn"                                        = "orange2",
    "S-Bahn"                                        = "orange1",
    "Nahverkehrszug"                               = "darkorange",
    "Fernverkehrszug"                              = "goldenrod3",
    "Taxi"                                         = "gold2",
    "Fernbus"                                      = "khaki3",
    "Anderes Verkehrsmittel"                       = "grey60"
  ))+
  labs(x = "Geschlecht", y = "Anteil", fill = "Hauptverkehrsmittel") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Hauptverkehrsmittel nach Geschlecht",
    subtitle = "QZG: Wohnen - Bildung & Bildung - Wohnen",
    caption  = "SrV 2023"
  )


# interpretation corner
# Wohnen Arbeit // Arbeit Wohnen
assocstats(table35a)
assocstats(table34a)
assocstats(table30a)

# Wohnen Arbeit // Arbeit Wohnen
assocstats(table35b)
assocstats(table34b)
assocstats(table30b)

# In beiden Fällen ist es spannend zu beobachten Wie Cramer's V und der Kontngenz koeffizient 
# sichtbar steigen sobald man die Einteilung der HVM in Gruppen weglässt
