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
  select(HHNR,E_HVM,E_HVM_5,E_HVM_4, V_GESCHLECHT,E_GESCHLECHT_3) # relevante Spalten zum Untersuchen des Zusammenhangs zwischen geschlecht und Verkerhsmittlwahl

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
  ))
  

gender = gender %>% # aufhübshen ist doch auch wichtig oder etwa nicht
  relocate(VM, .after = E_HVM) %>% 
  relocate(VM_5, .after = E_HVM_5) %>% 
  relocate(VM_4, .after = E_HVM_4) %>% 
  relocate(Geschlecht, .after = V_GESCHLECHT) %>% 
  relocate(Geschlecht_3, .after = E_GESCHLECHT_3) %>% 
  filter(VM_5 != "Berechnung nicht möglich")
rm(Gender)

# Schritt 2 Cramers V berechnen über Paket (vcd) pre loaded über WPAReader.R
# Format der Tabellen immer table35 für 3er Einteilung Geschlecht und 5er Einteilung HVM usw
table35 = table(gender$E_GESCHLECHT_3, gender$E_HVM_5)
assocstats(table35)

gender %>%
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
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

