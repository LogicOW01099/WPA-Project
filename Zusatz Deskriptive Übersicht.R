# Deskriptive Statistik zu Geschlecht
# Grundlage ist Datei Age_Zensus



Zahlen_gender1 = gender %>% 
  select(Geschlecht_3) %>% 
  count(Geschlecht_3)


Zahlen_gender2 = gender2 %>% 
  select(Geschlecht_3) %>% 
  count(Geschlecht_3)

Zahlen_gender3 = gender3 %>% 
  select(Geschlecht_3) %>% 
  count(Geschlecht_3)


rename_geschlecht <- function(df) {
  df %>%
    mutate(Geschlecht_3 = factor(
      Geschlecht_3,
      levels = c("Divers/keine Angabe im Geburtenregister", "Männlich", "Weiblich"),
      labels = c("Divers+", "Männlich", "Weiblich")
    ))
}

# Auf alle drei Data Frames anwenden
Zahlen_gender1 <- Zahlen_gender1 %>% rename_geschlecht()
Zahlen_gender2 <- Zahlen_gender2 %>% rename_geschlecht()
Zahlen_gender3 <- Zahlen_gender3 %>% rename_geschlecht()



Zahlen_gender = bind_rows(Zahlen_gender1, Zahlen_gender2, Zahlen_gender3) %>% 
  group_by(Geschlecht_3) %>% 
  summarise(n_sum_gender = sum(n))

# Relative Wsk anstatt absolut 
Zahlen_gender_rel = Zahlen_gender %>% 
  mutate(rel_gender = n_sum_gender /sum(n_sum_gender))



ggplot(Zahlen_gender_rel, aes(x = Geschlecht_3, y = rel_gender)) + 
  geom_col(fill = "darkblue", color = "white") +
  geom_text(
    aes(label = n_sum_gender),
    vjust = -0.5,
    size = 3.5,
    color = "black"
  ) +
  theme_minimal() 



# Deskriptive Statistik zum Alter
#Überlegung ein Histogram für Altersverteilung?

# ich nehme hier die 5er Einteilung und plane sie über die 3 Datensätze zu mitteln (falls sinnvoll)

Zahlen_alter1 = age %>% 
  select(Alter_5) %>% 
  count(Alter_5)


Zahlen_alter2 = age2 %>% 
  select(Alter_5)%>% 
  count(Alter_5)


Zahlen_alter3 = age3 %>% 
  select(Alter_5)%>% 
  count(Alter_5)

rename_alter <- function(df2) {
  df2 %>%
    mutate(Alter_5 = factor(
      Alter_5,
      levels = c("0 bis 14 Jahre", "15 bis 24 Jahre", "25 bis 44 Jahre", "45 bis 64 Jahre", "65 Jahre und älter"),
      labels = c("0-14 Jahre", "15-24 Jahre", "25-44 Jahre", "45-64 Jahre", "65+ Jahre ")
    ))
}

# Auf alle drei Data Frames anwenden
Zahlen_alter1 <- Zahlen_alter1 %>% rename_alter()
Zahlen_alter2 <- Zahlen_alter2 %>% rename_alter()
Zahlen_alter3 <- Zahlen_alter3 %>% rename_alter()


Zahlen_alter = bind_rows(Zahlen_alter1, Zahlen_alter2, Zahlen_alter3) %>% 
  group_by(Alter_5) %>% 
  summarise(n_sum_alter = sum(n)) 


# Relative Wsk anstatt absolut 
Zahlen_alter_rel = Zahlen_alter %>% 
  mutate(rel_alter = n_sum_alter /sum(n_sum_alter))


ggplot(Zahlen_alter_rel, aes(x = Alter_5, y = rel_alter)) +
  geom_col(fill = "darkblue", color = "white") +
  geom_text(
    aes(label = n_sum_alter),
    vjust = -0.5,
    size = 3.5,
    color = "black"
  ) +
  theme_minimal() 








# Deskriptive Statistik zum Einkommen

Zahlen_einkommen1 = income %>% 
  select(Eink_5) %>% 
  count(Eink_5)

Zahlen_einkommen2 = income2 %>% 
  select(Eink_5) %>% 
  count(Eink_5)

Zahlen_einkommen3 = income3 %>% 
  select(Eink_5) %>% 
  count(Eink_5)



rename_einkommen <- function(df3) {
  df3 %>%
    mutate(Eink_5 = factor(
      Eink_5,
      levels = c("1.500 bis unter 2.600 €", "2.600 bis unter 3.600", "3.600 bis unter 5.600 €", "5.600 € und mehr", "Unter 1.500 €"),
      labels = c("1.500-2.600€", "2.600-3.600€", "3.600-5.600€", "5.600€+", "unter 1.500€")
    ))
}

# Auf alle drei Data Frames anwenden
Zahlen_einkommen1 <- Zahlen_einkommen1 %>% rename_einkommen()
Zahlen_einkommen2 <- Zahlen_einkommen2 %>% rename_einkommen()
Zahlen_einkommen3 <- Zahlen_einkommen3 %>% rename_einkommen()


Zahlen_einkommen = bind_rows(Zahlen_einkommen1, Zahlen_einkommen2, Zahlen_einkommen3) %>% 
  group_by(Eink_5) %>% 
  summarise(n_sum_einkommen = sum(n))

Zahlen_einkommen_rel = Zahlen_einkommen %>% 
  mutate(rel_einkommen = n_sum_einkommen/sum(n_sum_einkommen))


Zahlen_einkommen_rel$Eink_5 <- factor(
  Zahlen_einkommen_rel$Eink_5,
  levels = c("unter 1.500€", "1.500-2.600€", "2.600-3.600€", "3.600-5.600€", "5.600€+")
)


ggplot(Zahlen_einkommen_rel, aes(x = Eink_5, y = rel_einkommen)) +
  geom_col(fill = "darkblue", color = "white") +
  geom_text(
    aes(label = n_sum_einkommen),
    vjust = -0.5,
    size = 3.5,
    color = "black"
  ) +
  theme_minimal() 




# aktuell wird das nicht benutzt

# jetzt Aufteilung je Verkehrsmittel
# ich fasse MIV Fahrer und Mitfahrer jetzt auf frech zusammen

gender_miv = gender %>% 
  select(VM_4, Geschlecht_3) %>% 
  filter(VM_4 == "MIV") %>% 
  count(Geschlecht_3)

gender_miv_rel = gender_miv %>% 
  mutate(rel_Wsk_miv = n/sum(n))

ggplot(gender_miv_rel, aes(x = Geschlecht_3, y = rel_Wsk_miv)) +
  geom_col(fill = "lightblue", color = "black") 


# im Endeffekt ist diese Grundstruktur für alle unterschiedlichen Verkehrsträger, vielleicht kann man da was vereinfachen





