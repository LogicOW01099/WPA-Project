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
  select(Alter_7) %>% 
  count(Alter_7)


Zahlen_alter2 = age2 %>% 
  select(Alter_7)%>% 
  count(Alter_7)


Zahlen_alter3 = age3 %>% 
  select(Alter_7)%>% 
  count(Alter_7)

rename_alter <- function(df2) {
  df2 %>%
    mutate(Alter_7 = factor(
      Alter_7,
      levels = c("0 bis 17 Jahre", "18 bis 25 Jahre", "26 bis 35 Jahre", "36 bis 50 Jahre", "51 bis 60 Jahre","61 bis 70 Jahre","71 Jahre und älter"),
      labels = c("0-17 Jahre", "18-25 Jahre", "26-35 Jahre", "36-50 Jahre", "51-60 Jahre", "61-70 Jahre", "70+ Jahre")
    ))
}

# Auf alle drei Data Frames anwenden
Zahlen_alter1 <- Zahlen_alter1 %>% rename_alter()
Zahlen_alter2 <- Zahlen_alter2 %>% rename_alter()
Zahlen_alter3 <- Zahlen_alter3 %>% rename_alter()


Zahlen_alter = bind_rows(Zahlen_alter1, Zahlen_alter2, Zahlen_alter3) %>% 
  group_by(Alter_7) %>% 
  summarise(n_sum_alter = sum(n)) 


# Relative Wsk anstatt absolut 
Zahlen_alter_rel = Zahlen_alter %>% 
  mutate(rel_alter = n_sum_alter /sum(n_sum_alter))


ggplot(Zahlen_alter_rel, aes(x = Alter_7, y = rel_alter)) +
  geom_col(fill = "darkblue", color = "white") +
  geom_text(
    aes(label = n_sum_alter),
    vjust = -0.5,
    size = 3.5,
    color = "black"
  ) +
  theme_minimal() +
  guides(x = guide_axis(angle = 20))








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






sum(Zahlen_gender$n_sum_gender)
sum(Zahlen_alter$n_sum_alter)
sum(Zahlen_einkommen$n_sum_einkommen)





