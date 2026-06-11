# Datei basierend auf WPAReader.R und Gender.R --> zuerst einlesen!
# Anleitung dazu wie man den Zensus beachtet 


# ========WIE IMPLEMENTIERT MAN GEWICHTUNG========


# === zum rechnen
# ==== 2a Wohnen Arbeit // Arbeit Wohnen -- geschlecht in 3er einteilung und HVM in 5er
table35a <- xtabs(GEWICHT_HH_ZENSUS ~ E_GESCHLECHT_3 + E_HVM_5,
                  data = gender,
                  subset = E_QZG_17 %in% c(1, 8))
assocstats(table35a) # Maße mit gewichtung

# === zum plotten
gender %>%
  filter(E_QZG_17 %in% c(1, 8)) %>%
  count(Geschlecht_3, VM_5, wt = GEWICHT_HH_ZENSUS) %>% ### === Hier steht wie man Gewichtung einbaut ===
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
    caption  = "Oberzentren unter 500000 EW Flach"
  )








