# Base Code füt Boxplot bei income - VM Wahl 
# IN Income.R die LETZTE Zeile in Kommentar umwandeln dann funktioniert das ohne Probleme 
# Interpretation mit KI 
  # https://claude.ai/share/5a354c82-8d4b-407e-962e-76225f1c5189
# Empfehlung für Zensus korregierte Daten, weil wir hier alle Städte über einen Kamm scheren
# WA und WB hier zusammengefasst

# 0Zu500Flach
income$Eink <- factor(
  income$Eink,
  levels = c(
    "Unter 500 €",
    "500 bis unter 900 €",
    "900 bis unter 1.500 €",
    "1.500 bis unter 2.000 €",
    "2.000 bis unter 2.600 €",
    "2.600 bis unter 3.000 €",
    "3.000 bis unter 3.600 €",
    "3.600 bis unter 4.600 €",
    "4.600 bis unter 5.600 €",
    "5.600 bis unter 6.600 €",
    "6.600 € und mehr"
  ),
  ordered = TRUE
)

income$VM_5 <- factor(income$VM_5)
income$VM_5 <- factor(income$VM_5,
                     levels = c("Fahrrad", "MIV Fahrer/-in", "MIV Mitfahrer/-in", "ÖV", "Zu Fuß"),
                     labels = c("Fahrrad", "MIV-F", "MIV-M", "ÖV", "Fuß")
)

levels(income$Eink) <- c(
  "< 500 €",
  "500–900 €",
  "900–1.500 €",
  "1.500–2.000 €",
  "2.000–2.600 €",
  "2.600–3.000 €",
  "3.000–3.600 €",
  "3.600–4.600 €",
  "4.600–5.600 €",
  "5.600–6.600 €",
  "> 6.600 €"
)

mosaic(
  ~ Eink_5 + VM_5,
  data     = income,
  shade    = TRUE,
  legend   = TRUE,
  labeling = labeling_border(
    rot_labels      = c(90, 0, 0, 0),
    just_labels     = c("left", "right", "center", "center"),
    set_varnames    = c(
      Eink = "Einkommen (netto/Monat)",
      VM_5          = "Verkehrsmittelwahl"
    ),
    offset_varnames = c(4, 0, 0, 4),  # oben und links weiter rausschieben
    offset_labels   = c(1, 0, 0, 1),
    gp_labels       = gpar(fontsize = 9),
    gp_varnames     = gpar(fontsize = 11, fontface = "bold")
  ),
  margins = c(10, 12, 6, 2),  # mehr Platz oben und links
  main    = "Verkehrsmittelwahl nach Haushaltseinkommen"
)



# 0Zu500huegelig
income2$Eink <- factor(
  income2$Eink,
  levels = c(
    "Unter 500 €",
    "500 bis unter 900 €",
    "900 bis unter 1.500 €",
    "1.500 bis unter 2.000 €",
    "2.000 bis unter 2.600 €",
    "2.600 bis unter 3.000 €",
    "3.000 bis unter 3.600 €",
    "3.600 bis unter 4.600 €",
    "4.600 bis unter 5.600 €",
    "5.600 bis unter 6.600 €",
    "6.600 € und mehr"
  ),
  ordered = TRUE
)

income2$VM_5 <- factor(income2$VM_5)
income2$VM_5 <- factor(income2$VM_5,
                      levels = c("Fahrrad", "MIV Fahrer/-in", "MIV Mitfahrer/-in", "ÖV", "Zu Fuß"),
                      labels = c("Fahrrad", "MIV-F", "MIV-M", "ÖV", "Fuß")
)

levels(income2$Eink) <- c(
  "< 500 €",
  "500–900 €",
  "900–1.500 €",
  "1.500–2.000 €",
  "2.000–2.600 €",
  "2.600–3.000 €",
  "3.000–3.600 €",
  "3.600–4.600 €",
  "4.600–5.600 €",
  "5.600–6.600 €",
  "> 6.600 €"
)

mosaic(
  ~ Eink_5 + VM_5,
  data     = income2,
  shade    = TRUE,
  legend   = TRUE,
  labeling = labeling_border(
    rot_labels      = c(90, 0, 0, 0),
    just_labels     = c("left", "right", "center", "center"),
    set_varnames    = c(
      Eink = "Einkommen (netto/Monat)",
      VM_5          = "Verkehrsmittelwahl"
    ),
    offset_varnames = c(4, 0, 0, 4),  # oben und links weiter rausschieben
    offset_labels   = c(1, 0, 0, 1),
    gp_labels       = gpar(fontsize = 9),
    gp_varnames     = gpar(fontsize = 11, fontface = "bold")
  ),
  margins = c(10, 12, 6, 2),  # mehr Platz oben und links
  main    = "Verkehrsmittelwahl nach Haushaltseinkommen"
)
















income2$Eink <- factor(
  income2$Eink,
  levels = c(
    "Unter 500 €", "500 bis unter 900 €", "900 bis unter 1.500 €",
    "1.500 bis unter 2.000 €", "2.000 bis unter 2.600 €",
    "2.600 bis unter 3.000 €", "3.000 bis unter 3.600 €",
    "3.600 bis unter 4.600 €", "4.600 bis unter 5.600 €",
    "5.600 bis unter 6.600 €", "6.600 € und mehr"
  ),
  ordered = TRUE
)
levels(income2$Eink) <- c(
  "< 500 €", "500–900 €", "900–1.500 €", "1.500–2.000 €",
  "2.000–2.600 €", "2.600–3.000 €", "3.000–3.600 €",
  "3.600–4.600 €", "4.600–5.600 €", "5.600–6.600 €", "> 6.600 €"
)
income2$VM_5 <- factor(
  income2$VM_5,
  levels = c("Fahrrad", "MIV Fahrer/-in", "MIV Mitfahrer/-in", "ÖV", "Zu Fuß"),
  labels = c("Fahrrad", "MIV-F", "MIV-M", "ÖV", "Fuß")
)

# Mosaikplot funktion
plot_mosaic <- function(daten, titel) {
  mosaic(
    ~ Eink_5 + VM_5,
    data     = daten,
    shade    = TRUE,
    legend   = TRUE,
    labeling = labeling_border(
      rot_labels      = c(90, 0, 0, 0),
      just_labels     = c("left", "right", "center", "center"),
      set_varnames    = c(
        Eink_5 = "Einkommen (Monat)",
        VM_5   = "Verkehrsmittelwahl"
      ),
      offset_varnames = c(4, 0, 0, 4),
      offset_labels   = c(1, 0, 0, 1),
      gp_labels       = gpar(fontsize = 9),
      gp_varnames     = gpar(fontsize = 11, fontface = "bold")
    ),
    margins = c(10, 12, 6, 2),
    main    = titel
  )
}

# 2 plots
sub_1_8  <- subset(income2, E_QZG_17 %in% c(1, 8))
sub_3_10 <- subset(income2, E_QZG_17 %in% c(3, 10))

plot_mosaic(sub_1_8,  "Verkehrsmittelwahl nach Haushaltseinkommen (Wohnen - Arbeit)")
plot_mosaic(sub_3_10, "Verkehrsmittelwahl nach Haushaltseinkommen (Wohnen - Bildung)")














# 0Züber500Flach
income3$Eink <- factor(
  income3$Eink,
  levels = c( 
    "Unter 500 €",
    "500 bis unter 900 €",
    "900 bis unter 1.500 €",
    "1.500 bis unter 2.000 €",
    "2.000 bis unter 2.600 €",
    "2.600 bis unter 3.000 €",
    "3.000 bis unter 3.600 €",
    "3.600 bis unter 4.600 €",
    "4.600 bis unter 5.600 €",
    "5.600 bis unter 6.600 €",
    "6.600 € und mehr"
  ),
  ordered = TRUE
)

income3$VM_5 <- factor(income3$VM_5)
income3$VM_5 <- factor(income3$VM_5,
                      levels = c("Fahrrad", "MIV Fahrer/-in", "MIV Mitfahrer/-in", "ÖV", "Zu Fuß"),
                      labels = c("Fahrrad", "MIV-F", "MIV-M", "ÖV", "Fuß")
)

levels(income3$Eink) <- c(
  "< 500 €",
  "500–900 €",
  "900–1.500 €",
  "1.500–2.000 €",
  "2.000–2.600 €",
  "2.600–3.000 €",
  "3.000–3.600 €",
  "3.600–4.600 €",
  "4.600–5.600 €",
  "5.600–6.600 €",
  "> 6.600 €"
)

mosaic(
  ~ Eink_5 + VM_5, # einkommen in 5er Gruppen
  data     = income3,
  shade    = TRUE,
  legend   = TRUE,
  labeling = labeling_border(
    rot_labels      = c(90, 0, 0, 0),
    just_labels     = c("left", "right", "center", "center"),
    set_varnames    = c(
      Eink = "Einkommen (netto/Monat)",
      VM_5          = "Verkehrsmittelwahl"
    ),
    offset_varnames = c(4, 0, 0, 4),  # oben und links weiter rausschieben
    offset_labels   = c(1, 0, 0, 1),
    gp_labels       = gpar(fontsize = 9),
    gp_varnames     = gpar(fontsize = 11, fontface = "bold")
  ),
  margins = c(10, 12, 6, 2),  # mehr Platz oben und links
  main    = "Verkehrsmittelwahl nach Haushaltseinkommen"
)
