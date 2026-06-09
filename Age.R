# Datei basierend auf WPAReader.R --> zuerst einlesen!
# Der Quellcode in dieser Datei beschäftigt sich ausschließlich mit den Zusammenhangsmaßen zwischen
# HVM mit dem Alter. Ergänzungen bitte direkt in den Aufgaben Tracker auf Notion schreiben.


# WIR SOLLTEN UNS GGF MIT KI GEDANKEN ZU ALTERNATIVER DARSTELLUNG NEBEN BALKENDIAGRAMMEN MACHEN


# bbs für übersichtlichkeit eingefügt wenn man doch nur eins betrachten mag

# interpretation Corners um auf einen schlag alle ergebnisse der Zusammenhangsmaße zu haben
# einfach per cmd+f // strg+f suchen 
# interpretation corner 1 --> OZu500Flach
# interpretation corner 2 --> OZu500Huegelig
# interpretation corner 3 --> OZü500Flach

# !!OZu500Flach!!
# bb = c("OZu500HuegeligH","OZu500HuegeligP","OZu500HuegeligW","OZüber500FlachH","OZüber500FlachP","OZüber500FlachW")
# rm(list=bb, bb)

# Schritt 1 Daten bereinigen, vorallem NAs sowie E_WEG_GUELTIG 