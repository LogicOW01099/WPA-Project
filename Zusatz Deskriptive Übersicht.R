# Die auswertung bezieht sich nur auf befragte aus städten Flach unter 500
# würde hier n aus gender 1, 2 und 3 berechnen geht zwar nur um wenig wäre aber 
# methodisch besser

Zahlen_gender = gender %>% 
  select(Geschlecht_3, E_GESCHLECHT_3) %>% 
  count(Geschlecht_3)

print(Zahlen_gender)

Zahlen_gender_rel = Zahlen_gender %>% 
  mutate(rel_Häufigkeit = n /sum(n))


ggplot(Zahlen_gender_rel, aes(x = Geschlecht_3, y = rel_Häufigkeit)) +
  geom_col(fill = "lightblue", color = "black") 

