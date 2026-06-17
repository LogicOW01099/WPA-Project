# Base Code füt Boxplot bei Alter - VM Wahl 
# IN Age.R die LETZTE Zeile in Kommentar umwandeln dann funktioniert das ohne Probleme 


ggplot(age, aes(x = factor(VM_5), y = V_ALTER, fill = factor(VM_5))) + # 0Zu500Flach
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  labs(
    x    = "Verkehrsmittelwahl",
    y    = "Alter",
    fill = "VM"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(age2, aes(x = factor(VM_5), y = V_ALTER, fill = factor(VM_5))) + # 0Zu500Huegelig
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  labs(
    x    = "Verkehrsmittelwahl",
    y    = "Alter",
    fill = "VM"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(age3, aes(x = factor(VM_5), y = V_ALTER, fill = factor(VM_5))) + # 0Züber500Flach
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  labs(
    x    = "Verkehrsmittelwahl",
    y    = "Alter",
    fill = "VM"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
