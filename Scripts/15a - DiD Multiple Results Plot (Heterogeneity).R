library(viridis)
# Extrair os resultados
df1 <- data.frame(
  group = mw.attgt4$group,
  time = mw.attgt4$t,
  att = mw.attgt4$att,
  se = mw.attgt4$se,
  c = mw.attgt4$c,
  Model = "High Accessibility*"
)

df2 <- data.frame(
  group = mw.attgt5$group,
  time = mw.attgt5$t,
  att = mw.attgt5$att,
  se = mw.attgt5$se,
  c = mw.attgt5$c,
  Model = "Low Accessibility*"
)


# Combinar os dataframes
df_combinado <- rbind(df1, df2)

# Ajustar a variável Model como fator com a ordem desejada
df_combinado$Model <- factor(df_combinado$Model, 
                             levels = c("High Accessibility*", 
                                        "Low Accessibility*"
                                        ))

library(ggplot2)

ggplot(df_combinado, aes(x = time, y = att, color = Model)) +
  geom_point(size = 2.5, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = att - c * se, ymax = att + c * se), 
                width = 0.4, size = 1.1, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + # Linha horizontal em negrito
  geom_vline(xintercept = 2014, linetype = "dashed", color = "lightgray", size = 0.8) + # Linha vertical pontilhada
  labs(
    x = "Year",
    y = "ATT") +
  theme_light() +
  # Paleta Viridis
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  scale_x_continuous(breaks = seq(floor(min(df_combinado$time)), ceiling(max(df_combinado$time)), by = 1))