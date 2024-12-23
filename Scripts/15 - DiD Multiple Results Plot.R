# Extrair os resultados
df1 <- data.frame(
  group = mw.attgt$group,
  time = mw.attgt$t,
  att = mw.attgt$att,
  se = mw.attgt$se,
  c = mw.attgt$c,
  Model = "No Covariates"
)

df2 <- data.frame(
  group = mw.attgt2$group,
  time = mw.attgt2$t,
  att = mw.attgt2$att,
  se = mw.attgt2$se,
  c = mw.attgt2$c,
  Model = "Lagged Outcomes*"
)

df3 <- data.frame(
  group = mw.attgt3$group,
  time = mw.attgt3$t,
  att = mw.attgt3$att,
  se = mw.attgt3$se,
  c = mw.attgt3$c,
  Model = "Avg Lagged Outcomes"
)


# Combinar os dataframes
df_combinado <- rbind(df1, df2, df3)

library(ggplot2)

ggplot(df_combinado, aes(x = time, y = att, color = Model)) +
  geom_point(size = 2.5, position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = att - c * se, ymax = att + c * se), 
                width = 0.1, size = 1.05, position = position_dodge(0.2)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) + # Linha horizontal em negrito
  geom_vline(xintercept = 2014, linetype = "dashed", color = "lightgray", size = 0.8) + # Linha vertical pontilhada
  labs(
       x = "Year",
       y = "ATT") +
  theme_light() +
  scale_color_manual(values = c("No Covariates" = "#ff5733", "Lagged Outcomes*" = "#33dbff",
                                "Avg Lagged Outcomes" = "#bd33ff")) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  scale_x_continuous(breaks = seq(floor(min(df_combinado$time)), ceiling(max(df_combinado$time)), by = 1))