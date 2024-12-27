library(MatchIt)
library(WeightIt)
library(cobalt)
library(viridis)

base <- da_filtrada4
# Matching IPW for abandono_tot_em graph
# Estimate the pscore
pscore_w <- weightit(treat ~ abandono_2009+abandono_2010+abandono_2011+abandono_2012+
                       abandono_2013+abandono_2014+NU_MEDIA_MT_x+NU_MEDIA_CN_x+
                       NU_MEDIA_CH_x+sem_enem,
                     data = base, 
                     method = "glm", 
                     estimand = "ATE",
                     stabilize = TRUE,
                     s.weights = "QT_MAT_MED")

cobalt::love.plot(pscore_w, binary = "std", thresholds = c(m = .1)) +
  theme(plot.title = element_blank())

bal.plot(pscore_w, which = "both") +
  xlim(c(0,1)) +
  labs(x = "Propensity Score", y = "Density") +
  theme(plot.title = element_blank())


# Adicione os pesos à base de dados
base$ipw <- pscore_w$weights

medias_anuais <- base %>%
  group_by(Ano, treat) %>%
  summarise(media_abandono = weighted.mean(abandono_tot_em, w = ipw, na.rm = TRUE))

ggplot(medias_anuais, aes(x = Ano, y = media_abandono, color = factor(treat))) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "lightgray", size = 0.8) +
  scale_color_viridis_d( 
    labels = c("Control", "Treatment"),
    name = "Group",
    end = 0.8    # Ajuste este valor para mudar a cor do "Tratado"

  ) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Year", 
       y = "IPW Weighted Average Dropout Rate") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )
