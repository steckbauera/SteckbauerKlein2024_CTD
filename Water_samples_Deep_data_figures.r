######################################################################################
######################################################################################
######################################################################################
###                                                                                ###
###                               OceanX RSDE 2022                                 ###
###                          Deep Depression Water Sample                          ###
###                            by Alexandra Steckbauer                             ###
###                                                                                ###
######################################################################################
######################################################################################
######################################################################################


### Install and LOAD LIBRARYs

# install.packages("oce")
library(oce)
# install.packages("ggplot2")
library(ggplot2)
#install.packages("ddalpha")
library(ddalpha)
#install.packages("dplyr")
library(dplyr)
#install.packages("psych")
library(psych)


####################################################################################
####################################################################################
####################################################################################

### Carbonate Chemistry


df_BH1 <- read.csv("/.../Water_samples/5_BH1_for_R.csv", header = TRUE)
head(df_BH1)  
tail(df_BH1)  

df_BH2 <- read.csv("/.../Water_samples/5_BH2_for_R.csv", header = TRUE)
head(df_BH2)  
tail(df_BH2)  

######################################################################################

# Merge files

df_BH <- merge(df_BH1, df_BH2, all=TRUE)

# Check that the dataframe is ok, and you don't get any error messages
head(df_BH)
tail(df_BH)
summary(df_BH)

######################################################################################

# Order dataframe by Depth (important for figures)

df_BH <- df_BH[order(df_BH$Depth),]
df_BH

df_BH1 <- df_BH1[order(df_BH1$Depth),]
df_BH1

df_BH2 <- df_BH2[order(df_BH2$Depth),]
df_BH2

######################################################################################

### Plot data

# TA
ggplot(data = df_BH, aes(x = TA_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = TA_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = TA_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = TA_Average, y = Depth,
                    xmin = TA_Average-TA_SD,
                    xmax = TA_Average+TA_SD),
                show.legend = FALSE) +
  labs(x = bquote(TA ~(µmol ~kg^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     limits = c(2360, 2460)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_TA.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_TA.pdf",
       width = 2.5, height = 3, dpi = 600)

# DIC
ggplot(data = df_BH, aes(x = DIC_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = DIC_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = DIC_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = DIC_Average, y = Depth,
                    xmin = DIC_Average-DIC_SD,
                    xmax = DIC_Average+DIC_SD),
                show.legend = FALSE) +
  labs(x = bquote(DIC ~(µmol ~kg^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     limits = c(2050, 2350)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_DIC.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_DIC.pdf",
       width = 2.5, height = 3, dpi = 600)


# Omega Aragonite
ggplot(data = df_BH, aes(x = OmegaAragonite_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE, shape = 17) +
  geom_path(data = df_BH1, aes(x = OmegaAragonite_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = OmegaAragonite_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = OmegaAragonite_Average, y = Depth,
                    xmin = OmegaAragonite_Average-OmegaAragonite_SD,
                    xmax = OmegaAragonite_Average+OmegaAragonite_SD),
                show.legend = FALSE) +
  labs(x = bquote(Omega[Ar]),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     limits = c(1, 4)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_OmegaAragonite.png",
       width = 2.5, height = 3, dpi = 600) 
ggsave("/.../Water_samples/BH1_BH2_OmegaAragonite.pdf",
       width = 2.5, height = 3, dpi = 600) 


# pCO2
ggplot(data = df_BH, aes(x = pCO2_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE, shape = 17) +
  geom_path(data = df_BH1, aes(x = pCO2_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = pCO2_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = pCO2_Average, y = Depth,
                    xmin = pCO2_Average-pCO2_SD,
                    xmax = pCO2_Average+pCO2_SD),
                show.legend = FALSE) +
  labs(x = expression(paste(italic("p"), CO[2], " (", italic("µ"), "atm)")),
  y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(400, 1400, by = 200),
                     limits = c(400, 1400)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_pCO2.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_pCO2.pdf",
       width = 2.5, height = 3, dpi = 600)

# pH
ggplot(data = df_BH, aes(x = pH_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE, shape = 17) +
  geom_path(data = df_BH1, aes(x = pH_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = pH_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = pH_Average, y = Depth,
                    xmin = pH_Average-pH_SD,
                    xmax = pH_Average+pH_SD),
                show.legend = FALSE) +
  labs(x = bquote(pH[T]),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(7.5, 8.1, by = 0.1),
                     limits = c(7.5, 8.1)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_pH.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_pH.pdf",
       width = 2.5, height = 3, dpi = 600)

######################################################################################
######################################################################################
######################################################################################

### Nutrients

# Nitrate
ggplot(data = df_BH, aes(x = Nitrate_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = Nitrate_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = Nitrate_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = Nitrate_Average, y = Depth,
                    xmin = Nitrate_Average-Nitrate_SD,
                    xmax = Nitrate_Average+Nitrate_SD),
                show.legend = FALSE) +
  labs(x = bquote(Nitrate ~(µmol ~L^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_reverse() +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 20, by = 5),
                     limits = c(0, 20)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_Nitrate.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_Nitrate.pdf",
       width = 2.5, height = 3, dpi = 600)


# Nitrite
ggplot(data = df_BH, aes(x = Nitrite_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = Nitrite_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = Nitrite_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = Nitrite_Average, y = Depth,
                    xmin = Nitrite_Average-Nitrite_SD,
                    xmax = Nitrite_Average+Nitrite_SD),
                show.legend = FALSE) +
  labs(x = bquote(Nitrite ~(µmol ~L^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 0.4, by = 0.1),
                     limits = c(0, 0.4)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_Nitrite.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_Nitrite.pdf",
       width = 2.5, height = 3, dpi = 600)

# Phosphate
ggplot(data = df_BH, aes(x = Phosphate_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = Phosphate_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = Phosphate_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = Phosphate_Average, y = Depth,
                    xmin = Phosphate_Average-Phosphate_SD,
                    xmax = Phosphate_Average+Phosphate_SD),
                show.legend = FALSE) +
  labs(x = bquote(Phosphate ~(µmol ~L^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 2, by = 0.5),
                     limits = c(0, 2)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_Phosphate.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_Phosphate.pdf",
       width = 2.5, height = 3, dpi = 600)

# Silica
ggplot(data = df_BH, aes(x = Silica_Average, y = Depth, color = Category), size = 2) +
  geom_point(show.legend = FALSE) +
  geom_path(data = df_BH1, aes(x = Silica_Average, y = Depth), color = "royalblue3", size = 0.3, show.legend = FALSE) +
  geom_path(data = df_BH2, aes(x = Silica_Average, y = Depth), color = "hotpink4", size = 0.3, show.legend = FALSE) +
  geom_errorbar(aes(x = Silica_Average, y = Depth,
                    xmin = Silica_Average-Silica_SD,
                    xmax = Silica_Average+Silica_SD),
                show.legend = FALSE) +
  labs(x = bquote(Silica ~(µmol ~L^-1)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse", expand = c(0, 0), limits = c(650, 0)) +
  scale_x_continuous(position = "top", 
                     breaks = seq(0, 20, by = 5),
                     limits = c(0, 20)
  ) +
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 9, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("BH1" = "royalblue3", "BH2" = "hotpink4")) 
ggsave("/.../Water_samples/BH1_BH2_Silica.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../Water_samples/BH1_BH2_Silica.pdf",
       width = 2.5, height = 3, dpi = 600)


######################################################################################
######################################################################################
######################################################################################