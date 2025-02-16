######################################################################################
######################################################################################
######################################################################################
###                                                                                ###
###                               OceanX RSDE 2022                                 ###
###                           Deep Depression CTD Data                             ###
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

### Reading and preparing the CTD files

CTD.CNV1 <- read.ctd(file = "/.../CTD/DOWN/BH1_down.cnv",
                   header = FALSE, sep = "", dec = ".")

# Save as CSV file
oce.write.table(CTD.CNV1, file = "/.../CTD/DOWN/BH1_down.csv")

# Read the CSV files
df.CTD1 <- read.csv("/.../CTD/DOWN/BH1_down.csv", header = TRUE, sep = " ")

df.CTD1$BH_Name <- "BlueHole1"

# Check that the dataframe is ok, and you don't get any error messages
head(df.CTD1)
tail(df.CTD1)
summary(df.CTD1)

####################################################################################

### Reading and preparing the CTD files

CTD.CNV2 <- read.ctd(file = "/.../CTD/DOWN/BH2_down.cnv",
                     header = FALSE, sep = "", dec = ".")

# Save as CSV file
oce.write.table(CTD.CNV2, file = "/.../CTD/DOWN/BH2_down.csv")

# Read the CSV files
df.CTD2 <- read.csv("/.../CTD/DOWN/BH2_down.csv", header = TRUE, sep = " ")

df.CTD2$BH_Name <- "BlueHole2"

# Check that the dataframe is ok, and you don't get any error messages
head(df.CTD2)
tail(df.CTD2)
summary(df.CTD2)


######################################################################################

# Change Column Names
names <- c("Time", "Pressure", "Temperature1", "Temperature2", "Conductivity1", "Conductivity2", "BeamAttenuation",
           "BeamTransmition", "Fluorescence", "Altimeter", "Depth", "PotentialTemprature1", "PotentialTemprature2", 
           "Oxygen1mll", "Oxygen1umolkg", "Oxygen1saturation", "Oxygen2mll", "Oxygen2umolkg", "Oxygen2saturation", 
           "Density1", "Density2", "SoundVelocity1", "SoundVelocity2", "SalinityPractical1","SalinityPractical2",
           "TemperatureDifference", "ConductivityDifference", "SalinityPracticalDifference", "Flag", "BH_Name")
colnames(df.CTD1) = names
colnames(df.CTD2) = names

######################################################################################

# Merge files

df.CTD <- merge(df.CTD1, df.CTD2, all=TRUE)

# Check that the dataframe is ok, and you don't get any error messages
head(df.CTD)
tail(df.CTD)
summary(df.CTD)

######################################################################################

# ### CLEANING STEPS

# Remove first 3 meters of the dataset
df.CTD_Depthcut <- subset(df.CTD, Depth > 3)
df.CTD_Depthcut1 <- subset(df.CTD1, Depth > 3)
df.CTD_Depthcut2 <- subset(df.CTD2, Depth > 3)

######################################################################################

# Oxygen µmol kg-1
ggplot() +
  geom_path(data = df.CTD_Depthcut1, aes(x = Oxygen1umolkg, y = Depth), color = "royalblue3", size = 0.5, show.legend = FALSE) +
  geom_path(data = df.CTD_Depthcut2, aes(x = Oxygen1umolkg, y = Depth), color = "hotpink4", size = 0.5, show.legend = FALSE) +
  labs(x = bquote(O[2] ~(µmol ~kg^-1)),
       y = bquote(Depth ~(m))) +
   scale_y_continuous(trans = "reverse") +
  scale_x_continuous(position = "top") +
  theme(axis.text = element_text(size = 9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("/.../CTD/DOWN/BH1_BH2_DOumolkg.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../CTD/DOWN/BH1_BH2_DOumolkg.pdf",
       width = 2.5, height = 3, dpi = 600)


# Temperature
ggplot() +
  geom_path(data = df.CTD_Depthcut1, aes(x = Temperature1, y = Depth), color = "royalblue3", size = 0.5, show.legend = FALSE) +
  geom_path(data = df.CTD_Depthcut2, aes(x = Temperature1, y = Depth), color = "hotpink4", size = 0.5, show.legend = FALSE) +
  labs(x = bquote(Temperature ~(ºC)),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(position = "top") +
  theme(axis.text = element_text(size = 9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("/.../CTD/DOWN/BH1_BH2_Temperature.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../CTD/DOWN/BH1_BH2_Temperature.pdf",
       width = 2.5, height = 3, dpi = 600)

# Salinity
ggplot() +
  geom_path(data = df.CTD_Depthcut1, aes(x = SalinityPractical1, y = Depth), color = "royalblue3", size = 0.5, show.legend = FALSE) +
  geom_path(data = df.CTD_Depthcut2, aes(x = SalinityPractical1, y = Depth), color = "hotpink4", size = 0.5, show.legend = FALSE) +
  labs(x = bquote(Salinity),
       y = bquote(Depth ~(m))) +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(position = "top") +
  theme(axis.text = element_text(size = 9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("/.../CTD/DOWN/BH1_BH2_Salinity.png",
       width = 2.5, height = 3, dpi = 600)
ggsave("/.../CTD/DOWN/BH1_BH2_Salinity.pdf",
       width = 2.5, height = 3, dpi = 600)


######################################################################################
######################################################################################
######################################################################################