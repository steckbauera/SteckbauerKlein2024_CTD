######################################################################################
######################################################################################
######################################################################################
###                                                                                ###
###                               OceanX RSDE 2022                                 ###
###                               Deep Depression                                  ###
###                           Carbonate Chemistry Data                             ###
###                            by Alexandra Steckbauer                             ###
###                                                                                ###
######################################################################################
######################################################################################
######################################################################################


# LOAD LIBRARYs #

# install.packages("marelac")
# install.packages("seacarb")
# install.packages("SolveSAPHE")
library(SolveSAPHE)
library(marelac)
library(seacarb)


######################################################################################

### Load data files individually:

TA_BH <- read.csv("/.../2_Carbonate_Chemistry_Input_File.csv", header = TRUE)
head(TA_BH)
tail(TA_BH)
summary(TA_BH)

################################################################################

# Calculate RHO

TA_BH$RHO <- rho(S = TA_BH$Salinity, T = TA_BH$Temperature, P = TA_BH$Pressure)
write.csv(TA_BH,"/.../4_rho_calculated_output.csv", row.names = FALSE)


################################################################################

# Unit needs to be in mol/kg, so the values need to be divided by e-6
TA_BH$Depth.2 <- TA_BH$Depth / 10
TA_BH$TA.2 <- TA_BH$Total_Alkalinity * 1e-06
TA_BH$DIC.2 <- TA_BH$DIC * 1e-06


var1 <- TA_BH$TA.2
var2 <- as.numeric(TA_BH$DIC.2)
Sal <- TA_BH$Salinity
Temp <- TA_BH$Temperature
p <- TA_BH$Depth.2

object_carb <- carb(flag = 15,                            # flag 15 is for input ALK and DIC
                    var1 = var1,                          # TA in mol/kg, needs to be divided by e-6
                    var2 = var2,                          # DIC in mol/kg, needs to be divided by e-6
                    S = Sal,
                    T = Temp,
                    P = p,                                # P in bar, needs to be divided by 10 from meters
                    k1k2 = "l",                           # l is for Lueker 2000 konstants for k1 and k2
                    pHscale = "T",                        # pH in Total Scale
                    kf = "dg",                            # dg is for Dickson and Riley 1979
                    ks = "d",                             # d is for Dickson 1990
                    b = "l10",                            # Concentration of total boron. "l10" for the Lee 2010
                    Pt = 0,                              # Pt is for Phosphate in mol/kg, needs to be divided by e-6
                    Sit = 0)                            # Sit is for Silicate in mol/kg, needs to be divided by e-6

write.csv(object_carb,"/.../5_Full_Biogeochem_output_NEW.csv", row.names = FALSE)

################################################################################
################################################################################
################################################################################