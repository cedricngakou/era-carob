### R script to transform ERA data into carob standard data 

library(tidyr)
library(dplyr)


load("~/era-carob/ERA data/agronomic_majestic-hippo-2020-2025-03-19.2_industrious-elephant-2023-2025-03-19.1.RData")

### read data 
r1 <- do.call(carobiner::bindr, era_merge["Data.Out"])

### Process Data.out file 

d <- data.frame(
   Time= r1$Time,
   B_code= r1$B.Code,
   country= r1$Country,
   Mean_Season_Prec= ifelse(is.na(r1$Site.MSP.S1) & !is.na(r1$Site.MSP.S2), r1$Site.MSP.S2, r1$Site.MSP.S2),
   Mean_Annual_Prec= r1$Site.MAP,
   #r1$Site.Slope.Perc,
   location= r1$Site.ID,
   on_farm= ifelse(grepl("Farm", r1$Site.Type), TRUE, FALSE) ,
   #adm= r1$Site.Admin,
   location_used= r1$Site.Agg,
   latitude= r1$Site.LatD,
   #latitude_unc= r1$Site.Lat.Unc,
   longitude= r1$Site.LonD,
   #longitude_unc= r1$Site.Lon.Unc,
   elevation= r1$Site.Elevation,
   season= r1$Site.Rain.Seasons,
   #r1$Site.Slope.Degree,
   soil_type= r1$Site.Soil.Texture,
   Value= r1$ED.Mean.T,
   variable= gsub(" ", "_",r1$Out.Subind),
   crop= r1$P.Product,
   #variety= r1$ED.Variety,
   intercrop= r1$IN.Prods.All,
   product_type= r1$Product.Type,
   version= r1$Version,
   #planting_method= r1$PD.Plant.Variable,
   planting_date= unclass(r1$PD.Plant.Start),
   planting_date_End=  unclass(r1$PD.Plant.End),
   harvest_date= unclass(r1$PD.Harvest.Start),
   harvest_date_End= unclass(r1$PD.Harvest.End),
   year= r1$Time.Start.Year,
   DAs= r1$PD.Harvest.DAS,
   DAP= r1$PD.Date.DAP,
   rep= r1$ED.Reps,
   temp= r1$Time.Clim.Temp.Mean,
   tmax= r1$Time.Clim.Temp.Max,
   tmin= r1$Time.Clim.Temp.Min,
   irrigation_method= r1$I.Method,
   irrigation_fulfullment= r1$I.Strategy,
   irrigated= r1$Irrig,
   plot_area= r1$EX.Plot.Size,
   variety_type= r1$V.Type,
   variety= r1$V.Var,
   maturity_days= r1$V.Maturity,
   Till.Level.Name= r1$Till.Level.Name,
   weeding_method= r1$W.Method,
   weeding_times= r1$W.Freq,
   herbicide_product= r1$C.Structure,
   Treatment= ifelse(is.na(r1$ED.Comparison1) & !is.na(r1$ED.Comparison2), r1$ED.Comparison2, r1$ED.Comparison1),
   yield_part= ifelse(is.na(r1$ED.Product.Comp) & !is.na(r1$ED.Product.Comp.L1), r1$ED.Product.Comp.L1, r1$ED.Product.Comp)

   )

### Adding fertilizer
rfo <- do.call(carobiner::bindr, era_merge["Fert.Out"])

rf <- r1[, names(rfo)]
dfert <- data.frame(
   N_organic= rf$F.NO,
   P_organic= rf$F.PO,
   K_organic= rf$F.KO,
   N_fertilizer= rf$F.NI,
   P_fertilizer= ifelse(is.na(rf$F.PI)& !is.na(rf$F.P2O5), rf$F.P2O5, rf$F.PI),
   K_fertilizer= ifelse(is.na(rf$F.KI)& !is.na(rf$F.K2O),  rf$F.K2O, rf$F.KI),
   fertilizer_type= rf$F.Level.Name2
)

d <- cbind(d, dfert)


#### Process plant.Method file 

## read plant.Method file 
rp <- do.call(carobiner::bindr, era_merge["Plant.Method"])

dpM <- data.frame(
   P.Level.Name= rp$P.Level.Name,
   planting_method= rp$Plant.Method,
   planting_implement= rp$Plant.Mechanization,
   plant_density= rp$Plant.Density,
   units= rp$Plant.Density.Unit,
   row_spacing= rp$Plant.Row,
   B.Code= rp$B.Code,
   Version= rp$Version
)


dpM$seed_rate <- dpM$seed_density <- NA
dpM$seed_density <- ifelse(grepl("^kg seed/ha$|^seeds/ha$|seed clusters/ha", dpM$units), dpM$plant_density,
                  ifelse(grepl("seed/m2|kg seed/m2|grains/m2|seeds/m2|seeds/m",dpM$units), dpM$plant_density*10000, dpM$seed_density))  

dpM$seed_rate <- ifelse(grepl("kg/ha", dpM$units), dpM$plant_density, dpM$seed_rate)
dpM$plant_density <- ifelse(grepl("plants/m3|plants/m6|plants/m|plants/m4|plants/m2|plants/m5|plants/m7|hill/m2", dpM$units), dpM$plant_density*10000, dpM$plant_density) 

i <- !is.na(dpM$seed_density) | !is.na(dpM$seed_rate)  
dpM$plant_density[i] <- NA

dpM$units <- NULL

## read plant.out file 
rpo <- do.call(carobiner::bindr, era_merge["Plant.Out"])

### merge plant out data and plant method data   
dpm <- merge(rpo, dpM, by= c("P.Level.Name", "B.Code", "Version"), all = TRUE)

### Adding all common variables in d 
d  <- cbind(d, r1[, names(rpo)]) 

## merge plant method data with d 
dmf <- merge(d, dpm, by= names(rpo), all = TRUE)

### removing usefulness variables  
dmf <- dmf[, !(names(dmf) %in% names(rpo))] 

### Removing missing variable in the outcome (value) and B_code 
dmf <-dmf[!is.na(dmf$variable) & !is.na(dmf$B_code),]



### Process soil.out file  

r2 <- do.call(carobiner::bindr, era_merge["Soil.Out"])

d1 <- data.frame(
   
   location= r2$Site.ID,
   soil_depth= paste0(r2$Soil.Upper, "-", r2$Soil.Lower),
   B_code= r2$B.Code,
   variable= r2$variable,
   value= r2$value
)

proc_soil <- function(f){
   
   d1 <- d1[d1$B_code==f,]
   
   ### Adding  step in the data to facilitate the transformation from long to wide 
   ds <- d1 %>%
      group_by(variable) %>%
      mutate(id = row_number()) %>%
      ungroup()
   df <- ds %>%
      pivot_wider(
         id_cols = names(ds)[!grepl("variable|value", names(ds))],
         names_from = variable ,        
         values_from = value      
      )
   
   return(df)
   
}

#### Append the data base on the study (B_code)
ff <- unique(d1$B_code)
ds <- lapply(ff, proc_soil)
ds <- do.call(carobiner::bindr, ds)
ds$id <- NULL
names(ds) <- gsub("Soil.", "soil_", names(ds))
i <- grep("CLY|SLT|SND|soil_AP", names(ds))
names(ds)[i] <-  c("soil_clay","soil_silt","soil_sand", "soil_P_available")

### merge soil information with dmf
dmf <- merge(dmf, ds, by=c("location","B_code"), all = TRUE)
### Removing missing variable in the outcome  
dmf <- dmf[!is.na(dmf$variable), ] 

### Process tillage file 

r3 <- do.call(carobiner::bindr, era_merge["Till.Out"])

d2 <- data.frame(
   Till.Level.Name= r3$Till.Level.Name,
   land_prep_method= ifelse(is.na(r3$T.Method)& !is.na(r3$Till.Other), r3$Till.Other, r3$T.Method),
   land_prep_implement= r3$T.Mechanization,
   location= r3$Site.ID,
   B_code= r3$B.Code,
   Time= r3$Time,
   version= r3$Version
)

## merge d2 and d
dt <- merge(dmf, d2, by=c("location","B_code", "Till.Level.Name", "Time", "version"), all = TRUE)
dt <- dt[!is.na(dt$variable), ] 
dt$Till.Level.Name <- NULL


### keep only rows with crop
dc <- dt[!grepl("Animal", dt$product_type),]

#### Transforming response variable from long into wide format 

proc <- function(f){
   
   dc <- dc[dc$B_code==f,]
   
### Adding  step in the data to facilitate the transformation from long to wide 
   df <- dc %>%
      group_by(variable) %>%
      mutate(id = row_number()) %>%
      ungroup()
   
   df_wide <- df %>%
      pivot_wider(
         id_cols = names(df)[!grepl("variable|Value", names(df))],
         names_from = variable ,        
         values_from = Value       
      )
   
   return(df_wide)
   
}

#### Append the data base on the study (B_code)
ff <- unique(dc$B_code)
dw <- lapply(ff, proc)

dw <- do.call(carobiner::bindr, dw)


### Change the era names  into carob standard names 
i <- which(names(dw) %in% c("Crop_Yield", "Soil_Moisture", "Soil_Organic_Carbon","Soil_Total_Nitrogen","Cation_Exchange_Capacity", "Crop_Residue_Yield","Aboveground_Biomass" , 
                            "Soil_Organic_Matter", "Soil_NH4", "Soil_NO3"))
names(dw)[i] <-  c("yield","soil_WHC_sat","soil_SOC", "soil_N","soil_CEC", "fwy_residue", "fwy_total", "soil_SOM", "soil_NH4", "soil_NO3")

### Keep suitable variables for carob 
Cnms <- names(dc)[!grepl("variable|Value", names(dc))]
respV <- c("yield", "Biomass_Yield", "fwy_residue","fwy_total", "soil_WHC_sat","soil_N")
df <- dw[,c(Cnms, respV)]

#### removing empty columns
df <- df[, colSums(!is.na(df)) > 0]
df[df=="-9999"] <- NA

### Fixing yield_part 

P <- carobiner::fix_name(df$yield_part)
P <- gsub("Grain/Seed", "grain", P)
P <- gsub("Biomass/Fodder - Above Ground", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Biomass/Fodder - Straw", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Pods", "pod", P)
P <- gsub("Haulm", "aboveground biomass", P)
P <- gsub("Biomass/Fodder - Above + Below Ground", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Tuber/Root \\(Total Yield\\)", "roots", P)
P <- gsub("Biomass/Fodder - Stover", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Bulb", "stems", P)
P <- gsub("Leaves", "leaves", P)
P <- gsub("Biomass/Fodder - Stalks", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Fibre/Lint", "fibre", P)
P <- gsub("Stalks\\+Leaves" , "leaves", P)
P <- gsub("Fruit \\(Total Yield\\)", "fruit", P)
P <- gsub("Fruit \\(Marketable Yield\\)", "fruit", P)
P <- gsub("Bulb \\(Unspecified\\)", "stems", P)
P <- gsub("Bulb \\(Non-Marketable Yield\\)", "stems", P)
P <- gsub("Bulb \\(Marketable Yield\\)", "stems", P)
P <- gsub("Tuber/Root \\(Unspecified\\)", "tubers", P)
P <- gsub("Biomass/Fodder - Stump", "aboveground biomass", P)
P <- gsub("Biomass/Fodder - Leaf Litter", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Fruit \\(Unspecified\\)", "fruit", P)
P <- gsub("Biomass/Fodder - Leaves + Stalks/Branches", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Stalks", "stems", P)
P <- gsub("Biomass/Fodder - Leaves", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Biomass/Fodder - Above + Below Ground", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Biomass/Fodder - leaves + stems/Branches", "aboveground biomass", P, fixed = TRUE)
P <- gsub("Biomass/Fodder", "aboveground biomass", P)
P <- gsub("Tuber/Root \\(Non-Marketable Yield\\)", "tubers", P)
P <- gsub("^\\s*Gum/Sap\\s*$", "none", P)
P <- gsub("Wood - Timber", "wood", P)
P <- gsub("Wood - Firewood", "wood", P)
P <- gsub("Tuber/Root", "roots", P)
P <- gsub("Tuber/Root \\(Marketable Yield\\)", "roots", P)
P <- gsub("stems+leaves", "stems", P, fixed = TRUE)
P <- gsub("Tuber/Root \\(Unspecified\\)", "roots", P)
P <- gsub("Whole Plant", "aboveground biomass", P)
P <- gsub("Stump|Leaf Litter|Below Ground", "", P)
P <- gsub("\\(Marketable Yield\\)|\\(Non-Marketable Yield\\)|\\(Total Yield\\)|\\(Tops\\)", "", P)
P <- gsub("\\(Unspecified\\)", "", P)
P <- gsub("Flowers", "flowers", P)
P <- gsub("Nuts|Baby corn", "grain", P)
P <- gsub("Corm |Cormel |^\\s*Stem/stems\\s*$", "stems", P)
P <- gsub("aboveground biomass/Branches", "aboveground biomass", P)
P <- gsub("biomassleaves", "biomass", P, fixed = TRUE)
P <- gsub(" - ", "", P)
P <- gsub("Unspecified|gh", "none", P)
df$yield_part <- P





