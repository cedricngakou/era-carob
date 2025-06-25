### R script to transform ERA data into carob standard data 

library(tidyr)
library(dplyr)


load("~/era-carob/ERA data/agronomic_majestic-hippo-2020-2025-03-19.2_industrious-elephant-2023-2025-03-19.1.RData")

### read data 
r1 <- do.call(carobiner::bindr, era_merge["Data.Out"])

### Process Data.out file 

d <- data.frame(
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
   variety= r1$ED.Variety,
   intercrop= r1$IN.Prods.All,
   product_type= r1$Product.Type,
   version= r1$Version,
   #planting_method= r1$PD.Plant.Variable,
   planting_date= unclass(r1$PD.Plant.Start),
   planting_date_End=  unclass(r1$PD.Plant.End),
   harvest_date= unclass(r1$PD.Harvest.Start),
   harvest_date_End= unclass(r1$PD.Harvest.End),
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
   variety_type= r1$V.Crop.Practice,
   land_prep_method= r1$Till.Level.Name,
   weeding_method= r1$W.Method,
   weeding_times= r1$W.Freq,
   herbicide_product= r1$C.Structure,
   Treatment= ifelse(is.na(r1$ED.Comparison1) & !is.na(r1$ED.Comparison2), r1$ED.Comparison2, r1$ED.Comparison1),
   yield_part= ifelse(is.na(r1$ED.Product.Comp) & !is.na(r1$ED.Product.Comp.L1), r1$ED.Product.Comp.L1, r1$ED.Product.Comp)

   )

#### Adding plant method

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

#### Transforming response variable from long into wide format 
proc <- function(f){
   
   d <- d[d$B_code==f,]
   
### Adding  step in the data to facilitate the transformation from long to wide 
   df <- d %>%
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
ff <- unique(d$B_code)
dw <- lapply(ff, proc)

dw <- do.call(carobiner::bindr, dw)

#### removing empty columns
df <- dw[, colSums(!is.na(dw)) > 0]
df[df=="-9999"] <- NA

### Change the era names code into carob standard names 
i <- which(names(df) %in% c("Crop_Yield", "Soil_Moisture", "Soil_Organic_Carbon","Soil_Total_Nitrogen","Cation_Exchange_Capacity", "Crop_Residue_Yield","Aboveground_Biomass" , "Soil_Organic_Matter", "Soil_NH4", "Soil_NO3"))
names(df)[i] <-  c("yield","soil_WHC_sat","soil_SOC", "soil_N","soil_CEC", "fwy_residue", "fwy_total", "soil_SOM", "soil_NH4", "soil_NO3")

### Keep suitable variables for carob 
df <- cbind(df[,c(1:36),], df[, c("yield","soil_WHC_sat","soil_SOC", "soil_N","soil_CEC", "fwy_residue", "fwy_total", "soil_SOM", "soil_NH4", "soil_NO3", "Biomass_Yield")])


#### Keep only rows with crop yield value 
df <- df[!is.na(df$yield),]


### Process soil information  suitable for Soil_data group in carob 

r2 <- do.call(carobiner::bindr, era_merge[3])

d1 <- data.frame(
   
   location= r2$Site.ID,
   soil_depth= paste0(r2$Soil.Upper, "-", r2$Soil.Lower),
   B_code= r2$B.Code,
   variable=  gsub(" ", "_",r2$variable),
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


