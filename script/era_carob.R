
library(tidyr)
library(dplyr)

load("~/era-carob/ERA_data/agronomic_majestic-hippo-2020-2025-03-19.2_industrious-elephant-2023-2025-03-19.1.RData")

### Baseline data 
rb <-  do.call(carobiner::bindr, era_merge["Data.Out"])

#### adding missing variables from prod.out to Data.out
r5 <- do.call(carobiner::bindr, era_merge["Prod.Out"])
rb <- merge(rb, r5,  by= intersect(names(rb), names(r5)), all = TRUE)

### Adding Tillage variables from Till.out to Data.out
r7 <- do.call(carobiner::bindr, era_merge["Till.Out"])
rb <- merge(rb, r7,  by= intersect(names(rb), names(r7)), all = TRUE)

### Adding planting management information 
r9 <- do.call(carobiner::bindr, era_merge["Plant.Method"])
rb <- merge(rb, r9,  by= intersect(names(rb), names(r9)), all = TRUE)

### Adding fertilizer method
r12 <- do.call(carobiner::bindr, era_merge["Fert.Method"])
rb <- merge(rb, r12,  by= intersect(names(rb), names(r12)), all = TRUE)

### Adding chemical elements 
r15 <- do.call(carobiner::bindr, era_merge["Chems.Out"])
rb <- merge(rb, r15,  by= intersect(names(rb), names(r15)), all = TRUE)

#### residue 
r18 <-  unclass(do.call(carobiner::bindr, era_merge["Res.Method"]) )
rb <- merge(rb, r18,  by= intersect(names(rb), names(r18)), all = TRUE)

## residue composition 
r19 <-  do.call(carobiner::bindr, era_merge["Res.Comp"]) 
rb <- merge(rb, r19,  by= intersect(names(rb), names(r19)), all = TRUE)

#### 
r21 <-  do.call(carobiner::bindr, era_merge["pH.Out"]) 
rb <- merge(rb, r21,  by= intersect(names(rb), names(r21)), all = TRUE)

####
r22 <-  do.call(carobiner::bindr, era_merge["pH.Method"]) 
rb <- merge(rb, r22,  by= intersect(names(rb), names(r22)), all = TRUE)

##### irrigation
r23 <-  do.call(carobiner::bindr, era_merge["Irrig.Method"])
rb <- merge(rb, r23,  by= intersect(names(rb), names(r23)), all = TRUE)

###
r25 <-  do.call(carobiner::bindr, era_merge["WH.Out"])
rb <- merge(rb, r25,  by= intersect(names(rb), names(r25)), all = TRUE)

### Add soil data 
r2 <- do.call(carobiner::bindr, era_merge["Soil.Out"])
d1 <- data.frame(
   Site.ID= r2$Site.ID,
   soil_depth= paste0(r2$Soil.Upper, "-", r2$Soil.Lower),
   B.Code= r2$B.Code,
   variable= r2$variable,
   value= r2$value
)

proc_soil <- function(f){
   
   d2 <- d1[d1$B.Code==f,]
   
   ### Adding  step in the data to facilitate the transformation from long to wide 
   ds <- d2 %>%
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
ff <- unique(r2$B.Code)
ds <- lapply(ff, proc_soil)
ds <- do.call(carobiner::bindr, ds)
ds$id <- NULL
names(ds) <- gsub("Soil.", "soil_", names(ds))
i <- grep("CLY|SLT|SND|soil_AP", names(ds))
names(ds)[i] <-  c("soil_clay","soil_silt","soil_sand", "soil_P_available")

rb <- merge(rb, ds, by= intersect(names(rb), names(ds)), all= TRUE)
rb <- rb[!grepl("All Sites", rb$Site.ID),]
### keep only rows with crop
df <- rb[!grepl("Animal", rb$Product.Type),]
#df$control <- ifelse(grepl("Yes", substr(df$T.Control, 1, 3)), TRUE, FALSE)
#df$treatment <- ifelse(is.na(df$T.Name) & !is.na(df$F.Level.Name), df$F.Level.Name, df$T.Name)

df$Out.Subind <- gsub(" ", "_", df$Out.Subind)
cols <- c("PD.Plant.Start", "PD.Plant.End", "PD.Harvest.Start", "PD.Harvest.End")
df[cols] <- lapply(df[cols], unclass)

d <- data.frame(
   uri=df$ B.DOI,
   reference= df$B.Author.Last,
   dataset_id= df$B.Code,
   location= df$Site.ID,
   on_farm= df$Site.Type,
   country= df$Country,
   latitude= df$Site.LatD,
   longitude= df$Site.LonD,
   rain= df$Site.MAP,
   #temp= df$Site.MAT,
   elevation= df$Site.Elevation,
   soil_type= df$Site.Soil.Texture,
   year= substr(df$Time, 1, 4),
   seasonal_prep= df$Time.Clim.SP,
   total_prec= df$Time.Clim.TAP,
   temp= df$Time.Clim.Temp.Mean,
   tmax= df$Time.Clim.Temp.Max,
   tmin= df$Time.Clim.Temp.Min,
   dsign= df$EX.Design,
   plot_area= df$EX.HPlot.Size,
   treatment= ifelse(is.na(df$T.Name) & !is.na(df$F.Level.Name), df$F.Level.Name, df$T.Name),
   control_T= ifelse(grepl("Yes", substr(df$T.Control, 1, 3)), TRUE, FALSE),
   crop= df$P.Product,
   harvest_date= df$PD.Harvest.Start,
   harvest_end= df$PD.Harvest.End,
   planting_date= df$PD.Plant.Start,
   planting_end= df$PD.Plant.End,
   tillage= df$Till.Level.Name,
   land_prep_method= ifelse(is.na(df$T.Method)& !is.na(df$Till.Other), df$Till.Other, df$T.Method),
   land_prep_implement= df$T.Mechanization,
   variable= df$Out.Subind,
   yield_part= ifelse(is.na(df$ED.Product.Comp) & !is.na(df$ED.Product.Comp.L1), df$ED.Product.Comp.L1, df$ED.Product.Comp),
   value= df$ED.Mean.T,
   rep= ifelse(is.na(df$T.Reps) & !is.na(df$ED.Reps), df$ED.Reps, df$T.Reps),
   #df$T.Residue.Prev,
   variety= ifelse(is.na(df$V.Var) & !is.na(df$ED.Variety), df$ED.Variety, df$V.Var),
   maturity_days= df$V.Maturity,
   N_organic= df$F.NO,
   P_organic= df$F.PO,
   K_organic= df$F.KO,
   N_fertilizer= df$F.NI,
   P_fertilizer= ifelse(is.na(df$F.PI)& !is.na(df$F.P2O5), df$F.P2O5, df$F.PI),
   K_fertilizer= ifelse(is.na(df$F.KI)& !is.na(df$F.K2O),  df$F.K2O, df$F.KI),
   #fert_org_unit= df$F.O.Unit,
   #fert_Io_unit= df$F.I.Unit,
   irrigation_amount= df$I.Method,
   irrigation_date= ifelse(is.na(df$I.Date.Start) & !is.na(df$I.Date.Gen), df$I.Date.Gen, df$I.Date.Start),
   irrigation_date_end= df$I.Date.End,
   irrrigated= ifelse(is.na(df$I.Amount), FALSE, TRUE),
   planting_method= df$Plant.Method,
   planting_implement= df$Plant.Mechanization,
   plant_density= df$Plant.Density,
   units= df$Plant.Density.Unit,
   row_spacing= df$Plant.Row,
   intercrops=df$IN.Prod,
   crop_rotation= df$R.Prod.Seq,
   herbicide_used= ifelse(grepl("Herbicide", df$C.Type), TRUE, FALSE),
   herbicide_method= ifelse(grepl("Herbicide", df$C.Type), df$C.App.Method, "none"),
   herbicide_implement= ifelse(grepl("Herbicide", df$C.Type), df$C.Mechanization, "none"),
   herbicide_amount= ifelse(grepl("Herbicide", df$C.Type),df$C.Amount, 0),
   herbicide_product= ifelse(grepl("Herbicide", df$C.Type), df$C.Name, "none"),
   
   
   insecticide_used= ifelse(grepl("Insecticide|Bioinsecticide", df$C.Type), TRUE, FALSE),
   insecticide_method= ifelse(grepl("Insecticide|Bioinsecticide", df$C.Type), df$C.App.Method, "none"), 
   insecticide_implement= ifelse(grepl("Insecticide|Bioinsecticide", df$C.Type), df$C.Mechanization, "none"), 
   insecticide_amount= ifelse(grepl("Insecticide|Bioinsecticide", df$C.Type), df$C.Amount, 0), 
   insecticide_product= ifelse(grepl("Insecticide|Bioinsecticide", df$C.Type), df$C.Name, "none"),
   
   
   fungicide_used= ifelse(grepl("Fungicide|Biofungicide", df$C.Type), TRUE, FALSE),
   fungicide_method= ifelse(grepl("Fungicide|Biofungicide", df$C.Type), df$C.App.Method, "none"), 
   fungicide_implement= ifelse(grepl("Fungicide|Biofungicide", df$C.Type), df$C.Mechanization, "none"), 
   fungicide_amount= ifelse(grepl("Fungicide|Biofungicide", df$C.Type), df$C.Amount, 0), 
   fungicide_product= ifelse(grepl("Fungicide|Biofungicide", df$C.Type), df$C.Name, "none"),
   
   
   pesticide_used= ifelse(grepl("Biopesticide", df$C.Type), TRUE, FALSE),
   pesticide_used_method= ifelse(grepl("Biopesticide", df$C.Type), df$C.App.Method, "none"), 
   pesticide_used_implement= ifelse(grepl("Biopesticide", df$C.Type), df$C.Mechanization, "none"), 
   pesticide_used_amount= ifelse(grepl("Biopesticide", df$C.Type), df$C.Amount, 0), 
   pesticide_product= ifelse(grepl("Biopesticide", df$C.Type), df$C.Name, "none"),
   
   soil_clay= df$soil_clay,
   soil_depth= df$soil_depth,
   soil_sand= df$soil_sand,
   soil_silt= df$soil_silt
)


### fixing seed density unit 

d$seed_rate <- d$seed_density <- NA
d$seed_density <- ifelse(grepl("^kg seed/ha$|^seeds/ha$|seed clusters/ha", d$units), d$plant_densit,
                         ifelse(grepl("seed/m2|kg seed/m2|grains/m2|seeds/m2|seeds/m",d$units), d$plant_density*10000, d$seed_density))  

d$seed_rate <- ifelse(grepl("kg/ha", d$units), d$plant_density, d$seed_rate)
d$plant_density <- ifelse(grepl("plants/m3|plants/m6|plants/m|plants/m4|plants/m2|plants/m5|plants/m7|hill/m2", d$units), d$plant_density*10000, d$plant_density) 

i <- !is.na(d$seed_density) | !is.na(d$seed_rate)  
d$plant_density[i] <- NA
d$units <- NULL

d$crop_rotation <- gsub("\\|+", ";", d$crop_rotation)

d$treatment_type <- ifelse(grepl("TRUE", d$control_T), "control", "treatment")

### Fixing country
d$country <- ifelse(grepl("Uganda", d$country), "Uganda", 
             ifelse(grepl("Ethiopia", d$country), "Ethiopia", 
             ifelse(grepl("Malawi", d$country), "Malawi", 
             ifelse(grepl("Tanzania", d$country), "Tanzania", 
             ifelse(grepl("Benin..Togo", d$country), "Benin", 
             ifelse(grepl("Ghana..Benin", d$country), "Ghana", 
             ifelse(grepl("Kenya..Kenya", d$country), "Kenya", 
             felse(grepl("Drc|Congo", d$country), "Democratic Republic of Congo", d$country))))))))

### Fixing intercrops

split <- strsplit(d$intercrops, "\\*\\*\\*")

# Find the max number of parts in any row
max_len <- max(sapply(split, length))

# Pad each list element to the same length with NAs
split_padded <- lapply(split, function(x) {
   length(x) <- max_len
   return(x)
})

# Now you can safely bind rows
inter <- as.data.frame(do.call(rbind, split_padded), stringsAsFactors = FALSE)
inter$V3 <- ifelse(grepl("Mangifera indica", inter$V3), "Mangifera", 
            ifelse(grepl("Leucaena leucocephala", inter$V3), "Leucaena", inter$V3))
inter$V2 <- ifelse(grepl("Poupartia silvatica", inter$V2), "Poupartia silvatica", 
            ifelse(grepl("Maize..Barley", inter$V2), "maize",
            felse(grepl("Terminalia ivoresensis", inter$V2), "Terminalia ivoresensis", 
            ifelse(grepl("Pearl Millet", inter$V2), "Pearl Millet", 
            ifelse(grepl("Mango..Papaya", inter$V2), "Mango", inter$V2)))))

d$intercrops <- tolower(ifelse(!is.na(inter$V3) & !is.na(inter$V4), paste(inter$V2, inter$V3, inter$V4, sep = ";"), 
                        ifelse(!is.na(inter$V3) & is.na(inter$V4), paste(inter$V2, inter$V3, sep = ";"), inter$V2)))




##############################################################
#### Transforming response variable from long into wide format ##########

proc <- function(f, dc){
   
   dc <- dc[dc$dataset_id==f,]
   
   ### Adding  step in the data to facilitate the transformation from long to wide 
   df <- dc %>%
      group_by(variable) %>%
      mutate(id = row_number()) %>%
      ungroup()
   
   df_wide <- df %>%
      pivot_wider(
         id_cols = names(df)[!grepl("value|variable", names(df))],
         names_from = variable ,        
         values_from = value       
      )
   df <- df_wide[order(df_wide$location), ]
   df <- df[, colSums(!is.na(df)) > 0]
   
      
   return(df)
   
}

#### Append the data base on the study (B_code)
ff <- unique(d$dataset_id)
dw <- lapply(ff, function(y) proc(y, d))

dwf <- do.call(carobiner::bindr, dw)

i <- grep(paste("Crop_Yield", "Soil_Organic_Carbon", "Soil_Total_Nitrogen", "Soil_Nitrogen", "Soil_Organic_Matter", "Carbon_Dioxide_Emissions", "Soil_Organic_Carbon_(Change)", sep = "|"), names(dwf))
names(dwf)[i] <- c("yield","soil_SOC", "soil_total_N", "soil_N", "soil_SOM", "soil_CO2", "soil_ex_SOC")


### fixing tillage
dwf$land_prep_method <- ifelse(is.na(dwf$land_prep_method) & !is.na(dwf$tillage), dwf$tillage, dwf$land_prep_method)

### Fixing land prep 
dwf$land_prep <- dwf$land_prep_method
dwf$land_prep_method <- tolower(ifelse(grepl("CT|CONV|ConvTill|conv|CON|Conservation|Direct|Conv|CA|Conventional", dwf$land_prep_method), "conventional", 
                               ifelse(grepl("MT|Min Till", dwf$land_prep_method), "minimum tillage",
                               ifelse(grepl("Disc", dwf$land_prep_method), "disk tillage", 
                               ifelse(grepl("NT|ZT|no-till|NoTill|Control|No till|No Till|No-till|T0|FlatUntill", dwf$land_prep_method), "zero tillage", 
                               ifelse(grepl("Ridge|Ridging|ridge|ridging", dwf$land_prep_method), "ridge tillage", 
                               ifelse(grepl("RT|reduced|Reduced", dwf$land_prep_method), "reduced tillage",
                               ifelse(grepl("Chiseling", dwf$land_prep_method), "deep ploughing",
                               ifelse(grepl("Hand Hoe|Hand hoe", dwf$land_prep_method), "hoeing", 
                               ifelse(grepl("Ploughed|Plough|plough", dwf$land_prep_method), "ploughing", 
                               ifelse(grepl("Rotary Plough", dwf$land_prep_method), "rotovating",
                               ifelse(grepl("Mali Tillage|BF Tillage|Niger Tillage|Tillage|Till|Maize till|RedTill|FlatTill|Plow-till", dwf$land_prep_method), "tillage", 
                               ifelse(grepl("Planting Basins|Basins|BASINS", dwf$land_prep_method), "basins", 
                               ifelse(grepl("Non-puddling NP", dwf$land_prep_method), "not puddled", 
                               ifelse(grepl("Puddling P", dwf$land_prep_method), "puddled", dwf$land_prep_method)))))))))))))))






