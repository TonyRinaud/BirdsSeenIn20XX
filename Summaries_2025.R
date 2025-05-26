
####
# Libraries ----
library(easypackages)

easypackages::libraries("bulkreadr","lubridate","googledrive","googlesheets4",
                        "PDE","tidyr","stringr","dplyr","knitr","gridExtra",
                        "PVplr","rstudioapi","grid","kableExtra","magick","readxl",
                        "matrixStats","tidyverse","glue","grDevices","scales")

setwd('Ornitho/BirdsSeenIn2024 challenge/')

####
# Import datasets ----

africa=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 1, col_types = "c")
2
africa=africa[order(africa$`Species (Scientific)`),]

nc_america=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 2, col_types = "c")
nc_america=nc_america[order(nc_america$`Species (Scientific)`),]

s_america=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 3, col_types = "c")
s_america=s_america[order(s_america$`Species (Scientific)`),]

asia=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 4, col_types = "c")
asia=asia[order(asia$`Species (Scientific)`),]

europe=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 5, col_types = "c")
europe=europe[order(europe$`Species (Scientific)`),]

oceania=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 6, col_types = "c")
oceania=oceania[order(oceania$`Species (Scientific)`),]

list_species=googlesheets4::read_sheet("1rkRaT79sKbdOnC046sGoDyfgWydIFwIpdTcSfK3qs2w", 7, col_types = "c")
list_species=list_species[,c(1,2)]
list_species=na.omit(list_species)
list_species=list_species[order(list_species$`Species (Scientific)`),]

# Merge datasets 

world=left_join(list_species, africa, by=c(names(africa)[1],names(africa)[2]))
world=left_join(world, s_america, by=c(names(africa)[1],names(africa)[2]))
world=left_join(world, nc_america, by=c(names(africa)[1],names(africa)[2]))
world=left_join(world, asia, by=c(names(africa)[1],names(africa)[2]))
world=left_join(world, europe, by=c(names(africa)[1],names(africa)[2]))
world=left_join(world, oceania, by=c(names(africa)[1],names(africa)[2]))

world = world %>% mutate_if(is.character,as.factor)

# Checking for mistyping

for (p in 3:247){
  
  d=levels(world[[p]])
  
  if(all_na(world[,p])){
    next
  }
  if(all(d %in% c("21","22","23","24","21_22","21_23","21_24", "22_23","22_24",
                  "23_24", "21_22_23","21_22_24", "21_23_24", "22_23_24", 
                  "21_22_23_24"))){
    next
  }
  
  else{
    
    stop("Wrong value(s) in column '",names(world[p]),"'")
  }
}


####
# Top 40 & more ----

## Top 40 ----

top=NA
top24=NA
Countries=names(world)[3:247]

for (i in 3:ncol(world)){
  
  
  if(is.integer(grep("24", world[[i]])) && length(grep("24", world[[i]])) == 0){
    
    top[i-2]=NA
    top24[i-2]=NA
    
  }
  
  else {
    
    p=grep("24", world[[i]])
    q=grep("2[0-9]", world[[i]])
    
    top[i-2]=paste(length(world[p,][[i]])," (",length(world[q,][[i]]),")", sep = '')
    top24[i-2]=length(world[p,][[i]]) 
    
  }
}

df=data.frame(Countries,top, top24)
df$Countries=as.factor(df$Countries)
names(df)[2]='# Species \n(total 21-24)'
df=df[order(df$top24,decreasing = T),]
df=df[,-3]

df=na.omit(df)
row.names(df)=seq(1,nrow(df),1)

df2=cbind(df[c(1:10),],df[c(11:20),],df[c(21:30),],df[c(31:40),])
# row.names(df2)=seq(1,nrow(df2),1)
names(df2)=c('Top 1-10','# Species \n(total 21-24)', 'Top 11-20', '#  Species \n(total 21-24)',
             'Top 21-30',' # Species \n(total 21-24)', 'Top 31-40', '# Species \n(total 21-24) ')

# Top 40 pdf

png(paste("Tops/A1_top40","_",Sys.Date(),".png", sep = ""), height = 750, width = 2200, res = 200)
grid.table(df2)
dev.off()


# k40=kable(df2, col.names = c("Countries","# Species","Countries","# Species",
#                              "Countries","# Species","Countries","# Species"),
#           align = 'c', 
#           row.names = TRUE,
#           caption = paste('Top 40 of #BirdsSeenIn2024 Hashtag Challenge - ',Sys.Date(),''),) %>%
#    kable_classic_2(full_width = F) %>%
#    kable_styling(full_width = T,
#                 font_size = 15, html_font = 'sans-serif', 
#                 bootstrap_options = c("striped", "hover", "condensed")) %>%
#   row_spec(0,bold = T, font_size = 18) %>%
#    as_image(., height = 4, width = 11, file = "Tops/top40.png")


## Top 41 - 157 pdf ----

df3=cbind(df[c(41:78),], df[c(79:116),], df[c(117:154),],df[c(155:192),])
row.names(df3)=seq(41, 78, 1)
names(df3)=c('Top 41-78','# Species \n(total 21-24)', 'Top 79-116', '#  Species \n(total 21-24)',
             'Top 117-154',' # Species \n(total 21-24)', 'Top 155-192', '# Species \n(total 21-24) ')

png(paste("Tops/A2_top41_192","_",Sys.Date(),".png", sep = ""), 
    height = 2400, 
    width = 3400, 
    res = 200)
grid.table(df3)
dev.off()


####
# Evolution species during year ----

tally_world=readxl::read_excel("Tally_world_2025-01-01.xlsx")

# Summary per country ----

# Delete previous files in folder

fn=list.files("Country lists",full.names = T)

# Check its existence & delete file if it exists

if (all(file.exists(fn))) {      
  
  file.remove(fn)
  
}

# Progress bar 

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(names(world)[3:247]),      # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")

key=0

for (i in names(world)[3:247]){
  
  if(all_na(world[,which(names(world) %in% i)])){ next }
  
  else {
  
    key=key+1
    
  column=as.numeric(which(names(world) %in% i))
  
  my_table = data.frame(world$`Species (Scientific)`[grep("2[0-9]",
                                                           world[[column]])], 
                       world$`Species (English)`[grep("2[0-9]", 
                                                      world[[column]])], 
                       as.character(world[grep("2[0-9]",
                                  world[[column]]),][[column]]),
                       NA)
  
# Changing column names

names(my_table) = c("Species (Scientific)", "Species (English)", 
                    paste("Recorded for",i,"in #BirdsSeenIn2024 challenge"), 
                    'Recorded in the challenge since 2021')

# Adding all-year records column

if(TRUE %in% (my_table[,3] %in% "21_22_23_24")){
  
  my_table[which(my_table[,3] %in% "21_22_23_24"),4] = "All years"
  
  my_table[-c(which(my_table[,3] %in% "21_22_23_24")),4] = paste("In 20", 
          gsub("_", " & ", my_table[-c(which(my_table[,3] %in% "21_22_23_24")),3]),
          sep = "")
  
   }else{
    
  my_table[,4] = paste("In 20", gsub("_", " & ",
                                            my_table[,3]),
                    sep = "")
   }

# Adding species names in 2024 records column

my_table[grep("24", my_table[,3]), 3] = paste(my_table$`Species (Scientific)`[grep("24", my_table[,3])],
                                              '-', 
                                              my_table$`Species (English)`[grep("24", my_table[,3])])
my_table[grep("2[0-9]", my_table[,3]), 3] = ""

# Adding the total number of species to tally_world

# tally_world[months(Sys.Date()),which(names(tally_world) %in% i)]=length(my_table$`Species (Scientific)`[which(!my_table[[3]] %in% "")])

# Printing the summary in pdf to ./Country_lists

long=nrow(my_table)
npage=ceiling(long/67)

pdf(paste("Country lists/",i,"_",length(my_table[which(!my_table[[3]] %in% ""),3])
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=16)

if(long < 68){
  
idx= seq(1:nrow(my_table))
  
grid.table(my_table[idx,])

}

if (long >= 68){
  
  idx= seq(1:67)

  grid.table(my_table[idx,])
  
  for (k in 2:npage){
  
    grid.newpage()
  
    if(k*67 <= nrow(my_table)){
    
      idx= seq(1+((k-1)*67), k*67)}
  
    else {
      idx= seq(1+((k-1)*67), nrow(my_table))
    }  
    
    grid.table(my_table[idx,])
    
  }

}

for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

  }
  
  setTxtProgressBar(pb, as.numeric(which(names(world) %in% i)))
  
}

## Upload files into drive folder ----
# 

# Remove old files from previous week in googledrive

drive_files=drive_ls(as_dribble("https://drive.google.com/drive/folders/1KXdrj9HOSSVZjec0LyxgoB18u_nNYwsr"))
2

drive_files=drive_files[order(drive_files$name),]

drive_files=drive_files$name[-c(grep("^0[0-9]_",drive_files$name))]

drive_files=drive_files[-c(grep("Country species 2021-2024",drive_files))]

drive_rm(drive_files)

local_files = list.files("~/Ornitho/BirdsSeenIn2024 challenge/Country lists/", full.names = T)
# local_files=local_files[-c(grep("^0[0-9]_", drive_files$name))]

files= map(local_files, 
           ~drive_upload(.x, 
                         path = as_dribble("https://drive.google.com/drive/folders/1KXdrj9HOSSVZjec0LyxgoB18u_nNYwsr")))

# Blank lists - Continents ----

## All time ---- 

blank_world <- world[!apply(world, 1, function(x) {any(x %in% c("21","22","23",
  "24","21_22","21_23","21_24", "22_23","22_24","23_24", "21_22_23","21_22_24",
  "21_23_24", "22_23_24","21_22_23_24"))}),]

blank_world=blank_world[,c(1,2)]

long=nrow(blank_world)
npage=ceiling(long/67)

pdf(paste("Country lists/07_Blanks_World_2021-2024_",nrow(blank_world)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.7)

idx= seq(1:67)

grid.table(blank_world[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_world)){
    
    idx=seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx=seq(1+((k-1)*67), nrow(blank_world))
  }
  
  grid.table(blank_world[idx,], rows=idx)
  
}

for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}


## 2024 ----

### World ----

blank_world_24 <- world[!apply(world, 1, function(x) {any(x %in% c("24","21_24",
            "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_world_24=blank_world_24[,c(1,2)]

long=nrow(blank_world_24)
npage=ceiling(long/67)

pdf(paste("Country lists/08_Blanks_World_2024_",nrow(blank_world_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.7)

idx= seq(1:67)

grid.table(blank_world_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_world_24)){
    
    idx=seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx=seq(1+((k-1)*67), nrow(blank_world_24))
  }
  
  grid.table(blank_world_24[idx,], rows=idx)
  
}

for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

### Africa ----

blank_africa_24 <- africa[!apply(africa, 1, function(x) {any(x %in% c("24","21_24",
            "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_africa_24=blank_africa_24[,c(1,2)]

long=nrow(blank_africa_24)
npage=ceiling(long/67)

pdf(paste("Country lists/01_Blanks_2024_Africa_",nrow(blank_africa_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_africa_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_africa_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_africa_24))
  }
  
  grid.table(blank_africa_24[idx,], rows=idx)
  
}

### Asia ----

for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

blank_asia_24 <- asia[!apply(asia, 1, function(x) {any(x %in% c("24","21_24",
         "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_asia_24=blank_asia_24[,c(1,2)]

long=nrow(blank_asia_24)
npage=ceiling(long/67)

pdf(paste("Country lists/02_Blanks_2024_Asia_",nrow(blank_asia_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_asia_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_asia_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_asia_24))
  }
  
  grid.table(blank_asia_24[idx,], rows=idx)
  
}


for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

### Europe ----

blank_europe_24 <- europe[!apply(europe, 1, function(x) {any(x %in% c("24","21_24",
             "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_europe_24=blank_europe_24[,c(1,2)]

long=nrow(blank_europe_24)
npage=ceiling(long/67)

pdf(paste("Country lists/06_Blanks_2024_Europe_",nrow(blank_europe_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_europe_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_europe_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_europe_24))
  }
  
  grid.table(blank_europe_24[idx,], rows=idx)
  
}


for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

### N America ----

blank_nc_america_24 <- nc_america[!apply(nc_america, 1, function(x) {any(x %in% c("24","21_24",
            "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_nc_america_24=blank_nc_america_24[,c(1,2)]

long=nrow(blank_nc_america_24)
npage=ceiling(long/67)

pdf(paste("Country lists/03_Blanks_2024_North_America_",nrow(blank_nc_america_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_nc_america_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_nc_america_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_nc_america_24))
  }
  
  grid.table(blank_nc_america_24[idx,], rows=idx)
  
}


for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

### Oceania ----

blank_oceania_24 <- oceania[!apply(oceania, 1, function(x) {any(x %in% c("24","21_24",
            "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_oceania_24=blank_oceania_24[,c(1,2)]

long=nrow(blank_oceania_24)
npage=ceiling(long/67)

pdf(paste("Country lists/04_Blanks_2024_Oceania_",nrow(blank_oceania_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_oceania_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_oceania_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_oceania_24))
  }
  
  grid.table(blank_oceania_24[idx,], rows=idx)
  
}


for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

### S America ----

blank_s_america_24 <- s_america[!apply(s_america, 1, function(x) {any(x %in% c("24","21_24",
            "22_24","23_24","21_22_24","21_23_24","22_23_24","21_22_23_24"))}),]

blank_s_america_24=blank_s_america_24[,c(1,2)]

long=nrow(blank_s_america_24)
npage=ceiling(long/67)

pdf(paste("Country lists/05_Blanks_2024_South_America_",nrow(blank_s_america_24)
          ,"_species_",Sys.Date(),".pdf", sep=""), height=20, width=5.5)

idx= seq(1:67)

grid.table(blank_s_america_24[idx,])

for (k in 2:npage){
  
  grid.newpage()
  
  if(k*67 <= nrow(blank_s_america_24)){
    
    idx= seq(1+((k-1)*67), k*67)
    
  }
  
  else {
    idx= seq(1+((k-1)*67), nrow(blank_s_america_24))
  }
  
  grid.table(blank_s_america_24[idx,], rows=idx)
  
}


for (j in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}


# Complete species 2021-2024 ----

## Top 40 ----

top=NA
top24=NA
Countries=names(world)[3:247]

for (i in 3:ncol(world)){
  
  
  if(is.integer(grep("21_22_23_24", world[[i]])) && length(grep("21_22_23_24", world[[i]])) == 0){
    
    top[i-2]=NA
    top24[i-2]=NA
    
  }
  
  else {
    
    p=grep("21_22_23_24", world[[i]])
    q=grep("21_22_23", world[[i]])
    
    if(length(world[p,][[i]]) == length(world[q,][[i]])){
     
      top[i-2]=paste('Complete'," (",length(world[q,][[i]]),")", sep = '')
      top24[i-2]=length(world[p,][[i]])  
      
    }else{
    
    top[i-2]=paste(length(world[p,][[i]])," (",length(world[q,][[i]]),")", sep = '')
    top24[i-2]=length(world[p,][[i]]) 
    
  }
  }
}

df=data.frame(Countries,top, top24)
df$Countries=as.factor(df$Countries)

df=df[order(df$top24,decreasing = T),]
df=df[,-3]

df=na.omit(df)
row.names(df)=seq(1,nrow(df),1)

df3=cbind(df[c(1:18),],df[c(19:36),],df[c(37:54),],df[c(55:72),])
names(df3)=c('Top 1-18','Species seen all years \n (possible species)', 'Top 19-36', 
            ' Species seen all years \n (possible species)',
             'Top 37-54',' Species seen all years \n (possible species) ', 'Top 55-72', 
            'Species seen all years \n  (possible species)')

tt <- ttheme_minimal()

# Top  pdf

png(paste("Tops/A3_top72_complete_species","_",Sys.Date(),".png", sep = ""), 
    height = 1200, 
    width = 3200, 
    res = 200)

grid.table(df3, theme=tt)
dev.off()

# Country number species per year ----

# Delete previous files in folder

fn=list.files("~/Ornitho/BirdsSeenIn2024 challenge/Evol country/", full.names = T)

# Check its existence & delete file if it exists

if (all(file.exists(fn))) {      
  
  file.remove(fn)
  
}

Y2021_sp=rep(NA,245)
Y2022_sp=rep(NA,245)
Y2023_sp=rep(NA,245)
Y2024_sp=rep(NA,245)
nb_sp=data.frame(Y2021_sp,Y2022_sp,Y2023_sp,Y2024_sp)

for(i in 3:length(world)){
  
  nb_sp$Y2021_sp[i-2]=length(grep("21",world[[i]]))
  nb_sp$Y2022_sp[i-2]=length(grep("22",world[[i]]))
  nb_sp$Y2023_sp[i-2]=length(grep("23",world[[i]]))
  nb_sp$Y2024_sp[i-2]=length(grep("24",world[[i]]))
  
}

rownames(nb_sp)=names(world)[3:247]

nb_sp=t(nb_sp)
nb_sp=as.data.frame(nb_sp)

for (i in 1:length(nb_sp)){
  
  x1=ggplot(nb_sp)+
    aes(c(2021,2022,2023,2024),nb_sp[[i]])+
    geom_col(fill = "lightyellow", colour = 'black', width = 0.5)+
    geom_label(aes(label=nb_sp[[i]]), size=2)+
    # geom_line(aes(c(2021,2022,2023,2024),rowMeans(x = nb_sp[4,])),
    #           colour = 'red')+
    scale_y_continuous(breaks = seq(0,1700,30),
                       labels = label_number(accuracy = 1,))+
    labs(x="Years", 
         y="Species count",
         title = names(nb_sp[i]))+
    theme_classic()
  
  ggsave(plot = x1, 
         filename = paste0("",names(nb_sp[i]),"_Number_species_",Sys.Date(),".png"), 
         path = "Evol country",
         width = 4,
         height = 5)
}


# Country evolution graphs ----

Y2021=rep(NA,245)
Y2022=rep(NA,245)
Y2023=rep(NA,245)
Y2024=rep(NA,245)
evol_sp=data.frame(Y2021,Y2022,Y2023,Y2024)

for(i in 3:length(world)){
  
  evol_sp$Y2021[i-2]=length(grep("^21",world[[i]]))
  evol_sp$Y2022[i-2]=evol_sp$Y2021[i-2]+length(grep("^22",world[[i]]))
  evol_sp$Y2023[i-2]=evol_sp$Y2022[i-2]+length(grep("^23",world[[i]]))
  evol_sp$Y2024[i-2]=evol_sp$Y2023[i-2]+length(grep("^24",world[[i]]))

}

rownames(evol_sp)=names(world)[3:247]

evol_sp=t(evol_sp)
evol_sp=as.data.frame(evol_sp)

for (i in 1:length(evol_sp)){

  x2=ggplot(evol_sp)+
  aes(c(2021,2022,2023,2024),evol_sp[[i]])+
  geom_point()+
  geom_label(aes(label = evol_sp[[i]]),size = 2, nudge_y = max(evol_sp[[i]])/15,
            nudge_x = -0.1)+
  geom_line()+ 
  geom_line(aes(c(2021,2022,2023,2024),rowMeans(x = evol_sp[4,])),
            colour = 'red',linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,1000,20),
    labels = label_number(accuracy = 1,))+
  labs(x="Years", 
       y="Cumulative species tally",
       title = names(evol_sp[i]))+
    theme_classic()

  ggsave(plot = x2, filename = paste0("",names(evol_sp[i]),"_cumulative_nb_species",Sys.Date(),".png"), path = "Evol country")
}


graph_files = list.files("~/Ornitho/BirdsSeenIn2024 challenge/Evol country/", full.names = T)
drive_files_graph=drive_ls(as_dribble("https://drive.google.com/drive/folders/1VNw3VZhncFfveNKzbq9QocL0WuLsTbDd"))
2

drive_rm(drive_files_graph)

files= map(graph_files,
           ~drive_upload(.x,
                         path = as_dribble("https://drive.google.com/drive/folders/1VNw3VZhncFfveNKzbq9QocL0WuLsTbDd")))

# Upload files to googledrive ----

blank_files = list.files("~/Ornitho/BirdsSeenIn2024 challenge/Country lists/", full.names = T)
blank_files = blank_files[c(grep("0[0-9]_Blanks_",blank_files))]

top_files = list.files("~/Ornitho/BirdsSeenIn2024 challenge/Tops/", full.names = T)
top_files = top_files[grep(Sys.Date(),top_files)]

blank_files = c(blank_files,top_files)

drive_files=drive_ls(as_dribble("https://drive.google.com/drive/folders/1KXdrj9HOSSVZjec0LyxgoB18u_nNYwsr"))
drive_files=drive_files[order(drive_files$name),]
drive_files=drive_files$name[c(grep("^0[0-9]_",drive_files$name))]
drive_files=drive_files[-1]

drive_rm(drive_files)

files= map(blank_files,
           ~drive_upload(.x,
                         path = as_dribble("https://drive.google.com/drive/folders/1KXdrj9HOSSVZjec0LyxgoB18u_nNYwsr")))


# Number species in 2024 ----

nsp=world[,-c(1,2)]

nsp$snp21_4=NA
nsp$snp24=NA

pb2 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                      max = length(nsp$Kenya),  # Maximum value of the progress bar
                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
                      width = 50,   # Progress bar width. Defaults to getOption("width")
                      char = "=")

for(i in 1:length(nsp$Kenya)){

  if(TRUE %in% (grepl("24",as.matrix(nsp)[i,]))){
    nsp$snp24[i]=TRUE
  }

  if(TRUE %in% (grepl("2[0-9]", as.matrix(nsp)[i,]))){
    nsp$snp21_4[i]=TRUE
  }
  else{
    next
  }

  setTxtProgressBar(pb2, i)

}

# tally_world$`Total 2024`[month(Sys.Date())]=as.numeric(table(nsp$snp24))
# tally_world$`Total 2021-2024`[month(Sys.Date())]=as.numeric(table(nsp$snp21_4))


# writexl::write_xlsx(tally_world, path = paste("Tally_world_",Sys.Date(),".xlsx", sep = ''))

# Most observed species in the world ----

# pb3 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
#                       max = length(world$`Species (Scientific)`),  # Maximum value of the progress bar
#                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                       width = 50,   # Progress bar width. Defaults to getOption("width")
#                       char = "=")
# 
# world$sum21_24=NA
# world$sum21=NA
# world$sum22=NA
# world$sum23=NA
# world$sum24=NA
# 
# for (i in 1:length(world$`Species (Scientific)`)){
# 
#   world$sum21_24[i]=length(which(is.na(as.matrix(world[i,-c(1,2)])) == F))
# 
#   world$sum21[i]=length(which(as.matrix(world[i,-c(1,2)]) %in% c("21","21_22",
#                                                                  "21_23","21_24","21_22_23","21_22_24","21_23_24","21_22_23_24")))
# 
#   world$sum22[i]=length(which(as.matrix(world[i,-c(1,2)]) %in% c("22","21_22",
#                                                                  "22_23","22_24","21_22_23","21_22_24","22_23_24","21_22_23_24")))
# 
#   world$sum23[i]=length(which(as.matrix(world[i,-c(1,2)]) %in% c("23","21_23",
#                                                                  "22_23","23_24","21_22_23","21_23_24","22_23_24","21_22_23_24")))
# 
#   world$sum24[i]=length(which(as.matrix(world[i,-c(1,2)]) %in% c("24","21_24",
#                                                                  "22_24","23_24","21_22_24","21_23_24","22_23_24",
#                                                                  "21_22_23_24")))
# 
#   setTxtProgressBar(pb3, i)
# 
# }
# 
# world$`Species (Scientific)`[which(world$sum21_24 %in% max(world$sum21_24))]
# max(world$sum21_24)
# world$`Species (Scientific)`[which(world$sum24 %in% max(world$sum24))]
# max(world$sum24)
# 



