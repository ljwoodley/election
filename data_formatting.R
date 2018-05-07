library(tidyverse)

#--------DATA FORMATTING----------#
filename <- list.files(pattern = '.csv')

data <- lapply(filename, read.csv)

for (i in 1:length(data)){
  data[[i]] <- cbind(data[[i]],'year' = substr(filename[i],17,20))
  }

df <- do.call(rbind, data)

colnames(df) <- tolower(colnames(df))

#create county comission district
df <- df %>% mutate(ccd = as.factor(if_else(df$precinctcode < 1000, substr(df$precinctcode,1,1), substr(df$precinctcode,1,2))))

#create school board district
df <- df %>% mutate(sbd = case_when(ccd %in% (1:2) ~ 1,
                                    ccd %in% (3:4) ~ 2,
                                    ccd %in% (5:6) ~ 3,
                                    ccd %in% (7:8) ~ 4,
                                    ccd %in% (9:10) ~ 5,
                                    ccd %in% (11:12) ~ 6,
                                    ccd %in% (13:14) ~ 7
                                   )
                   )

df$party <- as.character(df$party)
df$candidate_issue <- as.character(df$candidate_issue)

df$party <- if_else(df$party %in% c('DEM','REP'),df$party,'OTH')
df$candidate_issue <- if_else(df$party %in% c('DEM','REP'),df$candidate_issue, 'OTH')

df$contest <- as.character(df$contest)

df$contest[df$contest == "GOVERNOR AND LIEUTENANT GOVERNOR"] <- "Governor"
df$contest[df$contest == "MAYOR"] <- "Mayoral"
df$contest[df$contest == "United States President and Vice President"] <- "Presidental"

df <-  df %>% filter(contest %in% c("Governor", "Mayoral",
                                    "Presidental")) %>% 
              select(year,party, contest,ccd,sbd, candidate_issue, totalvotes)


#combine year and contest for unique election type
df$election <- paste(df$year,df$contest,"Election")

write.csv(df, "df.csv", row.names=FALSE)



