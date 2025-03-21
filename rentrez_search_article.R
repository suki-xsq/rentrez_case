rm(list=ls())
source("func_rentrez_search_article.R")

 
startdate="2020/08/01" 
enddate="2020/08/15"
journal=list("Nature Communications", "Cell", "Nature", "Science")
keywords = list("COVID-19", "excess mortality")
articles_df <- rentrez_search_article(keywords=keywords, journal=journal, 
                                      startdate=startdate, enddate=enddate)

startdate=gsub("/", "", startdate) 
enddate=gsub("/", "", enddate)

write.csv(articles_df, paste0("articles_df_",startdate,"_",enddate,".csv"))












