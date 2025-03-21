rentrez_search_article <- function(keywords=NULL, journal=NULL, startdate=NULL, enddate=NULL){
  # check and install packages ---------------------------------------------------
  packages <- c("tidyverse", "rentrez", "glue", "dplyr")
  install.packages(setdiff(packages, rownames(installed.packages())))
  library("tidyverse")
  library("rentrez")
  library("glue")
  library("dplyr")
  
  # rewrite search query for each term -----------------------------------------
  keywords = paste0("(", paste(keywords, collapse = " AND "), ")")
  
  journal = paste0('(',
                   paste0(sapply(journal, function(x) paste0(x, "[JOUR]")), collapse=" OR "),
                   ")") 
  
  # define search function given query ----------------------------------------- 
  search_article <- function(query){
    # search the pubmed database 
    search_results <- entrez_search(db = "pubmed", term = query, retmax = 9999)
    print(paste0("Have found ", length(search_results$ids), " related articles."))
    # glimpse(search_results)
     print(paste0("The translated query is:", search_results$QueryTranslation))
    
    # retrieve title, authors, jornal, publication date, uid, doi
    articles_df <- data.frame()
    for(i in 1:ceiling(length(search_results$ids)/100)){
      end_index = ifelse(i < ceiling(length(search_results$ids)/100), 
                         i*100,
                         length(search_results$ids))
      summary_results <- entrez_summary(db = "pubmed", id = search_results$ids[(1+(i-1)*100):end_index])
      
      if(length(search_results$ids) > 1){
        articles <- lapply(summary_results, function(article) {
          list(
            title = article$title,
            # title = if("title" %in% names(article)) article$title else "",
            authors = paste0(article$authors$name, collapse = ","),
            lastauthor = article$lastauthor,
            journal = article$fulljournalname,
            pubdate = article$pubdate,
            epubdate = article$epubdate,
            uid = article$uid,
            doi = article$elocationid
          )
        })
      } else  if (length(search_results$ids) == 1) {
        articles <- data.frame(
          title = summary_results$title,
          # title = if ("title" %in% names(summary_results)) summary_results$title else "",
          authors = paste0(summary_results$authors$name, collapse = ","),
          lastauthor = summary_results$lastauthor,
          journal = summary_results$fulljournalname,
          pubdate = summary_results$pubdate,
          epubdate = summary_results$epubdate,
          uid = summary_results$uid,
          doi = summary_results$elocationid
        )
      } 
      
      if (length(search_results$ids) > 1) {
        articles_df_temp <- bind_rows(lapply(articles, as.data.frame))
      } else if (length(search_results$ids) == 1) {
        articles_df_temp <- as.data.frame(articles)
      }
      
      articles_df <- rbind(articles_df, articles_df_temp)
    }
    
    # retrieve abstract 
    for (i in 1:nrow(articles_df)){
      print(paste0("正在提取第",i,"篇文献信息，共",nrow(articles_df),"篇。"))
      abstract = entrez_fetch(db="pubmed", id=articles_df$uid[i], rettype = "abstract")
      abstract = gsub(abstract, pattern = '\n',replacement = '')
      articles_df[i,"abstract"] <- abstract
      Sys.sleep(1)
    }
    return(articles_df)
  }
  
  # if both of the dates are null execute the function ---------------------------
  if(is.null(keywords) & is.null(journal) & is.null(startdate) & is.null(enddate)){
    stop("Please narrow down the search.")
    
  } else if(is.null(startdate) & is.null(enddate)){
    query = case_when(
      # is.null(keywords) & is.null(journal) ~ "",
      is.null(keywords) & !is.null(journal) ~ journal,
      !is.null(keywords) & is.null(journal) ~ paste0(keywords),
      !is.null(keywords) & !is.null(journal) ~ paste0(keywords, " AND ", journal)
    )
    articles_df <- search_article(query)
    
  } else if(!is.null(startdate) & !is.null(enddate)){
    # if both of the dates are given execute the function -----------
    query = case_when(
      is.null(keywords) & is.null(journal) ~ "",
      is.null(keywords) & !is.null(journal) ~ paste0(journal, " AND ", startdate, ":", enddate, "[PDAT]"),
      !is.null(keywords) & is.null(journal) ~ paste0(keywords, " AND ", startdate, ":", enddate, "[PDAT]"),
      !is.null(keywords) & !is.null(journal) ~ paste0(keywords, " AND ", journal, " AND ", startdate, ":", enddate, "[PDAT]")
    )
    articles_df <- search_article(query)
    
  } else if (is.null(startdate) | is.null(enddate)) {
    # if one of the dates is null stop the function -------------
    stop("Please also offer the start date or the end date of publication.")
  
  }
  
  return(articles_df)
}







