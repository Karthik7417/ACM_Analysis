library(rvest)
library(magrittr)
library(gsubfn)

key_words <- c("Expert systems","Cloud Computing","Genetic Algorithms","Data Analytics")

#for(a in 1:length(key_words)){
   
   topic = key_words[3]
   
   # doc_link <- read.csv(file = paste0(topic,"_doc_links.csv"), stringsAsFactors = F)
   # doc_link$link <- strapplyc(doc_link$link,"(.*)&CFID", simplify = T)
   # doc_link$link <- paste0(doc_link$link,"&preflayout=flat")
   # 
   # doc_link <- doc_link[!duplicated(doc_link[,c('title', 'link')]),]
   # doc_link <- doc_link[!doc_link$authors == "",]
   # row.names(doc_link) <- NULL
   # 
   
   #flag = 0
   
   for(i in 37:nrow(doc_link)){
      url <- URLencode(doc_link$link[i])
      doc <- try(read_html(url))
      
      if(class(doc) == "try-error"){
         next
      }
                      
      
      main_nodes <- doc %>% html_nodes('#divmain') 
      flatbody_nodes <- doc %>% html_nodes('.flatbody')
      #fback_nodes <- doc %>% html_nodes('#fback')
      
      #for(k in 1:length(main_nodes)){
      
      title <- main_nodes %>% html_node('strong') %>% html_text(trim = T)
      
      abstract <- paste(flatbody_nodes[1] %>% html_nodes('div') %>% html_text(trim=T), collapse = " ; ")
      
      author_name <- main_nodes %>% html_nodes('.medium-text+ .medium-text td:nth-child(2) a') %>% html_text(trim=T)
      if(length(author_name) == 0){
         author_name = "-"
      }
      
      author_link <- main_nodes %>% html_nodes('.medium-text+ .medium-text td:nth-child(2) a') %>% html_attr('href')
      if(length(author_link) == 0){
         author_link = "-"
      }
      
      
      author_affliation <-  main_nodes %>% html_nodes('small') %>% html_text(trim=T)
      if(length(author_affliation) == 0){
         author_affliation = "-"
      }
      
      publication_years <- doc %>% html_nodes('.flatbody td td tr:nth-child(1) .small-text') %>% html_text(trim=T)
      
      publication_count <- doc %>% html_nodes('.flatbody td td tr:nth-child(3) .small-text') %>% html_text(trim=T) %>% gsub(pattern = ",", replacement = "") %>% as.integer()
      if(length(publication_count) == 0){
         publication_count = 0
      }
      
      
      citation_count <- doc %>% html_nodes('.flatbody td td tr:nth-child(5) .small-text') %>% html_text(trim=T) %>% gsub(pattern = ",", replacement = "") %>% as.integer()
      if(length(citation_count) == 0){
         citation_count = 0
      }
      
      download_count <- doc %>% html_nodes('.flatbody td td tr:nth-child(13) .small-text') %>% html_text(trim=T) %>% gsub(pattern = ",", replacement = "") %>% as.integer()
      if(length(download_count) == 0){
         download_count = 0
      }
      
      
      avg_downloads_per_article <- doc %>% html_nodes('.flatbody td td tr:nth-child(15) .small-text') %>% html_text(trim=T)%>% gsub(pattern = ",", replacement = "") %>% as.numeric()
      if(length(avg_downloads_per_article) == 0){
         avg_downloads_per_article = 0
      }
      
      
      avg_citations_per_article <- doc %>% html_nodes('.flatbody td td tr:nth-child(17) .small-text') %>% html_text(trim=T) %>% as.numeric()
      if(length(avg_citations_per_article) == 0){
         avg_citations_per_article = 0
      }
      
      conference <- main_nodes %>% html_nodes(".medium-text .medium-text tr td") %>% html_text(trim = T)
      conference <- gsub("\u00A0","", conference, fixed = T)
      conference <- conference[match("·Proceeding", conference)+1]
      if(length(conference) == 0){
         conference = "-"
      }
      
      paper_acceptance_rate <- flatbody_nodes %>% html_nodes('.medium-text tr td') %>% html_text(trim=T)
      paper_acceptance_rate <- paper_acceptance_rate[grepl(pattern = "Paper Acceptance Rate", x = paper_acceptance_rate, ignore.case = T)]
      if(length(paper_acceptance_rate) == 0){
         paper_acceptance_number = "-"
         paper_acceptance_percent = "-"
      } else{
         paper_acceptance_number <- strapplyc(X = paper_acceptance_rate, pattern = 'Rate (.*) submissions', simplify = T) %>% gsub(pattern = ",", replacement = "")
         
         paper_acceptance_percent <-  as.integer(strapplyc(X = paper_acceptance_rate, pattern = 'submissions, (.*)%', simplify = T))
      }
      
      overall_acceptance_rate <- flatbody_nodes %>% html_nodes('.medium-text tr td') %>% html_text(trim=T)
      overall_acceptance_rate <- overall_acceptance_rate[grepl(pattern = "Overall Acceptance Rate", x = overall_acceptance_rate, ignore.case = T)]
      if(length(overall_acceptance_rate) == 0){
         overall_acceptance_number = "-"
         overall_acceptance_percent = "-"
      }else{
         overall_acceptance_number <- strapplyc(X = overall_acceptance_rate, pattern = 'Rate (.*) submissions', simplify = T)%>% gsub(pattern = ",", replacement = "")
         
         overall_acceptance_percent <-  as.integer(strapplyc(X = overall_acceptance_rate, pattern = 'submissions, (.*)%', simplify = T))
      }
      
      article_appears_in <- doc %>% html_nodes('.flatbody > div > a') %>% html_text(trim=T)
      article_appears_in <- paste(article_appears_in[!article_appears_in == ""], collapse = " , ")
      if(length(article_appears_in) == 0){
         article_appears_in = "-"
      }else if(article_appears_in == "ACM International Conference Proceeding Series"){
         article_appears_in = "-"
      }
      
      citations <- paste(doc %>% html_nodes('.flatbody > table td:nth-child(2) div a') %>% html_text(trim = T), collapse = ":;")
      if(length(citations) == 0){
         citations = "-"
      }   
      
      entry = data.frame(topic, title, abstract, author_name,author_link, author_affliation, publication_count, citation_count, download_count, avg_downloads_per_article, avg_citations_per_article ,conference, paper_acceptance_number, paper_acceptance_percent, overall_acceptance_number, overall_acceptance_percent, article_appears_in, citations ,stringsAsFactors = F)
      
      if(flag == 0){
         main = entry
         flag = 1
      }else{
         main = rbind(main, entry)
      }
      Sys.sleep(2)
   }
   
   write.csv(main, file = paste0(topic, "_data.csv"), row.names = F)
#}