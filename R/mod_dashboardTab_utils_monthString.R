monthString = function(month, year) {

  paste0(
    year, 
    "-", 
    ((as.numeric(month)/100) %>% 
       as.character() %>% 
       str_match('\\d\\.(.*)'))[,2])
  
}
