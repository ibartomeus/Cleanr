# Cleanr

I am tired to clean decent size datasets of species occurrences. Here there are some heuristics. 

**note**: check function clean_species buried somewhere maybe in traitbase?


1) Have a reference list of accepted names.
2) trim whitespaces e.g. data$Genus <- trimws(data$Genus) and other heuristics like "sp." etc...
3) subset only mismatches with the reference list. e.g.
mis <- data$Genus_species[which(!data$Genus_species %in% Genus_species)]
mismatches <- unique(mis)
4) You can try fuzzy matching too to select mismatches. I found two functions
a) e.g. agrep(c("Coleoxys"), genus, value = TRUE, max = list(all = 2)) #only detects small changes
b) for(i in 1:length(mismatches)){
  temp2 <- genus[as.logical(adist(mismatches[i],genus) <= 2)]
  if(length(temp2) == 1){
    fixed[i] <- temp2
  } else {
    fixed[i] <- NA
  }
} #(option b tends to work better).
SAVE BOTH THE ORIGINAL NAME AND THE NEW MATCHED NAMES IN DIFFERENT COLUMNS
5) Use taxize for the mismatched species and check for synonyms, etc...

I found a more up to date version of part of the code in the gist: 
temp <- taxize::synonyms(to_check[which(is.na(checked$checked))], db = "itis")
synonym_ids <- grep(pattern = "acc_name", temp)
accepted_names_temp <- unlist(lapply(temp[synonym_ids], "[", "acc_name"),
                              use.names = FALSE, recursive = FALSE)
accepted_names <- unlist(lapply(accepted_names_temp, `[[`, 1))
synonym_names <- rep(NA, length(to_check[which(is.na(checked$checked))]))
synonym_names[synonym_ids] <- accepted_names
synonims <- data.frame(to_check = to_check[which(is.na(checked$checked))], synonym_names)

6) Keep original, and fixed names, and do manually the species groups, etc...

