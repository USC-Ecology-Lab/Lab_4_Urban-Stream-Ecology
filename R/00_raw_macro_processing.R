####
# raw_macro_processing
####
rm(list = ls())

# Raw macro 
macroinverts <- read.csv('./data/raw_macro_survey.csv')
macroinverts$tag <- paste0(macroinverts$Site, macroinverts$Survey)

# sensitivity guide
sensitivity <- read.csv('./data/taxa_sensitivity_guide.csv')

## Add 0's for all inverts
all_sites <- unique(macroinverts$tag)
all_spp <- unique(macroinverts$Taxa)

full_combinations <- expand.grid(Taxa = all_spp, tag = all_sites)

result_df <- full_combinations |> 
  left_join(macroinverts, by = c("Taxa", "tag"))

# remove nobs rows
result_df <- result_df[which(!(result_df$Taxa == 'nobs')),]

# This is fairly mess and not the cleanest but it is remenant code:

for(tag in unique(result_df$tag)) {
  try({
    idx = which(result_df$tag == tag)
    result_df[idx, ]$Site <- unique(result_df[idx, ]$Site[which(!is.na(result_df[idx, ]$Site))])
    result_df[idx, ]$Survey <- unique(result_df[idx, ]$Survey[which(!is.na(result_df[idx, ]$Survey))])
    result_df[idx, ]$Group_id <- unique(result_df[idx, ]$Group_id[which(!is.na(result_df[idx, ]$Group_id))])
    result_df[idx,]$Count[which(is.na(result_df[idx,]$Count))] <- 0
  })
}


# manually fixing some rows
result_df$tag[which(is.na(result_df$Site))]

manual_row_fixer <- function(result_df, tag, Site, Survey, Group_id, Count = 0) {
  idx <- which(result_df$tag == tag)
  result_df$Site[idx] <- Site
  result_df$Survey[idx] <- Survey
  result_df$Group_id[idx] <- Group_id
  result_df$Count[idx] <- 0
  return(result_df)
}

result_df <- result_df |> 
  manual_row_fixer('ShoppingPlaza5', 'ShoppingPlaza', 5, 'A')
result_df <- result_df |> 
  manual_row_fixer('ShoppingPlaza9','ShoppingPlaza', 9, 'A')
result_df <- result_df |> 
  manual_row_fixer('Upstream7','Upstream', 7, 'C')

# merge with sensitivity
macro_survey <- result_df |> 
  left_join(sensitivity, by = 'Taxa')

write.csv(macro_survey, './data/macro_survey.csv', row.names = F)
