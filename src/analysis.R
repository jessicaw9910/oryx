library(magrittr)
library(ggplot2)
library(reshape2)
library(svglite)

df <- read.csv('2022-03-16T193938Z.csv', stringsAsFactors = FALSE, header = TRUE)

## add column for equipment damaged by TB-2
df$tb2 <- sapply(df$status, function(x) ifelse(x == 'TB2', 1, 0))

## fix unusual statuses
fix_status <- function(input){
  current <- c('TB2', 'burned', 'ground', 'Russia', 'stripped')
  updated <- c('destroyed', 'abandoned', 'destroyed', 'scuttled', 'captured')
  
  if (input %in% current) {
    output <- updated[input == current]
  } else {
    output <- input
  }
  
  return(output)
}


df$status <- sapply(df$status, fix_status)
df$group <- stringr::str_to_title(df$group)

df$combo <- paste(df$country, df$group, sep = '_')
df_wide <- table(df$combo, df$status) %>% as.data.frame()
colnames(df_wide) <- c('combo', 'status', 'count')  
df_wide <- df_wide %>%
  tidyr::pivot_wider(
    names_from = 'status',
    values_from = 'count'
  ) %>%
  tidyr::separate(col='combo', into = c('country', 'group'), sep='_')

View(df_wide)

df_plot <- melt(df_wide, id.vars = c('country', 'group'))
colnames(df_plot)[(dim(df_plot)[2]-1):dim(df_plot)[2]] <- 
  c('status', 'count')

top3 <- c('abandoned', 'captured', 'destroyed')
df_top3 <- df_plot[(df_plot$status %in% top3), ]
df_top3 <- df_top3[(df_top3$count != 0), ]

df_table <- aggregate(df_top3$count, by=list(status=df_top3$status, country=df_top3$country), FUN=sum)
colnames(df_table)[length(df_table)] <- 'total'

df_top3$total <- mapply(function(x, y) df_table$total[(df_table$status == x & df_table$country == y)], df_top3$status, df_top3$country)
df_top3$percent <- df_top3$count / df_top3$total
df_top3$country[df_top3$country == 'UKR'] = 'Ukraine'
df_top3$country[df_top3$country == 'RUS'] = 'Russia'

ggplot(df_top3, aes(x = stringr::str_to_title(status), y = percent, fill = group)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country, scales = 'free_y') +
  geom_text(aes(label=stringr::str_wrap(paste(group, count, sep = ':'), 100)), 
            size = 1.8, position = position_stack(vjust = 0.5)) +
  guides(fill=guide_legend(title="Equipment Type", ncol=1,
                           override.aes = list(size=5))) +
  theme(legend.title = element_text(size=8),
        legend.text = element_text(size=8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab('Status') +
  ylab('Percent')

ggsave('../plots/stacked_bar.svg',  width = 16, height = 8)