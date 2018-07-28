library(data.table)
library(ggplot2)
library(ggthemes)
library(GGally)
library(psych)
library(dplyr)

edf <- fread('./data/elections-2014.csv')
colnames(edf) <- c('state', 'pc', 'candidates', 'sex', 'age', 'category', 'party', 'symbol', 'general', 'postal', 'total', 'ratio_electors', 'ratio_votes', 'total_electors')

head(edf)

ed <- edf[, c('state', 'pc', 'party', 'total')]
head(ed)

upa <- ed %>%
  filter(party %in% c('INC', 'BSP', 'SP', 'JD(S)', 'AITC', 'NC', 'TDP', 'RJD', 'CPI', 'CPM', 'NCP', 'INLD')) %>%
  group_by(state, pc) %>%
  summarise(total = sum(total) + (sum(total)/10) + 1)

upa <- as.data.table(upa)
upa$party <- c('UPA')

nda <- ed %>%
  filter(party %in% c('BJP', 'JD(U)', 'JMM', 'LJP', 'SAD')) %>%
  group_by(state, pc) %>%
  summarise(total = sum(total) - (sum(total)/10) + 1)

nda <- as.data.table(nda)
nda$party <- c('NDA')

edup <- rbindlist(list(ed, upa, nda), use.names = T)

edup <- as.data.table(edup %>%
  filter(!party %in% c('BJP')))

res <- edup %>%
  group_by(state, pc) %>%
  filter(total == max(total)) %>%
  arrange(state, pc, party)

res <- as.data.table(res)

res[res$party == 'NDA']
res[res$party == 'UPA']
res[res$party == 'INC']

rc <- res %>%
  group_by(party) %>%
  summarise(n = n())

rc <- as.data.table(rc)
