c<-gis %>%
  mutate(OWN_ADD = as.character(OWN_ADD)) %>%
  mutate(PROP_ADD = as.character(PROP_ADD)) %>%
  filter(OWN_ADD!=PROP_ADD)

a<- c %>%
  count(OWNER_NAME)
