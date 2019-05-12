# Source start program -----------------------------------------------------------------------------

source("C:/Users/Christina.kim/Documents/Brand Tracker/Code/0_Inputs_Wgt.R")

# ToM: Top 3 brands (for corr) ---------------------------------------------------------------------

w_dummy <-                                                                                          # Apply filter and add dummies
  mdata %>%
  filter(qID == "q12" & startdate >= window) %>%
  to_dummy(answer, suffix = "label") %>%
  bind_cols(tmp) 

vars_dummy <- names(w_dummy)[grep("answer_", names(w_dummy))]                                       # List of dummies to loop through

weighted <-           
  map2(vars_dummy, "wt_weekly", ~ w_dummy %>%                                                       # Looping through dummies with weight as constant
         select(.x, .y) %>%                                                                         # Selecting dummy variable and weight variable
         mutate(!!paste0('prod_', quo_name(gsub("^.*\\_", "", .x))) := 
                  w_dummy[,.x]*w_dummy[,.y])) %>%                                                   # Multiply dummy and weight. Applying a dynamic name to product  
  bind_cols(w_dummy, .) %>%                                                                         # Bind back columns to og dataset
  rename(wt_weekly = "wt_weekly1") %>%
  select(-dplyr::starts_with('wt_weekly'))

vars_weighted <- names(weighted)[grep("prod_", names(weighted))]                                    # List of weighted dummies to loop through        

# Start back up here: 
rollup <-
  map(vars_weighted, ~ weighted %>%
        select(weeknum, weekof, vars_weighted, )
        dplyr::group_by(weeknum, weekof) %>%
        mutate(n.total= sum(wt_weekly),
               !!paste0('p_raw_', quo_name(gsub("^,*\\_", "", .x))) := sum(.x)/n.total,
               !!paste0('p_', quo_name(gsub("^,*\\_", "", .x))) := 
                 paste(round((sum(.x)/n.total*100), 0), "%", sep=""))) %>%
  bind_cols(weighted) %>% 
  filter(!tolower(answer) == "weighted") %>% 
  select(weeknum, weekof, n.total, answer, dplyr::starts_with("p_")) %>%
  distinct() %>%
  arrange(weeknum, answer)   

weighted %>%
  group_by(weeknum, weekof) %>%
  mutate(n.total = n(),
         blah = sum(prod_chobani)/n.total)
































  
