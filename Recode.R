# Clarity ------------------------------------------------------------------------------------------

var_naming <- names(screened)[grep("q97", names(screened))]
var_naming_cleaned <- c("Chobani Oat Parfaits", 
                        "Chobani Overnight Oats",
                        "Chobani Covered Oats",
                        "Chobani Dairy Oats",
                        "Chobani Yogurt Topped Oats",
                        "Chobani Oat Fusion",
                        "Chobani Soft Oats",
                        "Chobani Creamy Oats",
                        "Chobani Smothered Oats")

chi_naming <- purrr::map(var_naming, ~ stats::chisq.test(table(screened[,.x]))) # All significantly different but this is useless

xtab_naming <- purrr::map(var_naming, ~ screened %>%
                            group_by(UQ(sym(.x))) %>%
                            count() %>%
                            ungroup() %>%
                            mutate(p = (n / sum(n))*100)) %>%
  setNames(var_naming_cleaned)

naming <- purrr::map2(var_naming, var_naming_cleaned, ~ screened %>%
                        select(.x, 'type1', 'type2', 'pi') %>% 
                        dplyr::mutate(!!paste0(.y) := dplyr::recode(screened[, .x],              
                                                                    "Not clear at all" = 1, 
                                                                    "Somewhat unclear" = 2, 
                                                                    "Neither clear nor unclear" = 3, 
                                                                    "Somewhat clear " = 4, 
                                                                    "Extremely clear" = 5,
                                                                    "I have never heard of this brand" = NULL))) %>%
  bind_cols(data.frame()) %>%
  select(var_naming_cleaned, 'pi', 'type1', 'type2') 

naming_long <- reshape2::melt(naming, id = c("type1", "type2", "pi")) %>% filter(!is.na(value))

naming_long$variable <- relevel(naming_long$variable, ref="Chobani Oat Parfaits")

lm(value ~ variable, naming_long[naming_long$type1 == "oatmeal", ]) %>% summary() 
lm(value ~ variable, naming_long[naming_long$type1 == "yogurt only", ]) %>% summary() 
lm(value ~ variable, naming_long[naming_long$type2 == "chobani acceptor", ]) %>% summary() 
lm(value ~ variable, naming_long) %>% summary() 
# OVERALL: Significantly more likely to find Oat Parfait to be clearest
# 2. Chobani Yogurt Topped Oat
# 3. Chobani Creamy Oats [Much less than #2]  



mean_clarity <- naming_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value))

