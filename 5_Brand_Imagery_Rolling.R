# Set up time intervals to loop through ------------------------------------------------------------ 

maxdate <- max(mdata$startdate)
mindate <- min(mdata$startdate)

n_months <- length(seq(from = round_date(mindate, "month"), 
                       to = round_date(maxdate, "month"), 
                       by = 'month'))

n_intervals <- ceiling(n_months/3) 


# Create list of dataframes ------------------------------------------------------------------------

list_mdata <- list()

for (i in 1:n_intervals){
  window <- 3*i 
  list_mdata[[i]] <- mdata[mdata$startdate > maxdate %m-% months(window) &
                           mdata$startdate <= maxdate %m-% months(window - 3), ] 
  print(list_mdata[[i]] %>% distinct(monthyear_num, month_year) %>% arrange(desc(monthyear_num)))
}

names(list_mdata) <- paste0("rolling", 1:n_intervals)


# Calculate brand imagery score on a rolling basis # 

list_imagery <- lapply(1:length(list_mdata), function(x){
    
    # Set input - - - - -
    input <- list_mdata[x] %>% data.frame() 

    # Unaided Awareness - - - - - 
    # D: Top of mind sample
    tomSample <- input %>% 
      filter(qID == "q12")  %>%
      count(month_year, monthyear_num, monthnum, month, answer) %>%
      filter(!answer == "exclude") %>%
      group_by(monthyear_num) %>%
      mutate( n.total = sum(n),
              p_tom = round((n / n.total),2)) %>%
      select(month_year, monthyear_num, month, answer, p_tom)
    
    unaided <- input %>% 
      filter(qID %in% c("q13_1", "q13_2", "q13_3", "q13_4", "q13_5", "q13_6", "q13_7")) %>%
      count(month_year, monthnum, answer) %>% 
      filter(!answer == "exclude") %>%
      group_by(month_year) %>%
      mutate( n.total = sum(n),
              p_unaided = round((n / n.total), 2)) %>%
      left_join(tomSample, by=c("month_year", "answer")) %>%
      replace(., is.na(.), 0) %>%
      mutate(factor.avg = p_tom + p_unaided, 
             factor = "unaided awareness") %>%
      filter(!answer == "other") %>%
      select(monthnum, month_year, answer, factor, factor.avg)
    
    
    # Aided Awareness - - - - -
    # D: Total sample of the month
    monthSample <- input %>%
      filter(qgroup=="last time heard of brand") %>% 
      count(month_year, monthyear_num, description) %>%
      dplyr::select(month_year, monthyear_num, answer = description, n.month_year = n) %>%
      arrange(month_year, monthyear_num, answer)
    
    aided <- input %>%
      filter(qgroup == "last time heard of brand", !answer == "Never heard of this brand") %>%
      count(month_year, monthyear_num, description) %>%
      rename(n.brandAwareness = n,
             answer = description) %>%
      left_join(monthSample, by = c("month_year", "monthyear_num", "answer")) %>%
      dplyr::mutate(factor.avg = round(n.brandAwareness / n.month_year, 2),
                    factor = "aided awareness") %>%
      select(month_year, monthyear_num, answer, n.brandAwareness, n.month_year, factor, factor.avg) 
    
    
    # Fin - - - - -
    # D: Total aided sample
    aidedSample <- input %>%
      filter(qgroup=="last time heard of brand", answer!="Never heard of this brand") %>%
      count(month_year, monthyear_num, description) %>%
      rename(answer = description,
             n.brandAwareness = n)
    
    input %>%
      filter(qgroup=="brand attributes")%>%
      count(month_year, monthyear_num, description, answer) %>%
      left_join(aidedSample, by=c("month_year", "monthyear_num", "answer")) %>%
      group_by(month_year, answer) %>%
      dplyr::mutate(p_attribute = round((n / n.brandAwareness),3),
                    label = revalue(description, attribute_map),
                    factor = revalue(description, factor_map)) %>% 
      group_by(month_year, answer, factor) %>%
      dplyr::mutate(factor.avg = round(mean(p_attribute),3)) %>%
      bind_rows(aided, unaided) %>%
      select(month_year, monthyear_num, answer, description, label, factor, factor.avg, n.attribute = n, n.brandAwareness, p_attribute) 

  }
)
          
          

# TEST ---------------------------------------------------------------------------------------------

window <- max(mdata$startdate) %m-% months(1)

# Unaided Awareness - - - - - 
# D: Top of mind sample
tomSample <- mdata[mdata$qID == "q12" & mdata$startdate >= window, ] %>% 
  count(month_year, monthyear_num, monthnum, month, answer) %>%
  filter(!answer == "exclude") %>%
  group_by(monthyear_num) %>%
  mutate( n.total = sum(n),
          p_tom = round((n / n.total),2)) %>%
  select(month_year, monthyear_num, month, answer, p_tom)

unaided <- mdata %>% 
    filter(qID %in% c("q13_1", "q13_2", "q13_3", "q13_4", "q13_5", "q13_6", "q13_7"), startdate >= window) %>%
    count(month_year, monthnum, answer) %>% 
    filter(!answer == "exclude") %>%
    group_by(month_year) %>%
    mutate( n.total = sum(n),
            p_unaided = round((n / n.total), 2)) %>%
    left_join(tomSample, by=c("month_year", "answer")) %>%
    replace(., is.na(.), 0) %>%
    mutate(factor.avg = p_tom + p_unaided, 
           factor = "unaided awareness") %>%
    filter(!answer == "other") %>%
    select(monthnum, month_year, answer, factor, factor.avg)


# Aided Awareness - - - - -
# D: Total sample of the month
monthSample <- mdata %>%
  filter(qgroup=="last time heard of brand", startdate <= window) %>% 
  count(month_year, monthyear_num, description) %>%
  dplyr::select(month_year, monthyear_num, answer = description, n.month_year = n) %>%
  arrange(month_year, monthyear_num, answer)

aided <- mdata %>%
  filter(qgroup == "last time heard of brand", !answer == "Never heard of this brand", startdate <= window) %>%
  count(month_year, monthyear_num, description) %>%
  rename(n.brandAwareness = n,
         answer = description) %>%
  left_join(monthSample, by = c("month_year", "monthyear_num", "answer")) %>%
  dplyr::mutate(factor.avg = round(n.brandAwareness / n.month_year, 2),
                factor = "aided awareness") %>%
  select(month_year, monthyear_num, answer, n.brandAwareness, n.month_year, factor, factor.avg) 


# Fin - - - - -
# D: Total aided sample
aidedSample <- mdata %>%
  filter(qgroup=="last time heard of brand", answer!="Never heard of this brand", startdate <= window) %>%
  count(month_year, monthyear_num, description) %>%
  rename(answer = description,
         n.brandAwareness = n)

imagery <- mdata %>%
    filter(qgroup=="brand attributes" & startdate <= window)%>%
    count(month_year, monthyear_num, description, answer) %>%
    left_join(aidedSample, by=c("month_year", "monthyear_num", "answer")) %>%
    group_by(month_year, answer) %>%
    dplyr::mutate(p_attribute = round((n / n.brandAwareness),3),
                  label = revalue(description, attribute_map),
                  factor = revalue(description, factor_map)) %>% 
    group_by(month_year, answer, factor) %>%
    dplyr::mutate(factor.avg = round(mean(p_attribute),3)) %>%
    bind_rows(aided, unaided) %>%
    select(month_year, monthyear_num, answer, description, label, factor, factor.avg, n.attribute = n, n.brandAwareness, p_attribute) 


