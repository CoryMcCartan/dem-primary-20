library(tidyverse)
library(rvest)
library(magrittr)
library(lubridate)

parse_quinnipiac_primary = function(html) {
    n = html %>% 
        html_text %>%
        str_extract("(surveyed|survey includes)\\s+\\d+(?=\\s+Democrat)") %>%
        parse_number
    
    date = html %>% 
        html_text %>%
        str_extract("\\w+ \\d\\d?, 20(19|20)") %>%
        parse_date("%B %d, %Y")

    questions = html %>% 
        html_node("article") %>% 
        html_text %>%
        str_extract_all(regex("^\\d+[abc]?\\. .+\\?$", multiline=T)) %>%
        extract2(1)
    
    primary_idx = questions %>% 
        str_subset("Democratic primary for president were being held today") %>%
        str_extract("^\\d+") %>%
        as.numeric
    
    if (length(primary_idx) == 0) return(NA)
    
    tbls = html %>%
        html_nodes(".QChart pre") %>%
        extract2(primary_idx) %>%
        html_text %>%
        str_split("\r\n\\s+\r\n")
    
    primary_tbl1 = tbls[[1]][1:2] %>%
        paste(collapse="\n") %>%
        str_extract(regex("\\s+Tot\\s+Very.+", dotall=T)) %>%
        read_table %>%
        select(candidate=X1, total=Tot, very_lib=Very, smw_lib=Smwht, 
               mod_cons=Cons, men=Men, women=Wom, white=Wht, black=contains("Blk"), 
               white_coll=contains("Yes"), white_nocoll=contains("No")) %>%
        filter(candidate %in% c("Biden", "Sanders", "Warren", "Buttigieg", "Harris")) %>%
        #filter(candidate != "") %>%
        mutate_at(vars(-candidate), ~ coalesce(parse_number(., na="-")/100, 0))
    
    primary_tbl2 = tbls[[1]][3:4] %>%
        paste(collapse="\n") %>%
        str_extract(regex("\\s+Dem\\s+DemLn.+", dotall=T)) %>%
        read_table %>%
        select(candidate=X1, reg_dem=Dem, dem_lean=DemLn, 
               age_18_34=contains("18-34"), age_35_49=contains("35-49"), 
               age_50_64=contains("50-64"), age_65_inf=contains("65+"), 
               inc_000_050=`<50k`, inc_050_100=`50-100`, inc_100_inf=`100k+`) %>%
        filter(candidate %in% c("Biden", "Sanders", "Warren", "Buttigieg", "Harris")) %>%
        #filter(candidate != "") %>%
        mutate_at(vars(-candidate), ~ coalesce(parse_number(., na="-")/100, 0))
    
    left_join(primary_tbl1, primary_tbl2, by="candidate") %>%
        mutate(n=n, date=date, firm="Quinnipiac") %>%
        select(candidate, n, date, firm, everything()) %>%
        arrange(desc(total))
}

parse_quinnipiac_hth = function(html, lname) {
    n = html %>% 
        html_text %>%
        str_extract("surveyed [0-9,]+ ") %>%
        parse_number
    
    date = html %>% 
        html_text %>%
        str_extract("\\w+ \\d\\d?, 20(15|16|17|18|19|20)") %>%
        parse_date("%B %d, %Y")

    questions = html %>% 
        html_node("article") %>% 
        html_text %>%
        #str_replace_all("\r\n(?!\r\n)", " ") %>%
        str_extract_all(regex("^ ?\\d+[abc]?[ABC]?\\. .+\\? ?(\\(.+\\))?$", multiline=T)) %>%
        extract2(1)
    
    hth_idx = questions %>%
        str_detect(str_glue("candidates were \\w+ ", lname,
                                    " the Democrat and Donald Trump")) %>%
        match(T, .)
        #str_extract("^ ?\\d+") %>%
        #as.numeric
    
    #if (length(hth_idx) == 0) return(NA)
    if (is.na(hth_idx)) return(NA)
    if (date == "2015-09-24") hth_idx = hth_idx + 2
    
    tbls = html %>%
        html_nodes(".QChart pre") %>%
        extract2(hth_idx) %>%
        html_text%>%
        str_split("\r\n\\s+\r\n")
    
    hth_tbl1 = tbls[[1]][1:2] %>%
        paste(collapse="\n") %>%
        str_extract(regex("\\s+Tot\\s+Rep.+", dotall=T)) %>%
        read_table %>%
        rename(candidate=X1, total=Tot, gop=Rep, dem=Dem, ind=Ind,
               men=Men, women=Wom, white_coll=Yes, 
               white_nocoll=No) %>%
        mutate_at(vars(-candidate), ~ coalesce(parse_number(., na="-")/100, 0)) %>%
        mutate(candidate = case_when(
            candidate == "SMONE ELSE(VOL)" ~ "[lost]",
            candidate == "WLDN'T VOTE(VOL)" ~ "[lost]",
            candidate == "DK/NA" ~ NA_character_,
            T ~ candidate
        )) %>%
        filter(candidate != "") %>%
        group_by(candidate) %>%
        summarize_all(sum)
    
    hth_tbl2 = tbls[[1]][3:4] %>%
        paste(collapse="\n") %>%
        str_extract(regex("\\s+18-34\\s+35-49.+", dotall=T)) %>%
        read_table %>%
        rename(candidate=X1, age_18_34=`18-34`, age_35_49=`35-49`, 
               age_50_64=`50-64`, age_65_inf=`65+`, white_men=Men, 
               #white_women=Wom, white=Wht, black=Blk) %>%
               white_women=Wom, white=Wht, black=Blk, hisp=Hsp) %>%
        mutate_at(vars(-candidate), ~ coalesce(parse_number(., na="-")/100, 0)) %>%
        mutate(candidate = case_when(
            candidate == "SMONE ELSE(VOL)" ~ "[lost]",
            candidate == "WLDN'T VOTE(VOL)" ~ "[lost]",
            candidate == "DK/NA" ~ NA_character_,
            T ~ candidate
        )) %>%
        filter(candidate != "") %>%
        group_by(candidate) %>%
        summarize_all(sum)
    
    left_join(hth_tbl1, hth_tbl2, by="candidate") %>%
        mutate(hth=lname, n=n, date=date, firm="Quinnipiac") %>%
        select(candidate, hth, n, date, firm, everything()) %>%
        arrange(desc(total))
}

get_quinnipiac_primaries = function(max_date="2020-01-01", min_date="2019-01-01") {
    poll_pg = read_html("https://poll.qu.edu/national/") 
    
    dates = poll_pg %>%
        html_nodes("article table b") %>%
        html_text %>% 
        mdy
    
    links = poll_pg %>%
        html_nodes("article table a") %>%
        extract(dates >= min_date & dates <= max_date) %>%
        html_attr("href") %>%
        map(~ str_glue("https://poll.qu.edu", .))
    
    links %>%
        map(read_html) %>%
        #map(possibly(parse_quinnipiac_primary, NA))# %>%
        map(parse_quinnipiac_primary) %>%
        keep(is.tibble) %>%
        bind_rows
}

get_quinnipiac_hth = function(max_date="2020-01-01", min_date="2019-01-01",
                              candidates=c("Biden", "Sanders", "Warren",
                                           "Buttigieg" , "Harris")) {
    poll_pg = read_html("https://poll.qu.edu/national/") 
    
    dates = poll_pg %>%
        html_nodes("article table b") %>%
        html_text %>% 
        mdy
    
    links = poll_pg %>%
        html_nodes("article table a") %>%
        extract(dates >= min_date & dates <= max_date) %>%
        html_attr("href") %>%
        map(~ str_glue("https://poll.qu.edu", .))
    
    links %>%
        map(read_html) %>%
        cross2(candidates)%>%
        transpose %>%
        pmap(parse_quinnipiac_hth) %>%
        keep(is.tibble) %>%
        bind_rows
}

d.p = get_quinnipiac_primaries()
write_csv(d.p, "data/quinnipiac_primaries.csv")

gather(d.p, "group", "value", 5:23) %>% 
ggplot(aes(date, value, color=candidate, lty=candidate)) + 
    facet_wrap("group") + 
    geom_line()


d.h = get_quinnipiac_hth(max_date="2019-10-01")
html = read_html("https://poll.qu.edu/national/release-detail?ReleaseID=3643")
d.h2_biden = parse_quinnipiac_hth(html, "Biden")
d.h2_sanders = parse_quinnipiac_hth(html, "Sanders")
d.h2_warren = parse_quinnipiac_hth(html, "Warren")
d.h = bind_rows(d.h, d.h2_biden, d.h2_sanders, d.h2_warren)
write_csv(d.h, "data/quinnipiac_hth_2020.csv")

d.h.2016 = get_quinnipiac_hth(min_date="2015-08-01", max_date="2015-12-03",
                              candidates=c("Clinton", "Sanders"))
d.h.2016 = rename(d.h.2016, hisp=Hsp)
write_csv(d.h.2016, "data/quinnipiac_hth_2016.csv")

html = read_html("https://poll.qu.edu/national/release-detail?ReleaseID=2274")

parse_quinnipiac_primary(html)

parse_quinnipiac_hth(html, "Sanders")

