# Andrew Chen 2023 12
# Replicating Dino's replication of FF3 in 2023

# Environment -----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(ggplot2)

# Data -----------------------------------------------------------------------
# download ff3 factors from Ken French's website
temp = download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
    , destfile = "temp.zip")  
unzip("temp.zip")
ff3 = read.csv("F-F_Research_Data_Factors.CSV", skip = 3, nrows = 1172-4) %>% as_tibble()
colnames(ff3)= c("yearm", "mktrf", "smb", "hml", "rf")

# download 25 size and b/m sorted ports from Ken French's website
temp = download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_5x5_CSV.zip"
    , destfile = "temp.zip")
unzip("temp.zip")
ff25 = read.csv("25_Portfolios_5x5.CSV", skip = 15, nrows = 1184-16) %>% as_tibble()
colnames(ff25) = tolower(colnames(ff25))
colnames(ff25)[1] = "yearm"

# make long and merge
dat = ff25 %>% pivot_longer(-yearm, names_to = "port", values_to = "ret") %>% 
    left_join(ff3, by = "yearm") %>% 
    mutate(exret = ret - rf) %>% 
    select(-c(ret,rf))

# parse port names
dat = dat %>%  mutate(
        port = str_replace(port, 'small', 'me1')
        , port = str_replace(port, 'big', 'me5')
        , port = str_replace(port, 'lobm', 'bm1')
        , port = str_replace(port, 'hibm', 'bm5')
    ) %>% 
    mutate(
        me = substr(port, 1, 3), bm = substr(port, 5, 7)
    ) 
    
# Replicate FF1993 Tables --------------------------------------------------------------------

# replicate FF1993 Table 9a Panel (ii)
tab_9a2 = dat %>% 
    filter(yearm >= 196307 & yearm <= 199112) %>% 
    group_by(me, bm) %>% 
    summarize(alpha = lm(exret ~ mktrf )$coefficients[1]) %>% 
    pivot_wider(names_from = bm, values_from = alpha) %>% 
    knitr::kable(digits = 3, caption = 'Table 9a Panel (ii) (CAPM, intercept)')

# replicate FF1993 Table 9a Panel (iv)
tab_9a3= dat %>% 
    filter(yearm >= 196307 & yearm <= 199112) %>% 
    group_by(me, bm) %>% 
    summarize(alpha = lm(exret ~ mktrf + smb + hml)$coefficients[1]) %>% 
    pivot_wider(names_from = bm, values_from = alpha) %>% 
    knitr::kable(digits = 3, caption = 'Table 9a Panel (iv) (FF3, intercept))')

print(tab_9a2)
print(tab_9a3)

# Get alphas for many specs -------------------------------------------------------------------

get_alphas = function(model, yearmstart, yearmend){
    alpha = dat %>% 
        filter(yearm >= yearmstart & yearm <= yearmend) %>% 
        group_by(me, bm) %>% 
        summarize(alpha = lm(as.formula(model))$coefficients[1]) %>%
        mutate(model, yearmstart = yearmstart, yearmend = yearmend)
}

setlist = expand.grid(
    model = c('exret ~ mktrf', 'exret ~ mktrf + smb + hml')
    , yearmstart = c(196307, 199401, 200301)
    , yearmend = c(199112, 202310)
) %>% 
    filter(yearmstart < yearmend) %>% 
    mutate(model = as.character(model))

alpha = tibble()
for (i in 1:nrow(setlist)){
    print(setlist[i,])
    temp =  get_alphas(setlist[i,]$model, setlist[i,]$yearmstart, setlist[i,]$yearmend)
    alpha = rbind(alpha, temp)
}

# Summarize -------------------------------------------------------------------
alphasum = alpha %>% 
    group_by(model, yearmstart, yearmend) %>%
    summarize(mean_abs_alpha = mean(abs(alpha))) %>% ungroup() %>% 
    arrange(yearmstart, yearmend, model) %>%    
    mutate(
        modelname = case_when(
            model == 'exret ~ mktrf' ~ 'CAPM'
            , model == 'exret ~ mktrf + smb + hml' ~ 'FF3'
        )
        , sample = case_when(
            yearmstart == 196307 & yearmend == 199112 ~ 'Original Sample'
            , yearmstart == 199401 & yearmend == 202310 ~ 'Post Sample'
            , yearmstart == 196307 & yearmend == 202310 ~ 'Full Sample'
            , yearmstart == 200301 & yearmend == 202310 ~ 'Past 20 Years'
        )
    ) 
    

# table to console    
alphasum %>% 
    select(sample, yearmstart, yearmend, modelname, mean_abs_alpha)  %>% 
    pivot_wider(names_from = modelname, values_from = mean_abs_alpha) %>% 
    mutate(FF3_Improvement = (CAPM - FF3)) 


# plot
plt = ggplot(
        alphasum %>% 
            filter(sample %in% c('Original Sample', 'Post Sample')) %>% 
            mutate(sample = factor(
                sample, levels = c('Original Sample', 'Post Sample', 'Past 20 Years')
                    , labels = c('FF1993\'s Sample', 'Post 1993', 'Past 20 Years')))
        , aes(x=sample, y=mean_abs_alpha)) + 
    geom_bar(aes(fill=modelname), stat="identity", position=position_dodge()) +
    theme_bw() +
    theme(legend.title = element_blank()
        , legend.position = c(8,8)/10) +
    ylab('Mean Absolute Alpha (%, p.m.)') + xlab('') +
    ggtitle('Pricing 25 Size and B/M Sorted Portfolios') +
    scale_y_continuous(breaks = seq(0, 1, 0.05))

ggsave('FF3-in-2023.png', plt, width = 4, height = 4) 
