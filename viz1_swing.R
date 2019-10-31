library(tidyverse)
library(shiny)
library(gridSVG)

ests = read_rds("out/ests_09_09_baseline.rds")

d = filter(ests, state %in% c("PA", "MI", "WI", "OH", "IA", "NH", "VA", 
                    "FL", "NM", "NV"),
       candidate != "Clinton")  %>%
    arrange(overall, candidate, dem_win)

filter(ests, candidate != "Clinton")  %>%
    group_by(overall, candidate) %>%
    mutate(total_ev=sum(dem_ev)) %>%
    mutate(label = paste0(candidate, ": ", total_ev, " EV")) %>%
write_csv("out/test_interactive/ests_swing.csv")
    

p = ests %>%
    filter(near(overall, 0.5)) %>% 
    group_by(candidate) %>%
    mutate(total_ev=sum(dem_ev)) %>%
    filter(state %in% c("PA", "MI", "WI", "OH", "IA", "NH", "VA", "FL", "NM", "NV"),
           candidate != "Clinton") %>%
    mutate(label = paste0(candidate, ": ", total_ev, " EV")) %>%
ggplot(aes(reorder(state, dem_pct), dem_pct, fill=dem_win)) + 
    facet_wrap(vars(label)) +
    coord_flip(ylim=c(0.4, 0.6)) +
    geom_hline(yintercept=0.5) +
    geom_col() +
    scale_fill_manual(values=c("#cf222c", "#1a80c4")) +
    guides(fill=F) +
    scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1),
                       name="Votes won by Democratic candidate") +
    labs(x=NULL) +
    theme_minimal()

p

p.svg = grid.export("out/swing.svg", htmlWrapper=T, addClasses=T)

cat(rjson::toJSON(apply(p$data, MARGIN=1, FUN=function(x)return(list(x)))))

cat(rjson::toJSON(t(p$data)))
cat(rjson::toJSON(as.list(as.data.frame(t(p$data)))))

#############################################
# SHINY
#############################################

slider_min = min(100*d$overall)
slider_max = max(100*d$overall)
slider_step = with(d, 100*max(overall - lag(overall), na.rm=T))
    
ui = fluidPage(
    titlePanel("Electability in 2020"),
    mainPanel(
        sliderInput(inputId = "overall",
                    label = "Popular Vote",
                    min = slider_min,
                    max = slider_max,
                    step = slider_step,
                    value = 50, 
                    width="100%",
                    post="%"),
        plotOutput(outputId="plot", height=480)
    )
)


server = function(input, output) {
    output$plot = renderPlot({
        tot_ev = ests %>%
            filter(near(overall, input$overall/100)) %>%
            group_by(candidate) %>%
            summarize(total_ev=sum(dem_ev))
        
        filter(d, near(overall, input$overall/100)) %>% 
            left_join(tot_ev, by="candidate") %>%
            mutate(label = paste0(candidate, ": ", total_ev, " EV")) %>%
        ggplot(aes(reorder(state, dem_pct), dem_pct, fill=dem_win)) + 
            facet_wrap(vars(label)) +
            coord_flip(ylim=c(0.4, 0.6)) +
            geom_hline(yintercept=0.5) +
            geom_col() +
            scale_fill_manual(values=c("#cf222c", "#1a80c4")) +
            guides(fill=F) +
            scale_y_continuous(labels=function(x) scales::percent(x, accuracy=1),
                               name="Votes won by Democratic candidate") +
            labs(x=NULL) +
            theme_minimal()
    }, res=150)
}

shinyApp(ui, server, options=list(launch.browser=T))

