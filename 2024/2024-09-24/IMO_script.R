library(tidyverse)
library(reactable)
library(htmltools)
library(reactablefmtr)
library(webshot2)

fa_dependency <- htmlDependency(
  name = "font-awesome",
  version = "5.15.4",
  src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/"),
  stylesheet = "css/all.min.css"
)

# Load your data
IMO <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2024/2024-09-24/timeline_df.csv")

# Calculate length of time between start_date and end_date 
IMO$Days <- as.numeric(IMO$end_date - IMO$start_date)

# Define a mapping from country names to ISO codes
country_code_map <- c(
  "United Kingdom" = "gb",
  "Japan" = "jp",
  "Norway" = "no",
  "Russian Federation" = "ru",
  "Romania" = "ro",
  "Brazil" = "br",
  "Hong Kong" = "hk",
  "Thailand" = "th",
  "South Africa" = "za",
  "Colombia" = "co",
  "Argentina" = "ar",
  "Netherlands" = "nl",
  "Kazakhstan" = "kz",
  "Germany" = "de",
  "Spain" = "es",
  "Vietnam" = "vn",
  "Slovenia" = "si",
  "Mexico" = "mx",
  "Greece" = "gr",
  "United States of America" = "us",
  "Republic of Korea" = "kr",
  "Taiwan" = "tw",
  "India" = "in",
  "Canada" = "ca",
  "Türkiye" = "tr",
  "Sweden" = "se",
  "People's Republic of China" = "cn",
  "Australia" = "au",
  "Cuba" = "cu",
  "Poland" = "pl",
  "Finland" = "fi",
  "Czechoslovakia" = "cz",
  "France" = "fr",
  "Hungary" = "hu",
  "Austria" = "at",
  "Bulgaria" = "bg",
  "German Democratic Republic" = "de", # Reuse code for Germany
  "Union of Soviet Socialist Republics" = "ru" # Reuse code for Russia
)

# Create a function to get flag URLs
get_flag_url <- function(country) {
  code <- country_code_map[country]
  if (!is.null(code)) {
    paste0("https://flagpedia.net/data/flags/w580/", code, ".webp")
  } else {
    NA  # Return NA if country not found
  }
}

# Map country names to flag URLs
IMO$FlagURL <- sapply(IMO$country, get_flag_url)

# Proper headers
to_proper_case <- function(x) {
  sapply(x, function(str) {
    paste(toupper(substring(str, 1, 1)), tolower(substring(str, 2)), sep = "")
  })
}

colnames(IMO) <- to_proper_case(colnames(IMO))


twttr <-  str_glue("<span style='font-family:fa-brands'>&#xf081; </span> ")
github <- str_glue("<span style='font-family:fa-brands'>&#xf09b; </span> ")
linkedin <- str_glue("<span style='font-family:fa-brands'>&#f0e1; </span> ")
captiont  <- str_glue(" 
<br>{twttr}    @JezzaAyt 
{github}Jezzaayt
<br>{linkedin}Jeremyaytoun")


# Create reactable table with flag images
table <- reactable(
  IMO %>% select(Year, Country, Flagurl, Countries, Male_contestant, Female_contestant, Days, Start_date, End_date, Days),
  
  theme = reactableTheme(
    style = list(fontFamily = "sans-serif"),
    borderColor = "#DADADA"
  ),
  defaultPageSize = 70,
  defaultColDef = colDef(
    vAlign = "center",
    align = "center",
    headerVAlign = "center",
    headerStyle = list(fontFamily = "sans-serif"),
    width = 190
  ), 
  columns = list(
    Flagurl = colDef(
      cell = function(value) {
        if (!is.null(value)) {
          htmltools::tags$img(src = value, style = "width: 30px; height: auto;")
        } else {
          htmltools::tags$span("")  # Placeholder for missing flags
        }
      },
      html = TRUE
    )
  ),
  bordered = TRUE,
  highlight = TRUE
) %>% add_title("International Mathematical Olympiad (IMO) - TidyTuesday 2024-09-24") %>% add_source(source = htmltools::tags$div(
    "Social",
    htmltools::tags$br(),  # Add line break

    htmltools::tags$a(href = "https://twitter.com/Jezzaayt", target = "_blank",
                      htmltools::tags$i(class = "fab fa-twitter", style = "font-size: 20px; color: #1DA1F2;")),
    "Jezzaayt",

    
    htmltools::tags$a(href = "https://github.com/jezzaayt", target = "_blank",
                      htmltools::tags$i(class = "fab fa-github", style = "font-size: 20px; color: #333333;")),
    "Jezzaayt",

    htmltools::tags$a(href = "https://linkedin.com/in/jeremyaytoun", target = "_blank",
                      htmltools::tags$i(class = "fab fa-linkedin", style = "font-size: 20px; color: #0077B5;")),
                      "Jeremy Aytoun",
    style = "text-align: center; margin-top: 10px;"
  ) )

# Render the table
finished <-browsable(
  tagList(
    fa_dependency,  # Add Font Awesome CSS here
    table
  )
)

webshot::install_phantomjs()  # Install PhantomJS if needed




finished
htmlfilename <- "International Mathematical Olympiad (IMO).html"
save_html(finished, htmlfilename)
png_file <- "International Mathematical Olympiad (IMO).png"
webshot(htmlfilename, file = png_file, vwidth = 1024, vheight = 768, zoom = 2, delay = 2)

