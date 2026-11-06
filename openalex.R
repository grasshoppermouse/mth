library(httr)
library(jsonlite)
library(dplyr)

# List of chapters from "Man the Hunter" (Lee & DeVore, 1968)
chapters <- data.frame(
  Title = c(
    "What Hunters Do for a Living, or, How To Make Out on Scarce Resources",
    "The evolution of hunting",
    "An Introduction to Hadza Ecology",
    "Some Predictions for the Pleistocene Based on Equilibrium Systems among Recent Hunter-Gatherers",
    "Problems in the study of hunters and gatherers",
    "The “hunting” economies of the tropical forest zone of South America: an attempt at historical perspective",
    "Stability and Flexibility in Hadza Residential Groupings",
    "Hunting: An Integrating Biobehavior System and Its Evolutionary Importance",
    "Coping with Abundance: Subsistence on the Northwest Coast",
    "Epidemiological Factors: Health and Disease in Hunter-Gatherers",
    "Demographic and Ecological Influences on Aboriginal Australian Marriage Sections",
    "The Importance of Flux in Two Hunting Societies",
    "A Theoretical Framework for Interpreting Archeological Materials",
    "Methodological Considerations of the Archeological Use of Ethnographic Data",
    "Subsistence and Ecology of Northern Food Gatherers with Special Reference to the Ainu",
    "The Netsilik Eskimos: Adaptive Processes",
    "The current status of the world's hunting and gathering peoples",
    "The Diversity of Eskimo Societies",
    "The Nature of Dogrib Socioterritorial Groups",
    "Causal Factors and Processes in the Evolution of Pre-farming Societies",
    "Traces of Pleistocene Hunters: An East African Example",
    "The Concept of Primitiveness",
    "Australian Marriage, Land-Owning Groups, and Initiations",
    "Ownership and Use of Land among the Australian Aborigines",
    "Studies of Hunter-Gatherers as an Aid to the Interpretation of Prehistoric Societies",
    "Ethnographic Data and Understanding the Pleistocene",
    "The Birhor of India and Some Comments on Band Organization",
    "'Marriage Classes' and Demography in Central Australia",
    "Gidjingali Marriage Arrangements",
    "Southeastern Australia: Level of Social Organization"
  ),
  Author = c(
    "Richard B. Lee",
    "Sherwood L. Washburn and C. S. Lancaster",
    "James Woodburn",
    "Joseph B. Birdsell",
    "Richard B. Lee and Irven DeVore",
    "Donald W. Lathrap",
    "James Woodburn",
    "William S. Laughlin",
    "Wayne Suttles",
    "Frederick L. Dunn",
    "Aram A. Yengoyan",
    "Colin M. Turnbull",
    "Leslie G. Freeman",
    "Lewis R. Binford",
    "Hitoshi Watanabe",
    "Asen Balikci",
    "George P. Murdock",
    "David Damas",
    "June Helm",
    "Julian H. Steward",
    "Glynn Ll. Isaac",
    "Claude Lévi-Strauss",
    "Ronald G. Rose",
    "L. R. Hiatt",
    "J. G. D. Clark",
    "Sally R. Binford",
    "Tribhuwan Nath Williams",
    "M. J. Meggitt",
    "L. R. Hiatt",
    "Arnold R. Pilling"
  ),
  stringsAsFactors = FALSE
)

base_url <- "https://api.openalex.org/works"
polite_email <- "edhagen@wsu.edu" # Recommended for OpenAlex polite pool
api_key <- "3ST7lHYmak030Y5GxHUwDu" # Optional: Replace with your OpenAlex API key if you have one

results_list <- vector("list", nrow(chapters))

cat("Querying OpenAlex API for chapter citations...\n")

for (i in seq_len(nrow(chapters))) {
  title <- chapters$Title[i]
  author <- chapters$Author[i]

  # Build query parameters dynamically to include api_key if provided
  query_params <- list(
    search = title,
    mailto = polite_email
  )

  if (!is.null(api_key) && api_key != "" && api_key != "YOUR_API_KEY_HERE") {
    query_params$api_key <- api_key
  }

  tryCatch(
    {
      response <- GET(base_url, query = query_params)

      if (status_code(response) == 200) {
        content_text <- content(response, as = "text", encoding = "UTF-8")
        data <- fromJSON(content_text, flatten = TRUE)

        if (!is.null(data$results) && length(data$results) > 0) {
          top_match <- data$results[1, ]
          results_list[[i]] <- data.frame(
            Chapter_Title = title,
            Author = author,
            OpenAlex_Match = ifelse(
              "title" %in% names(top_match),
              top_match$title,
              NA
            ),
            Citations = ifelse(
              "cited_by_count" %in% names(top_match),
              top_match$cited_by_count,
              0
            ),
            OpenAlex_ID = ifelse("id" %in% names(top_match), top_match$id, NA),
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[i]] <- data.frame(
            Chapter_Title = title,
            Author = author,
            OpenAlex_Match = "Not Found",
            Citations = 0,
            OpenAlex_ID = NA,
            stringsAsFactors = FALSE
          )
        }
      } else {
        results_list[[i]] <- data.frame(
          Chapter_Title = title,
          Author = author,
          OpenAlex_Match = paste("HTTP Error", status_code(response)),
          Citations = NA,
          OpenAlex_ID = NA,
          stringsAsFactors = FALSE
        )
      }
    },
    error = function(e) {
      results_list[[i]] <- data.frame(
        Chapter_Title = title,
        Author = author,
        OpenAlex_Match = paste("Error:", e$message),
        Citations = NA,
        OpenAlex_ID = NA,
        stringsAsFactors = FALSE
      )
    }
  )

  # Polite delay between requests to avoid rate limits
  Sys.sleep(0.2)
}

# Combine results into a single data frame
final_df <- bind_rows(results_list)

# Display summary in console
print(final_df[, c("Chapter_Title", "Citations")])

# Export to CSV
write.csv(final_df, "man_the_hunter_openalex_citations.csv", row.names = FALSE)
cat("\nSaved results to 'man_the_hunter_openalex_citations.csv'.\n")
