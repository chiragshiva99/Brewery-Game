library(shiny.router)

#root_page <- div(h2("Root page"))
#other_page <- div(h3("Other page"))

router <- make_router(
  route("/", root_page),
#  route("other", other_page)
)



#server <- function(input, output, session) {
#  router$server(input, output, session)
#}