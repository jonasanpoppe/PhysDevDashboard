# Every page (its module id) and the navbar tab that holds it; a tab with
# more than one page shows them as pills (app/app.R). Quest links (link:
# {tab} in config/oath.yml) name a page; oath_follow() opens its tab and pill.
# Its own file so the tests can read it.
app_pages <- list(oath = "oath", reliquary = "oath", roadmap = "oath", scenarios = "oath",
                  weight = "body", strength = "body", vitals = "body",
                  engine = "engine", analytics = "analytics")
