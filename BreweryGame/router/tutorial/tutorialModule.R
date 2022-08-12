### Code was done by HaoHong

## The module loads the tabItem tutorial
tutorialModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "tutorialTab",
    fluidRow(
      column(
        width=12,
        h1("Introduction"),
        tags$p(
          "Littlefield Brewery opened its first and only Brewery with 3 beer types. You will have to manage this brewery for the next 490 days. From the ordering of raw materials to the service of customers, you will have to do it all. However, you also have the option to automate all these processes! Set reorder quantities and reorder points for your various materials and beers and let the brewery run. You are parachuted in on day 51 with the customer demand data from the past 50 days. You will also have to set automation parameters for the last 100 simulated days where you have no control over the brewery."
        ),
        h2("Game Instructions"),
        tags$p(
          "Upon starting game, you will see this broad overview"
        ),
        tags$img(
          src="tutorial/mainpage.png",
          style="width:70%;"
        ),
        tags$p(
          "You will be starting off at Day 51, and can view historical data as well as current demand"
        ),
        tags$img(
          src="tutorial/keyfigures.png",
          style="width:35%;"
        ),
        tags$p(
          "To view current demand, you can look at Progress Tracker under Total Demand tab"
        ),
        tags$img(
          src="tutorial/totalDemand.png",
          style="width:35%;"
        ),
        tags$p(
          "For historical demand data from day 0 to day 50, you can click on analysis page on the left column"
        ),
        tags$img(
          src="tutorial/analysis.png",
          style="width:70%;"
        ),
        tags$p(
          "You can take a look at the raw materials available in inventory and beer inventory as well. This is located under inventory. You can see a summary under All and Specifics under Material and Beer."
        ),
        tags$img(
          src="tutorial/inventory.png",
          style="width:35%;"
        ),
        tags$p(
          "If needed, you can purchase materials under the Action tab."
        ),
        tags$img(
          src="tutorial/purchaseMaterial.png",
          style="width:35%;"
        ),
        tags$p(
          "Costs would be updated upon filling in quantity, you should take note of the updated cash balance as well as lead time needed on purchase. "
        ),
        tags$img(
          src="tutorial/purchaseMaterialNumber.png",
          style="width:35%;"
        ),
        tags$p(
          "A purchase pop up will appear after confirming purchase"
        ),
        tags$img(
          src="tutorial/purchaseNotice.png",
          style="width:35%;"
        ),
        tags$p(
          "Post purchase, you can take note of the progress for material orders under the Progress Tracker tab as well as remaining cash balance under the Cash Balance value box. Cash balance has an interest of 10%.
"
        ),
        tags$img(
          src="tutorial/cashLeftAfterPurchase.png",
          style="width:35%;"
        ),
        tags$p(
          "Thereafter, you can brew beer with existing materials and choose type of beer and tank to use"
        ),
        tags$img(
          src="tutorial/brewBeer.png",
          style="width:35%;"
        ),
        tags$p(
          "To check progress on beer being brewed, look at Progress tracker under tab Tanks"
        ),
        tags$img(
          src="tutorial/tanks.png",
          style="width:35%;"
        ),
        tags$p(
          "You can serve customers the beer under Actions, Serve Customers. Serving can be automated using the button Auto Serve, alternatively it has to be manually clicked "
        ),
        tags$img(
          src="tutorial/serveCustomer.png",
          style="width:35%;"
        ),
        tags$p(
          "Under the Automate tab, you can automate all work or selective portions of work with the buttons provided."
        ),
        tags$img(
          src="tutorial/automate.png",
          style="width:35%;"
        ),
        tags$p(
          "To automate reordering of materials and rebrew points, reorder quantities and reorder points need to be set and submitted. Storage on auto mode is done without any priority order.
"
        ),
        tags$img(
          src="tutorial/materialAutomate.png",
          style="width:35%;"
        ),
        tags$img(
          src="tutorial/automateAllOn.png",
          style="width:35%;"
        ),
        tags$p(
          "You are permitted 3 beer related actions a day, and can monitor based off the Beer Actions taken tab. These beer actions are brewing and storing of beer."
        ),
        tags$img(
          src="tutorial/beerAction.png",
          style="width:35%;"
        ),
        tags$p(
          "Upon completion, you should go towards the next day and click the Advance next Day button. you can then complete new actions for that day. The day tab will be updated as well reflecting the progress. You can also advance more days as you like, through setting the number of days in the field.
"
        ),
        tags$img(
          src="tutrial/advance.png",
          style="width:35%;"
        ),
        tags$p(
          "You can monitor customers lost under the tab Total Customer Lost, as well as Lost by Beer. Customers leave after a certain amount of time as their needs are met.
"
        ),
        tags$img(
          src="tutorial/customerLost.png",
          style="width:35%;"
        ),
        tags$p(
          "While playing the game, you can view the Analysis tab to monitor growth and how demand is being met by inventory"
        ),
        tags$p(
          "After hitting day 390, the remaining days would be automatically simulated by system for the remaining 100 days. This can be done by clicking advance the next day."
        ),
        tags$img(
          src="tutorial/simulate100.png",
          style="width:35%;"
        ),
        tags$p(
          "Results can be seen after for cash balance, for customers wait time and more information is available on the Analysis page."
        )
      )
    )
  )
}

tutorialModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}