# Local Hosting

#shiny::runApp("Risk-Heatmap.R",host = "::", port= 3838)
#cmd
#ipconfig
#IPv6 address
#http://[2a02:8086:d00:e800:4b22:82e4:1a35:19ec]:3838

#---------------------------

# Online Hosting

install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name='headline-grabber',
                          token='C058517295C62C438DDC7B16D90BD5F8',
                          secret='P3XpMpcOP3mVrF9hVg4ZvWqBBKJ3pMrkbVRdqm79')

#Open the Shiny app you want to publish in RStudio.
#Go to File > Publish to Server > shinyapps.io.
#A dialog will pop up, allowing you to name the app and set any additional options.
#Click Publish, and RStudio will upload your app to shinyapps.io. You’ll see a progress bar and messages as the app is deployed.
#Once the process completes, RStudio will provide a link to your live app (e.g., https://yourusername.shinyapps.io/appname). You can share this link with others to access the app online.
