# cs424-Project1

NOTE: I highly recommend using Rstudio as your IDE for this application

Required libraries:
- Shiny
- ggplot2
- DT
- usmap
- scales

Please install the required libraries with 'install.packages("<library>") in the console.

Credits for code snippets (Only needed minor modifications to fit my solution. These guys are awesome!)

- Generic add units code by user 'Tung' at https://stackoverflow.com/questions/52602503/display-an-axis-value-in-millions-in-ggplot
- Select all feature by user 'yihui' at https://github.com/rstudio/shiny/issues/42
- Reduce DT table text size by user 'rsoren' at https://stackoverflow.com/questions/25069224/rstudio-shiny-renderdatatable-font-size#:~:text=You%20can%20put%20dataTableOutput("tableName,style%20takes%20CSS%20arguments.&text=to%20make%20the%20font%20size%2020%25%20smaller.

Steps to host this app on your system:

1. Open Rstudio
2. Set your working directory to the Shiny App folder with "setwd("<path>") NOTE: '\' is an escape character
3. Ensure you have the required libraries installed
4. Open either "server.R" or "ui.R" in Rstudio using "File -> Open File" on the top left
5. Click the "run app" option next to a green horizontal arrow on the top right NOTE: If you don't, try updating Rstudio
6. You can also publish the application to your Shiny account. The button is next to the 'run app' button from step 4

You'll need to "connect" your Shiny account to Rstudio. The steps to do that are:

1. Open Rstudio
2. Go to "Tools -> Global options"
3. In the "publishing" tab, select "connect"