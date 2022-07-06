# shiny model example

In this repo a shiny app is made, in which: interactive graphics are shown and simple regression model is integrated within the app. The app uses the following packages:

- tidyverse
- tidymodels
- ggiraph
- arrow
- geoarrow

The data collection and the model code are shown in the notebooks folder. All the functions used in the app are located in the src folder. Take the following steps, to run the app on your local machine:

1. pull code
2. set up local enviromnent (e.g. .Rproj)
3. install the aformentioned packages
4. run `notebooks/get_bbga_data.R`
5. run `notebooks/example_model.R`
6. run `example_app.R`
7. app will pop-up :) 
