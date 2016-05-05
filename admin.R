# Application to facilitate machine learning training with the caret package

# Admin file

shiny::runApp(launch.browser = T)

rsconnect::setAccountInfo(name='dimitrif',token='ABCBF1767D2C0EAC7EC021089817A3B0',secret='TzjwT+xL0w7UMktrWN57dS5Ie/61JDdrybPfe9m9')
rsconnect::deployApp(account="dimitrif",appName="caret_app")

system("git remote add origin https://github.com/dimitrif/caret_app.git")
