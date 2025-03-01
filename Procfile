web: gunicorn server:app
web: R -e "shiny::runApp('app.R', port=as.numeric(Sys.getenv('PORT')), host='0.0.0.0')"

