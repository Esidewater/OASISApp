web: gunicorn server:app
web: R -e "shiny::runApp('app.json', port=as.numeric(Sys.getenv('PORT')), host='0.0.0.0')"

