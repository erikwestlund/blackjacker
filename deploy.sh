#!/bin/sh

cd /var/www/blackjacker.net
git pull origin master
docker-compose -f docker-compose.prod.yml build

CONTAINER=$(docker ps -q --filter="NAME=blackjacker")
mkdir -p /var/www/blackjacker.net/app/www/__assets__/
docker cp $CONTAINER:/usr/local/lib/R/site-library/shiny/www/shared/ /var/www/blackjacker.net/app/www/
docker cp $CONTAINER:/opt/shiny-server/assets/ /var/www/blackjacker.net/app/www/__assets__/
docker cp $CONTAINER:/opt/shiny-server/node_modules/shiny-server-client/dist/shiny-server-client.min.js /var/www/blackjacker.net/app/www/__assets__/

docker-compose -f docker-compose.prod.yml up -d