#!/bin/sh

cd /var/www/blackjacker.net
git pull origin master

# build and go up once so we can grab files out of the container
docker-compose -f docker-compose.prod.yml build
docker-compose -f docker-compose.prod.yml up -d

# hacky, but grab needed files out of container so we can serve in nginx directly
# proxying, nginx into docker, these don't get served.
CONTAINER=$(docker ps -q --filter="NAME=blackjacker")
SHINYWWW=/var/www/blackjacker.net/app/www
DUNDERASS=$SHINYWWW/__assets__/
SHAREDASS=$SHINYWWW/assets/shared/

#remove shared assets and recopy
rm -rf $SHAREDASS
docker cp $CONTAINER:/usr/local/lib/R/site-library/shiny/www/shared/ $SHINYWWW/

# remove dunderbar shared assets and recopy
rm -rf $DUNDERASS
docker cp $CONTAINER:/opt/shiny-server/assets/ $DUNDERASS

# add import websocket stuff
docker cp $CONTAINER:/opt/shiny-server/node_modules/shiny-server-client/dist/shiny-server-client.min.js $DUNDERASS/

docker-compose -f docker-compose.prod.yml build
docker-compose -f docker-compose.prod.yml up -d
