FROM rocker/shiny:3.4.4

MAINTAINER Erik Westlund

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]