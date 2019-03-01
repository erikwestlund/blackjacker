FROM rocker/shiny

MAINTAINER Erik Westlund

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]