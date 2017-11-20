FROM ipds/bev-base

MAINTAINER Geoffrey Arnold "geoffrey.arnold@pittsburghpa.gov"

# copy the app to the image
RUN mkdir /root/burghs-eye-view
COPY burghs-eye-view /root/burghs-eye-view

CMD ["R", "-e shiny::runApp('/root/burghs-eye-view')"]