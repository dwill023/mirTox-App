# mirTox-App

This app aims to help analyze microRNAs within developmental toxicologically challenged human embryonic stem cells and integrate RNA-seq (of the same samples) to correlate microRNA regulation with target gene expression.

Identified microRNAs from this method may have the potential to be used as biomarkers to screen for early bone developmental defects.

The app is hosted [here](https://willdesi.shinyapps.io/mirTox-App/).

## Docker 

The [Dockerfile](https://github.com/dwill023/mirTox-App/blob/main/Dockerfile) in the main directory can be used to build an image to run the app in a container using the following commands:

```
# to build the image on a mac with ARM architecture.
docker build --platform=linux/amd64 -t mirtox-app .

# else if you're building the image on linux 
docker build -t mirtox-app .
```

To run the container:
```
docker container run --rm -p 3838:3838 mirtox-app
```

The app will be visible at http://localhost:3838/


