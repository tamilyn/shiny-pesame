## Running using Docker

You can run this using via Docker
with the following command

```
docker run -it --rm -p 8787:8787 -p 3838:3838 \
        -v $(pwd):/home/rstudio \
        --name myrstudio rocker/rstudio
```

This maps the current directory to /home/rstdio on the docker container.
Then you can set the r library path to be this directory,
so that the libraries will only need to be downloaded the first time
you run it (otherwise, all the packages will be downloaded 
everytime, currently takes tens of minutes) 

at the R console, 

```
mkdir rlibs
.libPaths("/home/rstdio/rlibs")
```

If you do this, then the library will be stored on your local disk, 
which means that the libraries will only have to be downloaded the first 
time.

Access rstudio at localhost:8787 with 
username/password rstudio/rstudio.

# Running without Docker

open the rstudio project.  when you run the shiny app, it should
call install-packages.R to download any packages that are not 
already available.

