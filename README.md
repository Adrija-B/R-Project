# MSBX 5420 Assignment 1
# HDFS Commands and Python MapReduce
# <p align="center">Adrija Barvadheesh </p>


## Using this repo
### With `docker`
Build:

```bash
docker build --rm -t adrijab/my-datascience-notebook .
#This uses the custom image for jupyter datascience notebook, as built on dockerhub. It is mounted to the current directry.
#The Jupyter Docker Stack used is 'jupyter/my-datascience-notebook', which had the 2022-01-24 tag and was retagged as adrijab/my-datascience-notebook, using the docker tag command.
```

Run:

```bash
docker run --rm -it -p 8888:8888 adrijab/my-datascience-notebook
# - Publishes on port 8888
# - Can also be opened using localhost:8888
```

### With `docker-compose`
Build and run:

```bash
docker-compose up
# This command publishes through port 8888 and custom image adrijab/my-datascience-notebook, as defined in the docker-compose.yml file
#Run this command from the repository terminal/command prompt to build your own container 
```
