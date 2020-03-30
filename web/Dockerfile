# base image
FROM node:12.16.1-alpine

# set working directory
WORKDIR /app

# add `/app/node_modules/.bin` to $PATH
ENV PATH /app/node_modules/.bin:$PATH

# copy files to install js modules
COPY package.json /app/

# install dependencies
RUN npm install

# copy source files
COPY public /app/public/
COPY src /app/src/

# start app
CMD npm start
