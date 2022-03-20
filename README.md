# species-backend

## Setup & execution

As a `stack` project, you can launch the webserver by running `stack run`.

## Usage

Once the server is running, data can be accessed through the following ways.
The default port is `5000`.

### Frontend

A basic frontend written with Yesod resources
can be accessed under `http://localhost:3000`.

### Json endpoint

Alternatively, the server may respond with json content.
The curl command below will access the json endpoint.

`curl -F 'query=Panthera pardus' http://localhost:3000/search.json`
