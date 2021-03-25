
# UK Parliament Votes Visualization

This is an interactive visualization of UK Parliament (House of Commons) votes
since 1997; it can be viewed online
[here](http://parliament.bobwhitelock.co.uk).


## How it works

- A series of scripts in the [`bin/`](bin) directory handle retrieving various
  public data on UK MPs and how they voted, made available by
  [TheyWorkForYou](https://www.theyworkforyou.com/), and extracting the
  relevant parts of this data into a Postgres database. The
  [`bin/setup`](bin/setup) script demonstrates the order in which these should
  typically be run.

- A tiny [Sinatra](http://sinatrarb.com/) server,
  [`server/server.rb`](server/server.rb), serves up the needed data from this
  database.

- An [Elm](http://elm-lang.org/) web app, with entry point
  [`client/src/Main.elm`](client/src/Main.elm), handles most of the UI,
  including making requests as needed for the above data.

- This communicates with JavaScript, with entry point
  [`client/src/index.js`](client/src/index.js), via ports to render the
  visualization itself; in particular
  [`client/src/BubbleChart.js`](client/src/BubbleChart.js) renders a bubble
  chart using the [D3](https://d3js.org/) force layout, and appropriately
  updates this with any new data sent from the Elm app.


## Development

The web app itself can be developed without needing to run a local server by
using the live version of the server, like this:

```bash
cd client
yarn install

export ELM_APP_API_URL=https://api.parliament.bobwhitelock.co.uk
yarn run start
```

To develop the server or data retrieval scripts, a new empty Postgres database
must be created and a [TheyWorkForYou API key
created](https://www.theyworkforyou.com/api/), and then:

```bash
export DATABASE_URL=# URL to access the database created above.
export TWFY_API_KEY=# API key created above.
bin/setup

ruby server/server.rb
```

To use a local server with a local client, the value of `ELM_APP_API_URL` used
when starting the client will need to be changed to point to this local server,
usually like this:

```bash
export ELM_APP_API_URL=http://localhost:4567
```
