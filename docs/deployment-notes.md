
# To setup server and database

- Ubuntu 16.04 on Linode
- Dokku 0.10.5

```bash
# Remote
dokku apps:create parliament-api
dokku plugin:install https://github.com/dokku/dokku-postgres.git
dokku postgres:create parliament-database
dokku postgres:link parliament-database parliament-api

# Local
git remote add dokku dokku@li1514-40.members.linode.com:parliament-api
```


# To access server database

```bash
dokku postgres:enter parliament-database
psql -U postgres
```


# To setup database data

Below runs setup script locally to import data remotely, could possibly have
this run remotely (and automatically?).

```bash
# Remote
dokku postgres:expose parliament-database # Temporarily expose database on server port.
dokku postgres:info parliament-database # To find database password.

# Local
export DATABASE_URL=postgres://postgres:password@ip:port # Including server IP, and password and port found above.
export TWFY_API_KEY=XXX # TWFY API key created as described in README.
bin/setup

# Remote
dokku postgres:unexpose parliament-database
```


# To deploy

```bash
git push dokku master # Server
git push origin master # Client
# Can also re-run database setup if necessary.
```
