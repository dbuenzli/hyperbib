Hyperbib
========

> Annotates bibliographies

## Installation

### Pre-requisites

In order to install and run Hyperbib you need: 

* `curl`
* OCaml >= 4.12.0 
* Sqlite3 >= v3.26.2
* A bunch of OCaml libraries.

## Using the web application with a webserver 

### Systemd 

Choose a service path prefix `$HYPERBIB_SERVICE_PATH` on your website on 
which you  want to publish your website and find a suitable value for
these variables:

```
export HYPERBIB_APP_DIR=/var/www/hyperbib
export HYPERBIB_LISTEN=localhost:8000
export HYPERBIB_SERVICE_PATH=/mybibliography/ 
export HYPERBIB_USER=`whoami`
```

Now make a systemd service file. You may want to adjust a few 
other parameters here and/or tweak the service 

```
cat - > hyperbib.service <<EOF

[Unit]
Description=Hyperbib

[Service]
Type=simple
Restart=on-failure
RestartSec=10s
User=$HYPERBIB_USER
Group=$HYPERBIB_USER

ExecStart=hyperbib serve \
          --listen $HYPERBIB_LISTEN \
          --service-path $HYPERBIB_SERVICE_PATH \
          --app-dir $HYPERBIB_APP_DIR

[Install]
WantedBy=multi-user.target
EOF
```

Install and start the service: 

```
cp hyperbib.service /etc/systemd/system/hyperbib.service
systemctl daemon-reload
systemctl start hyperbib 
```

Checkout the logs: 

```
sudo journalctl -u hyperbib -f -a
```

Stop the service:

```
systemctl stop hyperbib
```

### Nginx

In your nginx configuration add a location block with (substitute variables 
with actual values as defined above):

```
location $HYPERBIB_SERVICE_PATH
{
    proxy_http_version 1.1;
    proxy_pass http://$HYPERBIB_LISTEN/; # final slash is important
}
```

## Publishing as static HTML

The static HTML output can be used to publish a bibliography as a set
of static HTML files. 

Unless you use the `--file-browsable` option, the links in HTML files
lack the `.html` suffixe so you will have to instruct your webserver
to add them to hit the files. This allows to swap in and out the
`hyperbib` service without breaking links.

If you use the `--file-browsable` option the set of HTML files can be
browsed without a webserver via the `file://` protocol.

One use case it enables is to edit the bibliography locally and
publish it via a simple webserver:

```
hyperbib serve -a . --        # Edit your bibliography locally.
rm -rf /var/www/mybib     
hyperbib html app /var/www/mybib 
```

## Backup

By default `hyperbib serve` makes a stable copy of the SQLite data
base every XXX hours under `app/data/bib.sqlite3.backup` (use
`--no-backup` to disable this behaviour) alongside the live 
database. 

If the machine you are running the service on has a file system
backup, it should be enough to add the `app/data` application
directory the backup.

## Admin tasks

### Logging out all users 

User sessions are stored on the client and authenticated 
by the private key `app/auth.private` 

```
rm app/auth.private
sytemctl restart hyperbib 
```




