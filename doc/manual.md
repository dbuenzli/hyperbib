Hyperbib
========

> Annotates bibliographies

## Installation

### Pre-requisites

In order to install and run Hyperbib you need: 

* `curl`
* `pdftotext` (`apt install poppler-utils`)
* OCaml >= 5.4.0
* Sqlite3 >= v3.26.2
* A bunch of OCaml libraries.

## Playing with the web application on `localhost`

By default you can run `hyperbib` and it will write the data to
appropriate XDG directories, see `hyperbib config` to see which paths
it accesses.  If you want the data to be written in the current
working directory do a `mkdir hyperbib`, this directory will be used
for writing the data.

```
hyperbib serve --editable=unsafe
hyperbib serve --editable=unsafe --insecure-cookie # For safari
```

There are also a few command that can be used as is, see 
`hyperbib --help` for more information.

## Using the web application with a webserver 

### Systemd 

Choose a service path prefix `$HYPERBIB_SERVICE_PATH` on your website on 
which you want to publish your website and find a suitable value for
these variables which we are used in the subsequent instructions.

```
export HYPERBIB_DATA_DIR=/var/lib/hyperbib
export HYPERBIB_CACHE_DIR=/var/cache/
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
RestartSec=5s
User=$HYPERBIB_USER
Group=$HYPERBIB_USER

ExecStart=hyperbib serve \
          --listen $HYPERBIB_LISTEN \
          --service-path $HYPERBIB_SERVICE_PATH \
          --data-dir $HYPERBIB_DATA_DIR \
          --cache-dir $HYPERBIB_CACHE_DIR

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
hyperbib serve        # Edit your bibliography locally.
rm -rf /var/www/mybib     
hyperbib export html /var/www/mybib 
```

## Backup

By default `hyperbib serve` makes a stable copy of the SQLite data
base every XXX hours in the `bib` subdirectory of the 
data directory  as `bib.sqlite3.backup` (use
`--no-backup` to disable this behaviour) alongside the live 
database. 

If the machine you are running the service on has a file system
backup, it should be enough to add the `$(hyperbib config
--show-data-dir)/bib` directory to your backup (this won't save users
though).

## Admin tasks

### Logging out all users 

User sessions are stored on the client and authenticated 
by the private key `$(hyperbib config --show-data-dir)/auth.private` 

```
rm $(hyperbib config --show-data-dir)/auth.private
sytemctl restart hyperbib 
```




