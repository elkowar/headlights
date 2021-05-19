# Headlights

A simple redirection service for elks on streets.

## Launching

Launch the service using
```sh
swipl -l server.pl -g "server(8000)."
```

## Configuration format

Place the configuration file at the execution root, named `links.yaml`.
Alternatively, set the `HEADLIGHTS_LINKS_PATH` environment-variable to control where the file is placed.

The configuration file is formatted like this:
```yml
subdomain1:
  path1: https://target-url1
  path2: https://target-url2
subdomain2:
  path1: https://target-url3
  path2: https://target-url4
```
