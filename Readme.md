# Headlights

A simple redirection service for elks on popular routes.

## Launching

Launch the service using
```sh
swipl server.pl --port=8000 --fork=false
```

## Configuration format

The route mappings must be available under some public url.
The URL must be provided via the environment variable `HEADLIGHTS_LINKS_URL`.

The mappings are formatted like this:
```yml
subdomain1:
  path1: https://target-url1
  path2: https://target-url2
subdomain2:
  path1: https://target-url3
  path2: https://target-url4
```
