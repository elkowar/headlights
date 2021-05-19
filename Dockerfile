FROM swipl:stable

COPY ./server.pl /app/server.pl
EXPOSE 8000
ENTRYPOINT ["swipl"]
CMD ["/app/server.pl", "--user=daemon", "--fork=false", "--port=8000"]
