FROM swipl:latest
COPY . /app

ENV HOST=127.0.0.1
ENV PORT=8001

EXPOSE $PORT

CMD swipl -s /app/src/app.pl -g "server('$HOST', $PORT), sleep(9999999), halt."