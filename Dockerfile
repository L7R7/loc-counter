FROM eclipse-temurin:27_35-jre

RUN apt-get update && \
    apt-get install -y --no-install-recommends cloc curl git sqlite3 unzip && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY app.jar loc-counter.jar
COPY static/ static/

ENTRYPOINT ["sh"]
