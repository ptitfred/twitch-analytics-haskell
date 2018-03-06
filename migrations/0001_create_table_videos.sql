CREATE TABLE videos (
  url         TEXT,
  title       TEXT,
  description TEXT
);

ALTER TABLE videos ADD CONSTRAINT url_unique_key UNIQUE (url);
