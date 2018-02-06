CREATE TABLE tags (
  tag       TEXT,
  video_url TEXT
);

ALTER TABLE tags ADD CONSTRAINT tag_video_unique_key UNIQUE (tag, video_url);
