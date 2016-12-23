CREATE TABLE IF NOT EXISTS StorePaths (
  id   SERIAL PRIMARY KEY,
  path TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS StorePathContents (
  storePath INTEGER NOT NULL,
  subPath TEXT NOT NULL,
  file TEXT NOT NULL,
  type INTEGER NOT NULL,
  isExecutable BOOLEAN NOT NULL,
  fileSize INTEGER,
  target TEXT,
  PRIMARY KEY (storePath, subPath),
  FOREIGN KEY (storePath) REFERENCES StorePaths(id) ON DELETE CASCADE
);


-- TODO: M2M for channels
-- TODO: eval nixpkgs, add system and attribute
