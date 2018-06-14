CREATE TABLE accounts (
  id              INTEGER NOT NULL,
  name            TEXT NOT NULL,
  country         TEXT
);

CREATE TABLE identifiers (
  id              SERIAL,
  identifier      TEXT NOT NULL,
  identifier_type TEXT NOT NULL,
  account_id      INTEGER,
  created_at      TIMESTAMP WITH TIME ZONE NOT NULL
);

ALTER TABLE ONLY accounts
  ADD CONSTRAINT accounts_pkey
  PRIMARY KEY (id);

ALTER TABLE ONLY identifiers
  ADD CONSTRAINT identifiers_pkey
  PRIMARY KEY (identifier, identifier_type, created_at);

ALTER TABLE ONLY identifiers
  ADD CONSTRAINT identifiers_account_id_fkey
  FOREIGN KEY (account_id)
  REFERENCES accounts(id)
  ON DELETE RESTRICT;
