
CREATE TABLE IF NOT EXISTS archi (
    id integer primary key autoincrement,
    partenza character varying(256),
    arrivo character varying(256),
    tag character varying(256),
    last_updated text,
    autore character varying(256) NOT NULL
);


CREATE TABLE IF NOT EXISTS capitalizzazioni (
    isin character(12) NOT NULL,
    name character varying(255) NOT NULL,
    mv real,
    descrizione character varying(255) NOT NULL,
    data_contabile integer NOT NULL,
    data text NOT NULL,
    autore character varying(255) NOT NULL
);


CREATE TABLE IF NOT EXISTS conflitti (
    id integer primary key autoincrement,
    tag character varying(256) NOT NULL,
    name character varying(256) NOT NULL,
    anno smallint,
    prd smallint,
    freq smallint,
    dati text,
    autore character varying(256) NOT NULL,
    date integer NOT NULL,
    formula text
);


CREATE TABLE IF NOT EXISTS dati (
    id integer primary key autoincrement,
    name character varying(256) NOT NULL,
    anno smallint,
    periodo smallint,
    freq smallint,
    dati text,
    tag character varying(256) NOT NULL,
    last_updated text NOT NULL,
    autore character varying(256) NOT NULL,
    stato smallint DEFAULT 0 NOT NULL,
    notes text,
    stock smallint DEFAULT 0
);


CREATE TABLE IF NOT EXISTS elaborazioni (
    id integer primary key autoincrement,
    name character varying(256) NOT NULL,
    inizio datetime NOT NULL,
    fine datetime,
    tag character varying(256) NOT NULL,
    status character varying(256) DEFAULT 'RUNNING',
    notes character varying(4096)
);


CREATE TABLE IF NOT EXISTS formule (
    id integer primary key autoincrement,
    name character varying(256) NOT NULL,
    tag character varying(256) NOT NULL,
    formula text NOT NULL,
    last_updated text NOT NULL,
    autore character varying(256) NOT NULL,
    commenti text
);

CREATE TABLE IF NOT EXISTS grafi (
    id integer primary key autoincrement,
    tag character varying(256) NOT NULL,
    commento text,
    last_updated text NOT NULL,
    autore character varying(256) NOT NULL
);


CREATE TABLE IF NOT EXISTS history (
    name character varying(255) NOT NULL,
    tag character varying(255) NOT NULL,
    ordinale smallint NOT NULL,
    anno smallint NOT NULL,
    periodo smallint NOT NULL,
    freq smallint NOT NULL,
    dati text NOT NULL,
    formula text,
    archi_entranti text,
    last_updated text NOT NULL,
    autore character varying(255) NOT NULL
);

CREATE TABLE IF NOT EXISTS metadati (
    id integer primary key autoincrement,
    name text,
    key character varying(256),
    value text,
    tag character varying(256),
    last_updated text,
    autore character varying(256) NOT NULL
);

CREATE TABLE IF NOT EXISTS producers (
    id integer primary key autoincrement,
    name character varying(255) NOT NULL,
    starting datetime,
    ending datetime,
    command text NOT NULL,
    type character varying(8) NOT NULL,
    category character varying(255),
    freq smallint
);



CREATE UNIQUE INDEX archi_unique ON archi(partenza, arrivo, tag);

CREATE UNIQUE INDEX dati_tag_name_index ON dati(tag, name);

CREATE INDEX meta_key ON metadati(key); 

CREATE UNIQUE INDEX meta_unique ON metadati(name, key, value, tag);

CREATE INDEX meta_value ON metadati(value);

