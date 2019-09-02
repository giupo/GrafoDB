--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'LATIN9';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
--

CREATE TABLE archi (
    id integer NOT NULL,
    partenza character varying(256),
    arrivo character varying(256),
    tag character varying(256),
    autore character varying(256) NOT NULL,
    last_updated double precision DEFAULT (date_part('epoch'::text, now()))::bigint
);



--
--

CREATE SEQUENCE archi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



--
--

ALTER SEQUENCE archi_id_seq OWNED BY archi.id;


--
--

CREATE TABLE archi_cf10 (
    CONSTRAINT archi_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (archi);



--
--

CREATE TABLE conflitti (
    id integer NOT NULL,
    tag character varying(256) NOT NULL,
    name character varying(256) NOT NULL,
    anno smallint,
    prd smallint,
    freq smallint,
    dati text,
    autore character varying(256) NOT NULL,
    formula text,
    date bigint
);



--
--

CREATE SEQUENCE conflitti_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



--
--

ALTER SEQUENCE conflitti_id_seq OWNED BY conflitti.id;


--
--

CREATE TABLE dati (
    id integer NOT NULL,
    name character varying(256) NOT NULL,
    anno smallint,
    periodo smallint,
    freq smallint,
    dati text,
    tag character varying(256) NOT NULL,
    autore character varying(256) NOT NULL,
    stato smallint DEFAULT 0 NOT NULL,
    notes text,
    stock smallint DEFAULT 0,
    last_updated double precision DEFAULT (date_part('epoch'::text, now()))::bigint
);



--
--

CREATE SEQUENCE dati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



--
--

ALTER SEQUENCE dati_id_seq OWNED BY dati.id;


--
--

CREATE TABLE dati_cf10 (
    CONSTRAINT dati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (dati);



--
--

CREATE TABLE formule (
    id integer NOT NULL,
    name character varying(256),
    tag character varying(256),
    formula text,
    autore character varying(256) NOT NULL,
    commenti text,
    last_updated double precision DEFAULT (date_part('epoch'::text, now()))::bigint
);



--
--

CREATE SEQUENCE formule_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



--
--

ALTER SEQUENCE formule_id_seq OWNED BY formule.id;


--
--

CREATE TABLE formule_cf10 (
    CONSTRAINT formule_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (formule);



--
--

CREATE TABLE metadati (
    id integer NOT NULL,
    name text,
    key character varying(256),
    value text,
    tag character varying(256),
    autore character varying(256) NOT NULL,
    last_updated double precision DEFAULT (date_part('epoch'::text, now()))::bigint
);



--
--

CREATE SEQUENCE metadati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



--
--

ALTER SEQUENCE metadati_id_seq OWNED BY metadati.id;


--
--

CREATE TABLE metadati_cf10 (
    CONSTRAINT metadati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (metadati);



--
--

ALTER TABLE ONLY archi ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
--

ALTER TABLE ONLY conflitti ALTER COLUMN id SET DEFAULT nextval('conflitti_id_seq'::regclass);


--
--

ALTER TABLE ONLY dati ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
--

ALTER TABLE ONLY formule ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
--

ALTER TABLE ONLY metadati ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_name_tag_key" UNIQUE (name, tag);


--
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_pkey" PRIMARY KEY (id);


--
--

ALTER TABLE ONLY dati
    ADD CONSTRAINT dati_name_key UNIQUE (name);


--
--

CREATE UNIQUE INDEX archi_unique ON archi USING btree (partenza, arrivo);


--
--

CREATE UNIQUE INDEX dati_tag_name_index ON dati USING btree (tag, name);


--
--

CREATE UNIQUE INDEX formule_name_unique_cf10 ON formule USING btree (name);


--
--

CREATE UNIQUE INDEX formule_tag_name_unique ON formule USING btree (tag, name);


--
--

CREATE INDEX meta_key ON metadati USING btree (key);


--
--

CREATE UNIQUE INDEX meta_unique ON metadati USING btree (name, key, value);


--
--

CREATE INDEX meta_value ON metadati USING btree (value);


--
--

CREATE TRIGGER formule_insert_trigger
    BEFORE INSERT ON formule
    FOR EACH ROW
    EXECUTE PROCEDURE formule_insert();


--
--

CREATE TRIGGER insert_archi_trigger
    BEFORE INSERT ON archi
    FOR EACH ROW
    EXECUTE PROCEDURE archi_insert();


--
--

CREATE TRIGGER insert_dati_trigger
    BEFORE INSERT ON dati
    FOR EACH ROW
    EXECUTE PROCEDURE dati_insert();


--
--

CREATE TRIGGER insert_metadati_trigger
    BEFORE INSERT ON metadati
    FOR EACH ROW
    EXECUTE PROCEDURE metadati_insert();


--
-- PostgreSQL database dump complete
--

