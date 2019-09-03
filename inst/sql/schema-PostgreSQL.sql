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


DROP FUNCTION IF EXISTS archi_insert CASCADE;
CREATE FUNCTION archi_insert() RETURNS trigger
LANGUAGE plpgsql
AS $_$
BEGIN
IF not exists(SELECT 1
FROM   pg_catalog.pg_class c
JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE  n.nspname = 'public'
AND    c.relname = 'archi_' || NEW.tag) then

EXECUTE 'CREATE TABLE archi_' || NEW.tag || ' (CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (archi)';
EXECUTE 'ALTER TABLE archi_' || NEW.tag || ' ADD CONSTRAINT archi_check_' || NEW.tag || ' CHECK (tag = ''' || NEW.tag || ''')';
EXECUTE 'create unique index archi_unique_' || NEW.tag || ' on archi_' || NEW.tag || ' (partenza, arrivo)';

end if;

EXECUTE 'INSERT INTO archi_' || NEW.tag ||'(tag, partenza, arrivo, autore) ' || ' SELECT ($1).tag, ($1).partenza, ($1).arrivo, ($1).autore'
USING NEW;

RETURN NULL;

END;
$_$;



DROP FUNCTION IF EXISTS dati_insert CASCADE;
CREATE FUNCTION dati_insert() RETURNS trigger
LANGUAGE plpgsql
AS $_$
BEGIN
IF not exists(SELECT 1
FROM   pg_catalog.pg_class c
JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE  n.nspname = 'public'
AND    c.relname = 'dati_' || NEW.tag) then

EXECUTE 'CREATE TABLE dati_' || NEW.tag || ' (CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (dati)';
EXECUTE 'ALTER TABLE dati_' || NEW.tag || ' ADD CONSTRAINT dati_check_' || NEW.tag || ' CHECK (tag = ''' || NEW.tag || ''')';
EXECUTE 'create unique index dati_unique_' || NEW.tag || ' on dati_' || NEW.tag || ' (name)';

end if;

EXECUTE 'INSERT INTO dati_' || NEW.tag ||'(tag, name, anno, periodo, freq, dati, autore, stato, notes, stock) ' || ' SELECT ($1).tag, ($1).name, ($1).anno, ($1).periodo, ($1).freq, ($1).dati, ($1).autore, ($1).stato, ($1).notes, ($1).stock'
USING NEW;

RETURN NULL;


END;
$_$;



DROP FUNCTION IF EXISTS formule_insert CASCADE;
CREATE FUNCTION formule_insert() RETURNS trigger
LANGUAGE plpgsql
AS $_$
BEGIN
IF not exists(SELECT 1
FROM   pg_catalog.pg_class c
JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE  n.nspname = 'public'
AND    c.relname = 'formule_' || NEW.tag) then

EXECUTE 'CREATE TABLE formule_' || NEW.tag || ' (CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (formule)';
EXECUTE 'ALTER TABLE formule_' || NEW.tag || ' ADD CONSTRAINT formule_check_' || NEW.tag || ' CHECK (tag = ''' || NEW.tag || ''')';
EXECUTE 'create unique index formule_unique_' || NEW.tag || ' on formule_' || NEW.tag || ' (name)';

end if;

EXECUTE 'INSERT INTO formule_' || NEW.tag ||'(tag, name, formula, autore, commenti) '
|| ' SELECT ($1).tag, ($1).name, ($1).formula, ($1).autore, ($1).commenti'
USING NEW;

RETURN NULL;

END;
$_$;


DROP FUNCTION IF EXISTS metadati_insert CASCADE;
CREATE FUNCTION metadati_insert() RETURNS trigger
LANGUAGE plpgsql
AS $_$
BEGIN
IF not exists(SELECT 1
FROM   pg_catalog.pg_class c
JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE  n.nspname = 'public'
AND    c.relname = 'metadati_' || NEW.tag) then

EXECUTE 'CREATE TABLE metadati_' || NEW.tag || ' (CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (metadati)';
EXECUTE 'ALTER TABLE metadati_' || NEW.tag || ' ADD CONSTRAINT metadati_check_' || NEW.tag || ' CHECK (tag = ''' || NEW.tag || ''')';
EXECUTE 'create unique index metadati_unique_' || NEW.tag || ' on metadati_' || NEW.tag || ' (name, key, value)';

end if;

EXECUTE 'INSERT INTO metadati_' || NEW.tag ||'(tag, name, key, value, autore) '
|| ' SELECT ($1).tag, ($1).name, ($1).key, ($1).value, ($1).autore'
USING NEW;

RETURN NULL;

END;
$_$;
--
-- PostgreSQL database dump
--

DROP TABLE IF EXISTS archi CASCADE;
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
DROP TABLE IF EXISTS archi_cf10 CASCADE;
CREATE TABLE archi_cf10 (
CONSTRAINT archi_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (archi);



--
--
DROP TABLE IF EXISTS conflitti CASCADE;

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
DROP TABLE IF EXISTS dati CASCADE;

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
DROP TABLE IF EXISTS dati_cf10 CASCADE;

CREATE TABLE dati_cf10 (
CONSTRAINT dati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (dati);



--
--
DROP TABLE IF EXISTS formule CASCADE;
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
DROP TABLE IF EXISTS formule_cf10 CASCADE;
CREATE TABLE formule_cf10 (
CONSTRAINT formule_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (formule);



--
--
DROP TABLE IF EXISTS metadati CASCADE;

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
DROP TABLE IF EXISTS metadati_cf10 CASCADE;
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

--
-- Name: grafi; Type: TABLE; Schema: public; Owner: cfin; Tablespace:
--
DROP TABLE IF EXISTS grafi CASCADE;
CREATE TABLE grafi (
id integer NOT NULL,
tag character varying(256) NOT NULL,
commento text,
last_updated0 timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone NOT NULL,
autore character varying(256) NOT NULL,
stato smallint,
last_updated double precision
);


--
-- Name: grafi_id_seq; Type: SEQUENCE; Schema: public; Owner: cfin
--

CREATE SEQUENCE grafi_id_seq
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO MINVALUE
CACHE 1;


--
-- Name: grafi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cfin
--

ALTER SEQUENCE grafi_id_seq OWNED BY grafi.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: cfin
--

ALTER TABLE ONLY grafi ALTER COLUMN id SET DEFAULT nextval('grafi_id_seq'::regclass);


--
--

ALTER TABLE ONLY grafi
ADD CONSTRAINT grafi_tag_key UNIQUE (tag);


--
--

CREATE INDEX last_updated_index ON grafi USING btree (last_updated);



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

