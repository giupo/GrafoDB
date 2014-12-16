--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: archi_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION archi_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'archi_' || NEW.tag) then

       EXECUTE 'CREATE TABLE IF NOT EXISTS archi_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (archi)';       
       -- EXECUTE 'ALTER TABLE archi_' || NEW.tag || ' ADD PRIMARY KEY (partenza, arrivo)';
    end if;


    EXECUTE 'INSERT INTO archi_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.archi_insert() OWNER TO m024000;


--
-- Name: dati_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION dati_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$BEGIN

    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'dati_' || NEW.tag) then

       EXECUTE 'CREATE TABLE IF NOT EXISTS dati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (dati)';       
       -- EXECUTE 'ALTER TABLE dati_' || NEW.tag || ' ADD PRIMARY KEY (name)';

    end if;
    EXECUTE 'INSERT INTO dati_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.dati_insert() OWNER TO m024000;

--
-- Name: formule_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION formule_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'formule_' || NEW.tag) then

       EXECUTE 'CREATE TABLE IF NOT EXISTS formule_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (formule)';       
       -- EXECUTE 'ALTER TABLE formule_' || NEW.tag || ' ADD PRIMARY KEY (name, tag)';
    end if;


    EXECUTE 'INSERT INTO formule_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.formule_insert() OWNER TO m024000;

--
-- Name: metadati_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION metadati_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$BEGIN

    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'metadati_' || NEW.tag) then

       EXECUTE 'CREATE TABLE IF NOT EXISTS metadati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (metadati)';       
       -- EXECUTE 'ALTER TABLE metadati_' || NEW.tag || ' ADD PRIMARY KEY (name, key, value)';
    end if;

    EXECUTE 'INSERT INTO metadati_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.metadati_insert() OWNER TO m024000;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: conflitti; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
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
    date timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone NOT NULL,
    formula text
);


ALTER TABLE public.conflitti OWNER TO m024000;

--
-- Name: Conflitti_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE "Conflitti_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Conflitti_id_seq" OWNER TO m024000;

--
-- Name: Conflitti_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE "Conflitti_id_seq" OWNED BY conflitti.id;


--
-- Name: archi; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi (
    id integer NOT NULL,
    partenza character varying(256),
    arrivo character varying(256),
    tag character varying(256),
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone,
    autore character varying(256) NOT NULL
);


ALTER TABLE public.archi OWNER TO m024000;

--
-- Name: archi_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE archi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.archi_id_seq OWNER TO m024000;

--
-- Name: archi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE archi_id_seq OWNED BY archi.id;


--
-- Name: archi_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_test1 (
    CONSTRAINT archi_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (archi);
ALTER TABLE ONLY archi_test1 ALTER COLUMN partenza SET NOT NULL;
ALTER TABLE ONLY archi_test1 ALTER COLUMN arrivo SET NOT NULL;


ALTER TABLE public.archi_test1 OWNER TO m024000;

--
-- Name: dati; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati (
    id integer NOT NULL,
    name character varying(256),
    anno smallint,
    periodo smallint,
    freq smallint,
    dati text,
    tag character varying(256),
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone,
    autore character varying(256) NOT NULL
);


ALTER TABLE public.dati OWNER TO m024000;

--
-- Name: dati_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE dati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.dati_id_seq OWNER TO m024000;

--
-- Name: dati_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE dati_id_seq OWNED BY dati.id;


--
-- Name: dati_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_test1 (
    CONSTRAINT dati_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (dati);
ALTER TABLE ONLY dati_test1 ALTER COLUMN name SET NOT NULL;


ALTER TABLE public.dati_test1 OWNER TO m024000;

--
-- Name: formule; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule (
    id integer NOT NULL,
    name character varying(256),
    tag character varying(256),
    formula text,
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone,
    autore character varying(256) NOT NULL,
    commenti text
);


ALTER TABLE public.formule OWNER TO m024000;

--
-- Name: formule_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE formule_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.formule_id_seq OWNER TO m024000;

--
-- Name: formule_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE formule_id_seq OWNED BY formule.id;


--
-- Name: grafi; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE grafi (
    id integer NOT NULL,
    tag character varying(256) NOT NULL,
    commento text,
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone NOT NULL,
    autore character varying(256) NOT NULL
);


ALTER TABLE public.grafi OWNER TO m024000;

--
-- Name: grafi_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE grafi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.grafi_id_seq OWNER TO m024000;

--
-- Name: grafi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE grafi_id_seq OWNED BY grafi.id;


--
-- Name: metadati; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati (
    id integer NOT NULL,
    name text,
    key character varying(256),
    value text,
    tag character varying(256),
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone,
    autore character varying(256) NOT NULL
);


ALTER TABLE public.metadati OWNER TO m024000;

--
-- Name: metadati_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf10 (
    CONSTRAINT metadati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (metadati);
ALTER TABLE ONLY metadati_cf10 ALTER COLUMN name SET NOT NULL;
ALTER TABLE ONLY metadati_cf10 ALTER COLUMN key SET NOT NULL;
ALTER TABLE ONLY metadati_cf10 ALTER COLUMN value SET NOT NULL;


ALTER TABLE public.metadati_cf10 OWNER TO m024000;

--
-- Name: metadati_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE metadati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.metadati_id_seq OWNER TO m024000;

--
-- Name: metadati_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE metadati_id_seq OWNED BY metadati.id;


--
-- Name: metadati_test; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_test (
    CONSTRAINT metadati_test_tag_check CHECK (((tag)::text = 'test'::text))
)
INHERITS (metadati);
ALTER TABLE ONLY metadati_test ALTER COLUMN name SET NOT NULL;
ALTER TABLE ONLY metadati_test ALTER COLUMN key SET NOT NULL;
ALTER TABLE ONLY metadati_test ALTER COLUMN value SET NOT NULL;


ALTER TABLE public.metadati_test OWNER TO m024000;

--
-- Name: metadati_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_test1 (
    CONSTRAINT metadati_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (metadati);
ALTER TABLE ONLY metadati_test1 ALTER COLUMN name SET NOT NULL;
ALTER TABLE ONLY metadati_test1 ALTER COLUMN key SET NOT NULL;
ALTER TABLE ONLY metadati_test1 ALTER COLUMN value SET NOT NULL;


ALTER TABLE public.metadati_test1 OWNER TO m024000;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_test1 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_test1 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY conflitti ALTER COLUMN id SET DEFAULT nextval('"Conflitti_id_seq"'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY grafi ALTER COLUMN id SET DEFAULT nextval('grafi_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf10 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf10 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_test ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_test ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_test1 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_test1 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: Conflitti_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_name_key" UNIQUE (name);


--
-- Name: Conflitti_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_pkey" PRIMARY KEY (id);


--
-- Name: Conflitti_tag_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_tag_key" UNIQUE (tag);


--
-- Name: archi_test1_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY archi_test1
    ADD CONSTRAINT archi_test1_pkey PRIMARY KEY (partenza, arrivo);


--
-- Name: dati_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY dati
    ADD CONSTRAINT dati_name_key UNIQUE (name);


--
-- Name: dati_test1_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY dati_test1
    ADD CONSTRAINT dati_test1_pkey PRIMARY KEY (name);


--
-- Name: grafi_tag_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY grafi
    ADD CONSTRAINT grafi_tag_key UNIQUE (tag);


--
-- Name: metadati_cf10_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY metadati_cf10
    ADD CONSTRAINT metadati_cf10_pkey PRIMARY KEY (name, key, value);


--
-- Name: metadati_test1_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY metadati_test1
    ADD CONSTRAINT metadati_test1_pkey PRIMARY KEY (name, key, value);


--
-- Name: metadati_test_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY metadati_test
    ADD CONSTRAINT metadati_test_pkey PRIMARY KEY (name, key, value);


--
-- Name: archi_unique; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX archi_unique ON archi USING btree (partenza, arrivo);


--
-- Name: meta_key; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_key ON metadati USING btree (key);


--
-- Name: meta_unique; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX meta_unique ON metadati USING btree (name, key, value);


--
-- Name: meta_value; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_value ON metadati USING btree (value);


--
-- Name: formule_insert_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER formule_insert_trigger BEFORE INSERT ON formule FOR EACH ROW EXECUTE PROCEDURE formule_insert();


--
-- Name: insert_archi_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_archi_trigger BEFORE INSERT ON archi FOR EACH ROW EXECUTE PROCEDURE archi_insert();


--
-- Name: insert_dati_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_dati_trigger BEFORE INSERT ON dati FOR EACH ROW EXECUTE PROCEDURE dati_insert();


--
-- Name: insert_metadati_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_metadati_trigger BEFORE INSERT ON metadati FOR EACH ROW EXECUTE PROCEDURE metadati_insert();


--
-- Name: public; Type: ACL; Schema: -; Owner: m024000
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM m024000;
GRANT ALL ON SCHEMA public TO m024000;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

