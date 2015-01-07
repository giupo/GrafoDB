--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'LATIN9';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: m024000
--

CREATE PROCEDURAL LANGUAGE plpgsql;


ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO m024000;

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

       EXECUTE 'CREATE TABLE archi_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (archi)';       
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

       EXECUTE 'CREATE TABLE dati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (dati)';       
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
    AS $_$BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'formule_' || NEW.tag) then

       EXECUTE 'CREATE TABLE  formule_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (formule)';       
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

       EXECUTE 'CREATE TABLE metadati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (metadati)';       
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
    NO MAXVALUE
    NO MINVALUE
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
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.archi_id_seq OWNER TO m024000;

--
-- Name: archi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE archi_id_seq OWNED BY archi.id;


--
-- Name: archi_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf10 (CONSTRAINT archi_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf10 OWNER TO m024000;

--
-- Name: archi_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1403 (CONSTRAINT archi_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1403 OWNER TO m024000;

--
-- Name: auth_cas; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_cas (
    id integer NOT NULL,
    user_id integer,
    created_on timestamp without time zone,
    service character varying(512),
    ticket character varying(512),
    renew character(1)
);


ALTER TABLE public.auth_cas OWNER TO m024000;

--
-- Name: auth_cas_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_cas_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_cas_id_seq OWNER TO m024000;

--
-- Name: auth_cas_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_cas_id_seq OWNED BY auth_cas.id;


--
-- Name: auth_event; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_event (
    id integer NOT NULL,
    time_stamp timestamp without time zone,
    client_ip character varying(512),
    user_id integer,
    origin character varying(512),
    description text
);


ALTER TABLE public.auth_event OWNER TO m024000;

--
-- Name: auth_event_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_event_id_seq OWNER TO m024000;

--
-- Name: auth_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_event_id_seq OWNED BY auth_event.id;


--
-- Name: auth_group; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_group (
    id integer NOT NULL,
    role character varying(512),
    description text
);


ALTER TABLE public.auth_group OWNER TO m024000;

--
-- Name: auth_group_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_group_id_seq OWNER TO m024000;

--
-- Name: auth_group_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_group_id_seq OWNED BY auth_group.id;


--
-- Name: auth_membership; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_membership (
    id integer NOT NULL,
    user_id integer,
    group_id integer
);


ALTER TABLE public.auth_membership OWNER TO m024000;

--
-- Name: auth_membership_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_membership_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_membership_id_seq OWNER TO m024000;

--
-- Name: auth_membership_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_membership_id_seq OWNED BY auth_membership.id;


--
-- Name: auth_permission; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_permission (
    id integer NOT NULL,
    group_id integer,
    name character varying(512),
    table_name character varying(512),
    record_id integer
);


ALTER TABLE public.auth_permission OWNER TO m024000;

--
-- Name: auth_permission_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_permission_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_permission_id_seq OWNER TO m024000;

--
-- Name: auth_permission_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_permission_id_seq OWNED BY auth_permission.id;


--
-- Name: auth_user; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE auth_user (
    id integer NOT NULL,
    first_name character varying(128),
    last_name character varying(128),
    email character varying(512),
    password character varying(512),
    registration_key character varying(512),
    reset_password_key character varying(512),
    registration_id character varying(512)
);


ALTER TABLE public.auth_user OWNER TO m024000;

--
-- Name: auth_user_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE auth_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.auth_user_id_seq OWNER TO m024000;

--
-- Name: auth_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE auth_user_id_seq OWNED BY auth_user.id;


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
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.dati_id_seq OWNER TO m024000;

--
-- Name: dati_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE dati_id_seq OWNED BY dati.id;


--
-- Name: dati_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf10 (CONSTRAINT dati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf10 OWNER TO m024000;

--
-- Name: dati_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1403 (CONSTRAINT dati_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1403 OWNER TO m024000;

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
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.formule_id_seq OWNER TO m024000;

--
-- Name: formule_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE formule_id_seq OWNED BY formule.id;


--
-- Name: formule_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf10 (CONSTRAINT formule_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf10 OWNER TO m024000;

--
-- Name: formule_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1403 (CONSTRAINT formule_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1403 OWNER TO m024000;

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
    NO MAXVALUE
    NO MINVALUE
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
-- Name: metadati_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE metadati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.metadati_id_seq OWNER TO m024000;

--
-- Name: metadati_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE metadati_id_seq OWNED BY metadati.id;


--
-- Name: metadati_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf10 (CONSTRAINT metadati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf10 OWNER TO m024000;

--
-- Name: metadati_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1403 (CONSTRAINT metadati_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1403 OWNER TO m024000;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE archi ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_cas ALTER COLUMN id SET DEFAULT nextval('auth_cas_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_event ALTER COLUMN id SET DEFAULT nextval('auth_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_group ALTER COLUMN id SET DEFAULT nextval('auth_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_membership ALTER COLUMN id SET DEFAULT nextval('auth_membership_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_permission ALTER COLUMN id SET DEFAULT nextval('auth_permission_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE auth_user ALTER COLUMN id SET DEFAULT nextval('auth_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE conflitti ALTER COLUMN id SET DEFAULT nextval('"Conflitti_id_seq"'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE dati ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE formule ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE grafi ALTER COLUMN id SET DEFAULT nextval('grafi_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE metadati ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: Conflitti_name_tag_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_name_tag_key" UNIQUE (name, tag);


--
-- Name: Conflitti_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY conflitti
    ADD CONSTRAINT "Conflitti_pkey" PRIMARY KEY (id);


--
-- Name: archi_cf10_partenza_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY archi_cf10
    ADD CONSTRAINT archi_cf10_partenza_key UNIQUE (partenza, arrivo);


--
-- Name: auth_cas_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_cas
    ADD CONSTRAINT auth_cas_pkey PRIMARY KEY (id);


--
-- Name: auth_event_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_event
    ADD CONSTRAINT auth_event_pkey PRIMARY KEY (id);


--
-- Name: auth_group_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_group
    ADD CONSTRAINT auth_group_pkey PRIMARY KEY (id);


--
-- Name: auth_membership_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_membership
    ADD CONSTRAINT auth_membership_pkey PRIMARY KEY (id);


--
-- Name: auth_permission_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_permission
    ADD CONSTRAINT auth_permission_pkey PRIMARY KEY (id);


--
-- Name: auth_user_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY auth_user
    ADD CONSTRAINT auth_user_pkey PRIMARY KEY (id);


--
-- Name: dati_cf10_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY dati_cf10
    ADD CONSTRAINT dati_cf10_name_key UNIQUE (name);


--
-- Name: dati_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY dati
    ADD CONSTRAINT dati_name_key UNIQUE (name);


--
-- Name: formule_cf10_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY formule_cf10
    ADD CONSTRAINT formule_cf10_name_key UNIQUE (name);


--
-- Name: grafi_tag_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY grafi
    ADD CONSTRAINT grafi_tag_key UNIQUE (tag);


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

CREATE TRIGGER formule_insert_trigger
    BEFORE INSERT ON formule
    FOR EACH ROW
    EXECUTE PROCEDURE formule_insert();


--
-- Name: insert_archi_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_archi_trigger
    BEFORE INSERT ON archi
    FOR EACH ROW
    EXECUTE PROCEDURE archi_insert();


--
-- Name: insert_dati_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_dati_trigger
    BEFORE INSERT ON dati
    FOR EACH ROW
    EXECUTE PROCEDURE dati_insert();


--
-- Name: insert_metadati_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER insert_metadati_trigger
    BEFORE INSERT ON metadati
    FOR EACH ROW
    EXECUTE PROCEDURE metadati_insert();


--
-- Name: auth_cas_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_cas
    ADD CONSTRAINT auth_cas_user_id_fkey FOREIGN KEY (user_id) REFERENCES auth_user(id) ON DELETE CASCADE;


--
-- Name: auth_event_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_event
    ADD CONSTRAINT auth_event_user_id_fkey FOREIGN KEY (user_id) REFERENCES auth_user(id) ON DELETE CASCADE;


--
-- Name: auth_membership_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_membership
    ADD CONSTRAINT auth_membership_group_id_fkey FOREIGN KEY (group_id) REFERENCES auth_group(id) ON DELETE CASCADE;


--
-- Name: auth_membership_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_membership
    ADD CONSTRAINT auth_membership_user_id_fkey FOREIGN KEY (user_id) REFERENCES auth_user(id) ON DELETE CASCADE;


--
-- Name: auth_permission_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_permission
    ADD CONSTRAINT auth_permission_group_id_fkey FOREIGN KEY (group_id) REFERENCES auth_group(id) ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

