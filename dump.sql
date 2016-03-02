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
    AS $_$DECLARE R RECORD;
BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'archi_' || NEW.tag) then

       EXECUTE 'CREATE TABLE archi_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (archi)';       
       -- EXECUTE 'ALTER TABLE archi_' || NEW.tag || ' ADD PRIMARY KEY (partenza, arrivo)';

       FOR r IN SELECT username FROM auth_user a, auth_membership b, auth_group c where
                        a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
          EXECUTE 'GRANT ALL ON TABLE archi_' || NEW.tag || ' TO ' || r.username;
       END LOOP;

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
    AS $_$DECLARE r RECORD;
BEGIN

    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'dati_' || NEW.tag) then

       EXECUTE 'CREATE TABLE dati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (dati)';       
       -- EXECUTE 'ALTER TABLE dati_' || NEW.tag || ' ADD PRIMARY KEY (name)';
       FOR r IN SELECT username FROM auth_user a, auth_membership b, auth_group c where
                        a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
          EXECUTE 'GRANT ALL ON TABLE dati_' || NEW.tag || ' TO ' || r.username;
       END LOOP;
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
    AS $_$DECLARE r RECORD;
BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'formule_' || NEW.tag) then

       EXECUTE 'CREATE TABLE  formule_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (formule)';       
       -- EXECUTE 'ALTER TABLE formule_' || NEW.tag || ' ADD PRIMARY KEY (name, tag)';
       FOR r IN SELECT username FROM auth_user a, auth_membership b, auth_group c where
                        a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
          EXECUTE 'GRANT ALL ON TABLE formule_' || NEW.tag || ' TO ' || r.username;
       END LOOP;
    end if;


    EXECUTE 'INSERT INTO formule_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.formule_insert() OWNER TO m024000;

--
-- Name: history_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION history_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$DECLARE r RECORD;
BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'history_' || NEW.tag) then

       EXECUTE 'CREATE TABLE  history_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (history)';       
       -- EXECUTE 'ALTER TABLE history_' || NEW.tag || ' ADD PRIMARY KEY (name, tag, ordinale)';
       FOR r IN SELECT username FROM auth_user a, auth_membership b, auth_group c where
                        a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
          EXECUTE 'GRANT ALL ON TABLE history_' || NEW.tag || ' TO ' || r.username;
       END LOOP;
    end if;


    EXECUTE 'INSERT INTO history_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.history_insert() OWNER TO m024000;

--
-- Name: metadati_insert(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION metadati_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$DECLARE r RECORD;
BEGIN
    if not exists(SELECT 1 
       FROM   pg_catalog.pg_class c
       JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE  n.nspname = 'public'
       AND    c.relname = 'metadati_' || NEW.tag) then

       EXECUTE 'CREATE TABLE metadati_' || NEW.tag || '(CHECK (tag = '''|| NEW.tag|| ''')) INHERITS (metadati)';       
       -- EXECUTE 'ALTER TABLE metadati_' || NEW.tag || ' ADD PRIMARY KEY (name, key, value)';

       FOR r IN SELECT username FROM auth_user a, auth_membership b, auth_group c where
                        a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
          EXECUTE 'GRANT ALL ON TABLE metadati_' || NEW.tag || ' TO ' || r.username;
       END LOOP;
    end if;

    EXECUTE 'INSERT INTO metadati_' || NEW.tag
        || ' SELECT ($1).*'
    USING NEW;

    RETURN NULL;
END
$_$;


ALTER FUNCTION public.metadati_insert() OWNER TO m024000;

--
-- Name: setgrant(); Type: FUNCTION; Schema: public; Owner: m024000
--

CREATE FUNCTION setgrant() RETURNS void
    LANGUAGE plpgsql
    AS $$DECLARE r RECORD;
BEGIN
   FOR r IN SELECT distinct a.username, z.table_name from information_schema.tables z, 
                      auth_user a, auth_membership b, auth_group c where 
                     z.table_schema='public' and z.table_catalog = 'grafo' and
                     a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP

     EXECUTE 'GRANT ALL ON TABLE ' || r.table_name || ' TO ' || r.username;
   END LOOP;
   FOR r in select distinct a.username, z.relname from pg_class z, auth_user a, auth_membership b, auth_group c where 
                     z.relkind = 'S' and  a.id = b.user_id and b.group_id = c.id and c.role = 'cfin' LOOP
      EXECUTE 'GRANT ALL ON SEQUENCE ' || r.relname || ' to ' || r.username;
   END LOOP;


END;
$$;


ALTER FUNCTION public.setgrant() OWNER TO m024000;

--
-- Name: active_locks; Type: VIEW; Schema: public; Owner: m024000
--

CREATE VIEW active_locks AS
    SELECT t.schemaname, t.relname, l.locktype, l.page, l.virtualtransaction, l.pid, l.mode, l.granted FROM (pg_locks l JOIN pg_stat_all_tables t ON ((l.relation = t.relid))) WHERE ((t.schemaname <> 'pg_toast'::name) AND (t.schemaname <> 'pg_catalog'::name)) ORDER BY t.schemaname, t.relname;


ALTER TABLE public.active_locks OWNER TO m024000;

SET default_tablespace = '';

SET default_with_oids = false;

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
-- Name: archi_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf10 (CONSTRAINT archi_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf10 OWNER TO m024000;

--
-- Name: archi_cf1304; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1304 (CONSTRAINT archi_cf1304_tag_check CHECK (((tag)::text = 'cf1304'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1304 OWNER TO m024000;

--
-- Name: archi_cf1401; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1401 (CONSTRAINT archi_cf1401_tag_check CHECK (((tag)::text = 'cf1401'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1401 OWNER TO m024000;

--
-- Name: archi_cf1402; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1402 (CONSTRAINT archi_cf1402_tag_check CHECK (((tag)::text = 'cf1402'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1402 OWNER TO m024000;

--
-- Name: archi_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1403 (CONSTRAINT archi_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1403 OWNER TO m024000;

--
-- Name: archi_cf1404; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1404 (CONSTRAINT archi_cf1404_tag_check CHECK (((tag)::text = 'cf1404'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1404 OWNER TO m024000;

--
-- Name: archi_cf1501; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1501 (CONSTRAINT archi_cf1501_tag_check CHECK (((tag)::text = 'cf1501'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1501 OWNER TO m024000;

--
-- Name: archi_cf1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1502 (CONSTRAINT archi_cf1502_tag_check CHECK (((tag)::text = 'cf1502'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1502 OWNER TO m024000;

--
-- Name: archi_cf1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_cf1503 (CONSTRAINT archi_cf1503_tag_check CHECK (((tag)::text = 'cf1503'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_cf1503 OWNER TO m024000;

--
-- Name: archi_i1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_i1502 (CONSTRAINT archi_i1502_tag_check CHECK (((tag)::text = 'i1502'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_i1502 OWNER TO m024000;

--
-- Name: archi_i1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_i1503 (CONSTRAINT archi_i1503_tag_check CHECK (((tag)::text = 'i1503'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_i1503 OWNER TO m024000;

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
-- Name: archi_naseca1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_naseca1503 (CONSTRAINT archi_naseca1503_tag_check CHECK (((tag)::text = 'naseca1503'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_naseca1503 OWNER TO m024000;

--
-- Name: archi_nasecg1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_nasecg1502 (CONSTRAINT archi_nasecg1502_tag_check CHECK (((tag)::text = 'nasecg1502'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_nasecg1502 OWNER TO m024000;

--
-- Name: archi_nasecg1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_nasecg1503 (CONSTRAINT archi_nasecg1503_tag_check CHECK (((tag)::text = 'nasecg1503'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_nasecg1503 OWNER TO m024000;

--
-- Name: archi_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE archi_test1 (CONSTRAINT archi_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (archi);


ALTER TABLE public.archi_test1 OWNER TO m024000;

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
    username character varying(128),
    password character varying(512),
    registration_key character varying(512),
    reset_password_key character varying(512),
    registration_id character varying(512),
    api_key character varying(512)
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
-- Name: capitalizzazioni; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE capitalizzazioni (
    isin character(12) NOT NULL,
    name character varying(255) NOT NULL,
    mv real,
    descrizione character varying(255) NOT NULL,
    data_contabile integer NOT NULL,
    data timestamp without time zone NOT NULL,
    autore character varying(255) NOT NULL
);


ALTER TABLE public.capitalizzazioni OWNER TO m024000;

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
-- Name: conflitti_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE conflitti_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.conflitti_id_seq OWNER TO m024000;

--
-- Name: conflitti_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE conflitti_id_seq OWNED BY conflitti.id;


--
-- Name: cr_request; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE cr_request (
    id integer NOT NULL,
    db2_username character varying(512),
    db2_password character varying(512),
    output_path character varying(512),
    servizio character varying(512),
    db character varying(512),
    nome_tabella character varying(512),
    nodo integer,
    cr_attributes text,
    soggetti_primari character(1),
    soggetti_legati character(1),
    legami character(1),
    soggetti_collegati character(1),
    soggetti_primari_e_collegati character(1),
    status character varying(512),
    belongs_to integer
);


ALTER TABLE public.cr_request OWNER TO m024000;

--
-- Name: cr_request_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE cr_request_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.cr_request_id_seq OWNER TO m024000;

--
-- Name: cr_request_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE cr_request_id_seq OWNED BY cr_request.id;


--
-- Name: dati; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati (
    id integer NOT NULL,
    name character varying(256) NOT NULL,
    anno smallint,
    periodo smallint,
    freq smallint,
    dati text,
    tag character varying(256) NOT NULL,
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone NOT NULL,
    autore character varying(256) NOT NULL,
    stato smallint DEFAULT 0 NOT NULL,
    notes text,
    stock smallint DEFAULT 0
);


ALTER TABLE public.dati OWNER TO m024000;

--
-- Name: dati_biss; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_biss (CONSTRAINT dati_biss_tag_check CHECK (((tag)::text = 'biss'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_biss OWNER TO m024000;

--
-- Name: dati_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf10 (CONSTRAINT dati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf10 OWNER TO m024000;

--
-- Name: dati_cf1304; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1304 (CONSTRAINT dati_cf1304_tag_check CHECK (((tag)::text = 'cf1304'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1304 OWNER TO m024000;

--
-- Name: dati_cf1401; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1401 (CONSTRAINT dati_cf1401_tag_check CHECK (((tag)::text = 'cf1401'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1401 OWNER TO m024000;

--
-- Name: dati_cf1402; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1402 (CONSTRAINT dati_cf1402_tag_check CHECK (((tag)::text = 'cf1402'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1402 OWNER TO m024000;

--
-- Name: dati_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1403 (CONSTRAINT dati_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1403 OWNER TO m024000;

--
-- Name: dati_cf1404; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1404 (CONSTRAINT dati_cf1404_tag_check CHECK (((tag)::text = 'cf1404'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1404 OWNER TO m024000;

--
-- Name: dati_cf1501; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1501 (CONSTRAINT dati_cf1501_tag_check CHECK (((tag)::text = 'cf1501'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1501 OWNER TO m024000;

--
-- Name: dati_cf1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1502 (CONSTRAINT dati_cf1502_tag_check CHECK (((tag)::text = 'cf1502'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1502 OWNER TO m024000;

--
-- Name: dati_cf1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_cf1503 (CONSTRAINT dati_cf1503_tag_check CHECK (((tag)::text = 'cf1503'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_cf1503 OWNER TO m024000;

--
-- Name: dati_i1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_i1502 (CONSTRAINT dati_i1502_tag_check CHECK (((tag)::text = 'i1502'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_i1502 OWNER TO m024000;

--
-- Name: dati_i1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_i1503 (CONSTRAINT dati_i1503_tag_check CHECK (((tag)::text = 'i1503'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_i1503 OWNER TO m024000;

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
-- Name: dati_naseca1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_naseca1503 (CONSTRAINT dati_naseca1503_tag_check CHECK (((tag)::text = 'naseca1503'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_naseca1503 OWNER TO m024000;

--
-- Name: dati_nasecg1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_nasecg1502 (CONSTRAINT dati_nasecg1502_tag_check CHECK (((tag)::text = 'nasecg1502'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_nasecg1502 OWNER TO m024000;

--
-- Name: dati_nasecg1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_nasecg1503 (CONSTRAINT dati_nasecg1503_tag_check CHECK (((tag)::text = 'nasecg1503'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_nasecg1503 OWNER TO m024000;

--
-- Name: dati_pne; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_pne (CONSTRAINT dati_pne_tag_check CHECK (((tag)::text = 'pne'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_pne OWNER TO m024000;

--
-- Name: dati_preload; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_preload (CONSTRAINT dati_preload_tag_check CHECK (((tag)::text = 'preload'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_preload OWNER TO m024000;

--
-- Name: dati_rm; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_rm (CONSTRAINT dati_rm_tag_check CHECK (((tag)::text = 'rm'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_rm OWNER TO m024000;

--
-- Name: dati_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE dati_test1 (CONSTRAINT dati_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (dati);


ALTER TABLE public.dati_test1 OWNER TO m024000;

--
-- Name: elaborazioni; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE elaborazioni (
    id integer NOT NULL,
    name character varying(256) NOT NULL,
    inizio timestamp with time zone DEFAULT (('now'::text)::timestamp without time zone)::timestamp(0) without time zone NOT NULL,
    fine timestamp with time zone,
    tag character varying(256) NOT NULL,
    status character varying(256) DEFAULT 'RUNNING'::character varying,
    notes character varying(4096)
);


ALTER TABLE public.elaborazioni OWNER TO m024000;

--
-- Name: elaborazioni_dati; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE elaborazioni_dati (
    dati_id integer,
    elaborazioni_id integer
);


ALTER TABLE public.elaborazioni_dati OWNER TO m024000;

--
-- Name: elaborazioni_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE elaborazioni_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.elaborazioni_id_seq OWNER TO m024000;

--
-- Name: elaborazioni_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE elaborazioni_id_seq OWNED BY elaborazioni.id;


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
-- Name: formule_biss; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_biss (CONSTRAINT formule_biss_tag_check CHECK (((tag)::text = 'biss'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_biss OWNER TO m024000;

--
-- Name: formule_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf10 (CONSTRAINT formule_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf10 OWNER TO m024000;

--
-- Name: formule_cf1304; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1304 (CONSTRAINT formule_cf1304_tag_check CHECK (((tag)::text = 'cf1304'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1304 OWNER TO m024000;

--
-- Name: formule_cf1401; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1401 (CONSTRAINT formule_cf1401_tag_check CHECK (((tag)::text = 'cf1401'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1401 OWNER TO m024000;

--
-- Name: formule_cf1402; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1402 (CONSTRAINT formule_cf1402_tag_check CHECK (((tag)::text = 'cf1402'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1402 OWNER TO m024000;

--
-- Name: formule_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1403 (CONSTRAINT formule_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1403 OWNER TO m024000;

--
-- Name: formule_cf1404; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1404 (CONSTRAINT formule_cf1404_tag_check CHECK (((tag)::text = 'cf1404'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1404 OWNER TO m024000;

--
-- Name: formule_cf1501; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1501 (CONSTRAINT formule_cf1501_tag_check CHECK (((tag)::text = 'cf1501'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1501 OWNER TO m024000;

--
-- Name: formule_cf1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1502 (CONSTRAINT formule_cf1502_tag_check CHECK (((tag)::text = 'cf1502'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1502 OWNER TO m024000;

--
-- Name: formule_cf1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_cf1503 (CONSTRAINT formule_cf1503_tag_check CHECK (((tag)::text = 'cf1503'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_cf1503 OWNER TO m024000;

--
-- Name: formule_i1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_i1502 (CONSTRAINT formule_i1502_tag_check CHECK (((tag)::text = 'i1502'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_i1502 OWNER TO m024000;

--
-- Name: formule_i1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_i1503 (CONSTRAINT formule_i1503_tag_check CHECK (((tag)::text = 'i1503'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_i1503 OWNER TO m024000;

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
-- Name: formule_naseca1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_naseca1503 (CONSTRAINT formule_naseca1503_tag_check CHECK (((tag)::text = 'naseca1503'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_naseca1503 OWNER TO m024000;

--
-- Name: formule_nasecg1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_nasecg1502 (CONSTRAINT formule_nasecg1502_tag_check CHECK (((tag)::text = 'nasecg1502'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_nasecg1502 OWNER TO m024000;

--
-- Name: formule_nasecg1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_nasecg1503 (CONSTRAINT formule_nasecg1503_tag_check CHECK (((tag)::text = 'nasecg1503'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_nasecg1503 OWNER TO m024000;

--
-- Name: formule_pne; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_pne (CONSTRAINT formule_pne_tag_check CHECK (((tag)::text = 'pne'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_pne OWNER TO m024000;

--
-- Name: formule_rm; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_rm (CONSTRAINT formule_rm_tag_check CHECK (((tag)::text = 'rm'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_rm OWNER TO m024000;

--
-- Name: formule_test1; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE formule_test1 (CONSTRAINT formule_test1_tag_check CHECK (((tag)::text = 'test1'::text))
)
INHERITS (formule);


ALTER TABLE public.formule_test1 OWNER TO m024000;

--
-- Name: grafi; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE grafi (
    id integer NOT NULL,
    tag character varying(256) NOT NULL,
    commento text,
    last_updated timestamp with time zone DEFAULT ('now'::text)::timestamp without time zone NOT NULL,
    autore character varying(256) NOT NULL,
    data bytea
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
-- Name: history; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE history (
    name character varying(255) NOT NULL,
    tag character varying(255) NOT NULL,
    ordinale smallint NOT NULL,
    anno smallint NOT NULL,
    periodo smallint NOT NULL,
    freq smallint NOT NULL,
    dati text NOT NULL,
    formula text,
    archi_entranti text,
    last_updated timestamp with time zone DEFAULT (('now'::text)::timestamp without time zone)::timestamp(0) without time zone NOT NULL,
    autore character varying(255) NOT NULL
);


ALTER TABLE public.history OWNER TO m024000;

--
-- Name: history_biss; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE history_biss (CONSTRAINT history_biss_tag_check CHECK (((tag)::text = 'biss'::text))
)
INHERITS (history);


ALTER TABLE public.history_biss OWNER TO m024000;

--
-- Name: history_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE history_cf10 (CONSTRAINT history_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (history);


ALTER TABLE public.history_cf10 OWNER TO m024000;

--
-- Name: history_pne; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE history_pne (CONSTRAINT history_pne_tag_check CHECK (((tag)::text = 'pne'::text))
)
INHERITS (history);


ALTER TABLE public.history_pne OWNER TO m024000;

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

CREATE TABLE metadati_cf10 (CONSTRAINT metadati_cf10_tag_check CHECK (((tag)::text = 'cf10'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf10 OWNER TO m024000;

--
-- Name: metadati_cf1304; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1304 (CONSTRAINT metadati_cf1304_tag_check CHECK (((tag)::text = 'cf1304'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1304 OWNER TO m024000;

--
-- Name: metadati_cf1401; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1401 (CONSTRAINT metadati_cf1401_tag_check CHECK (((tag)::text = 'cf1401'::text))
)
INHERITS (metadati);



ALTER TABLE public.metadati_cf1401 OWNER TO m024000;

--
-- Name: metadati_cf1402; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1402 (CONSTRAINT metadati_cf1402_tag_check CHECK (((tag)::text = 'cf1402'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1402 OWNER TO m024000;

--
-- Name: metadati_cf1403; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1403 (CONSTRAINT metadati_cf1403_tag_check CHECK (((tag)::text = 'cf1403'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1403 OWNER TO m024000;

--
-- Name: metadati_cf1404; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1404 (CONSTRAINT metadati_cf1404_tag_check CHECK (((tag)::text = 'cf1404'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1404 OWNER TO m024000;

--
-- Name: metadati_cf1501; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1501 (CONSTRAINT metadati_cf1501_tag_check CHECK (((tag)::text = 'cf1501'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1501 OWNER TO m024000;

--
-- Name: metadati_cf1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1502 (CONSTRAINT metadati_cf1502_tag_check CHECK (((tag)::text = 'cf1502'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1502 OWNER TO m024000;

--
-- Name: metadati_cf1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_cf1503 (CONSTRAINT metadati_cf1503_tag_check CHECK (((tag)::text = 'cf1503'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_cf1503 OWNER TO m024000;

--
-- Name: metadati_i1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_i1502 (CONSTRAINT metadati_i1502_tag_check CHECK (((tag)::text = 'i1502'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_i1502 OWNER TO m024000;

--
-- Name: metadati_i1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_i1503 (CONSTRAINT metadati_i1503_tag_check CHECK (((tag)::text = 'i1503'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_i1503 OWNER TO m024000;

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
-- Name: metadati_naseca1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_naseca1503 (CONSTRAINT metadati_naseca1503_tag_check CHECK (((tag)::text = 'naseca1503'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_naseca1503 OWNER TO m024000;

--
-- Name: metadati_nasecg1502; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_nasecg1502 (CONSTRAINT metadati_nasecg1502_tag_check CHECK (((tag)::text = 'nasecg1502'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_nasecg1502 OWNER TO m024000;

--
-- Name: metadati_nasecg1503; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE metadati_nasecg1503 (CONSTRAINT metadati_nasecg1503_tag_check CHECK (((tag)::text = 'nasecg1503'::text))
)
INHERITS (metadati);


ALTER TABLE public.metadati_nasecg1503 OWNER TO m024000;

--
-- Name: producers; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE producers (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    starting timestamp without time zone,
    ending timestamp without time zone,
    command text NOT NULL,
    type character varying(8) NOT NULL,
    category character varying(255),
    freq smallint
);


ALTER TABLE public.producers OWNER TO m024000;

--
-- Name: producers_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE producers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.producers_id_seq OWNER TO m024000;

--
-- Name: producers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE producers_id_seq OWNED BY producers.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf10 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf10 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1304 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1304 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1401 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1401 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1402 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1402 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1403 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1403 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1404 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1404 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1501 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1501 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1502 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1503 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_cf1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_i1502 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_i1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_i1503 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_i1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_naseca1503 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_naseca1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_nasecg1502 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_nasecg1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_nasecg1503 ALTER COLUMN id SET DEFAULT nextval('archi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY archi_nasecg1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


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

ALTER TABLE ONLY auth_cas ALTER COLUMN id SET DEFAULT nextval('auth_cas_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_event ALTER COLUMN id SET DEFAULT nextval('auth_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_group ALTER COLUMN id SET DEFAULT nextval('auth_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_membership ALTER COLUMN id SET DEFAULT nextval('auth_membership_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_permission ALTER COLUMN id SET DEFAULT nextval('auth_permission_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY auth_user ALTER COLUMN id SET DEFAULT nextval('auth_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY conflitti ALTER COLUMN id SET DEFAULT nextval('conflitti_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY cr_request ALTER COLUMN id SET DEFAULT nextval('cr_request_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_biss ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_biss ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_biss ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_biss ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf10 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf10 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf10 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf10 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1304 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1304 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1304 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1304 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1401 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1401 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1401 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1401 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1402 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1402 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1402 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1402 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1403 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1403 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1403 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1403 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1404 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1404 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1404 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1404 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1501 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1501 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1501 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1501 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1502 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1502 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1502 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1503 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1503 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_cf1503 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1502 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1502 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1502 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1503 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1503 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_i1503 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_naseca1503 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_naseca1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_naseca1503 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_naseca1503 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1502 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1502 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1502 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1503 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1503 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_nasecg1503 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_pne ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_pne ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_pne ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_pne ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_preload ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_preload ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_preload ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_preload ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_rm ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_rm ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_rm ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_rm ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN id SET DEFAULT nextval('dati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: stato; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN stato SET DEFAULT 0;


--
-- Name: stock; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY dati_test1 ALTER COLUMN stock SET DEFAULT 0;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY elaborazioni ALTER COLUMN id SET DEFAULT nextval('elaborazioni_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_biss ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_biss ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf10 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf10 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1304 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1304 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1401 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1401 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1402 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1402 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1403 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1403 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1404 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1404 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1501 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1501 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1502 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1503 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_cf1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_i1502 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_i1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_i1503 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_i1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_naseca1503 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_naseca1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_nasecg1502 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_nasecg1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_nasecg1503 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_nasecg1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_pne ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_pne ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_rm ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_rm ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_test1 ALTER COLUMN id SET DEFAULT nextval('formule_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY formule_test1 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY grafi ALTER COLUMN id SET DEFAULT nextval('grafi_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY history_biss ALTER COLUMN last_updated SET DEFAULT (('now'::text)::timestamp without time zone)::timestamp(0) without time zone;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY history_cf10 ALTER COLUMN last_updated SET DEFAULT (('now'::text)::timestamp without time zone)::timestamp(0) without time zone;


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY history_pne ALTER COLUMN last_updated SET DEFAULT (('now'::text)::timestamp without time zone)::timestamp(0) without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf10 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf10 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1304 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1304 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1401 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1401 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1402 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1402 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1403 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1403 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1404 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1404 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1501 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1501 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1502 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1503 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_cf1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_i1502 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_i1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_i1503 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_i1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_naseca1503 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_naseca1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_nasecg1502 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_nasecg1502 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_nasecg1503 ALTER COLUMN id SET DEFAULT nextval('metadati_id_seq'::regclass);


--
-- Name: last_updated; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY metadati_nasecg1503 ALTER COLUMN last_updated SET DEFAULT ('now'::text)::timestamp without time zone;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY producers ALTER COLUMN id SET DEFAULT nextval('producers_id_seq'::regclass);


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
-- Name: cap_pk; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY capitalizzazioni
    ADD CONSTRAINT cap_pk PRIMARY KEY (isin, data_contabile);


--
-- Name: cr_request_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY cr_request
    ADD CONSTRAINT cr_request_pkey PRIMARY KEY (id);


--
-- Name: dati_name_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY dati
    ADD CONSTRAINT dati_name_key UNIQUE (name);


--
-- Name: elaborazioni_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY elaborazioni
    ADD CONSTRAINT elaborazioni_pkey PRIMARY KEY (id);


--
-- Name: grafi_tag_key; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY grafi
    ADD CONSTRAINT grafi_tag_key UNIQUE (tag);


--
-- Name: history_pkey; Type: CONSTRAINT; Schema: public; Owner: m024000; Tablespace: 
--

ALTER TABLE ONLY history
    ADD CONSTRAINT history_pkey PRIMARY KEY (name, tag, ordinale);


--
-- Name: archi_unique; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX archi_unique ON archi USING btree (partenza, arrivo);


--
-- Name: dati_name_key_cf10; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX dati_name_key_cf10 ON dati_cf10 USING btree (name);


--
-- Name: dati_tag_name_index; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX dati_tag_name_index ON dati USING btree (tag, name);


--
-- Name: dati_tag_name_index_cf10; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX dati_tag_name_index_cf10 ON dati_cf10 USING btree (tag, name);


--
-- Name: elaborazioni_name_index; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX elaborazioni_name_index ON elaborazioni USING btree (name);


--
-- Name: meta_key; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_key ON metadati USING btree (key);


--
-- Name: meta_key_cf10; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_key_cf10 ON metadati_cf10 USING btree (key);


--
-- Name: meta_unique; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX meta_unique ON metadati USING btree (name, key, value);


--
-- Name: meta_unique_cf10; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE UNIQUE INDEX meta_unique_cf10 ON metadati_cf10 USING btree (name, key, value);


--
-- Name: meta_value; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_value ON metadati USING btree (value);


--
-- Name: meta_value_cf10; Type: INDEX; Schema: public; Owner: m024000; Tablespace: 
--

CREATE INDEX meta_value_cf10 ON metadati_cf10 USING btree (value);


--
-- Name: formule_insert_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER formule_insert_trigger
    BEFORE INSERT ON formule
    FOR EACH ROW
    EXECUTE PROCEDURE formule_insert();


--
-- Name: history_insert_trigger; Type: TRIGGER; Schema: public; Owner: m024000
--

CREATE TRIGGER history_insert_trigger
    BEFORE INSERT ON history
    FOR EACH ROW
    EXECUTE PROCEDURE history_insert();


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
-- Name: cr_request_belongs_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY cr_request
    ADD CONSTRAINT cr_request_belongs_to_fkey FOREIGN KEY (belongs_to) REFERENCES auth_user(id) ON DELETE CASCADE;


--
-- Name: serie_elaborazioni_elaborazioni_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: m024000
--

ALTER TABLE ONLY elaborazioni_dati
    ADD CONSTRAINT serie_elaborazioni_elaborazioni_id_fkey FOREIGN KEY (elaborazioni_id) REFERENCES elaborazioni(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: archi; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi FROM PUBLIC;
REVOKE ALL ON TABLE archi FROM m024000;
GRANT ALL ON TABLE archi TO m024000;
GRANT ALL ON TABLE archi TO e728162;
GRANT ALL ON TABLE archi TO a747430;
GRANT ALL ON TABLE archi TO d902950;
GRANT ALL ON TABLE archi TO h686120;
GRANT ALL ON TABLE archi TO m024533;


--
-- Name: archi_cf10; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf10 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf10 FROM m024000;
GRANT ALL ON TABLE archi_cf10 TO m024000;
GRANT ALL ON TABLE archi_cf10 TO a747430;
GRANT ALL ON TABLE archi_cf10 TO e728162;
GRANT ALL ON TABLE archi_cf10 TO d902950;
GRANT ALL ON TABLE archi_cf10 TO m024533;
GRANT ALL ON TABLE archi_cf10 TO h686120;


--
-- Name: archi_cf1304; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1304 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1304 FROM m024000;
GRANT ALL ON TABLE archi_cf1304 TO m024000;
GRANT ALL ON TABLE archi_cf1304 TO a747430;
GRANT ALL ON TABLE archi_cf1304 TO e728162;
GRANT ALL ON TABLE archi_cf1304 TO d902950;
GRANT ALL ON TABLE archi_cf1304 TO m024533;
GRANT ALL ON TABLE archi_cf1304 TO h686120;


--
-- Name: archi_cf1401; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1401 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1401 FROM m024000;
GRANT ALL ON TABLE archi_cf1401 TO m024000;
GRANT ALL ON TABLE archi_cf1401 TO a747430;
GRANT ALL ON TABLE archi_cf1401 TO e728162;
GRANT ALL ON TABLE archi_cf1401 TO d902950;
GRANT ALL ON TABLE archi_cf1401 TO m024533;
GRANT ALL ON TABLE archi_cf1401 TO h686120;


--
-- Name: archi_cf1402; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1402 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1402 FROM m024000;
GRANT ALL ON TABLE archi_cf1402 TO m024000;
GRANT ALL ON TABLE archi_cf1402 TO a747430;
GRANT ALL ON TABLE archi_cf1402 TO e728162;
GRANT ALL ON TABLE archi_cf1402 TO d902950;
GRANT ALL ON TABLE archi_cf1402 TO m024533;
GRANT ALL ON TABLE archi_cf1402 TO h686120;


--
-- Name: archi_cf1403; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1403 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1403 FROM m024000;
GRANT ALL ON TABLE archi_cf1403 TO m024000;
GRANT ALL ON TABLE archi_cf1403 TO a747430;
GRANT ALL ON TABLE archi_cf1403 TO e728162;
GRANT ALL ON TABLE archi_cf1403 TO d902950;
GRANT ALL ON TABLE archi_cf1403 TO m024533;
GRANT ALL ON TABLE archi_cf1403 TO h686120;


--
-- Name: archi_cf1404; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1404 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1404 FROM m024000;
GRANT ALL ON TABLE archi_cf1404 TO m024000;
GRANT ALL ON TABLE archi_cf1404 TO a747430;
GRANT ALL ON TABLE archi_cf1404 TO e728162;
GRANT ALL ON TABLE archi_cf1404 TO d902950;
GRANT ALL ON TABLE archi_cf1404 TO m024533;
GRANT ALL ON TABLE archi_cf1404 TO h686120;


--
-- Name: archi_cf1501; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1501 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1501 FROM m024000;
GRANT ALL ON TABLE archi_cf1501 TO m024000;
GRANT ALL ON TABLE archi_cf1501 TO a747430;
GRANT ALL ON TABLE archi_cf1501 TO e728162;
GRANT ALL ON TABLE archi_cf1501 TO d902950;
GRANT ALL ON TABLE archi_cf1501 TO m024533;
GRANT ALL ON TABLE archi_cf1501 TO h686120;


--
-- Name: archi_cf1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1502 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1502 FROM m024000;
GRANT ALL ON TABLE archi_cf1502 TO m024000;
GRANT ALL ON TABLE archi_cf1502 TO a747430;
GRANT ALL ON TABLE archi_cf1502 TO e728162;
GRANT ALL ON TABLE archi_cf1502 TO d902950;
GRANT ALL ON TABLE archi_cf1502 TO m024533;
GRANT ALL ON TABLE archi_cf1502 TO h686120;


--
-- Name: archi_cf1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_cf1503 FROM PUBLIC;
REVOKE ALL ON TABLE archi_cf1503 FROM m024000;
GRANT ALL ON TABLE archi_cf1503 TO m024000;
GRANT ALL ON TABLE archi_cf1503 TO a747430;
GRANT ALL ON TABLE archi_cf1503 TO d902950;
GRANT ALL ON TABLE archi_cf1503 TO m024533;
GRANT ALL ON TABLE archi_cf1503 TO e728162;


--
-- Name: archi_i1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_i1502 FROM PUBLIC;
REVOKE ALL ON TABLE archi_i1502 FROM m024000;
GRANT ALL ON TABLE archi_i1502 TO m024000;
GRANT ALL ON TABLE archi_i1502 TO a747430;
GRANT ALL ON TABLE archi_i1502 TO d902950;
GRANT ALL ON TABLE archi_i1502 TO m024533;
GRANT ALL ON TABLE archi_i1502 TO e728162;


--
-- Name: archi_i1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_i1503 FROM PUBLIC;
REVOKE ALL ON TABLE archi_i1503 FROM m024000;
GRANT ALL ON TABLE archi_i1503 TO m024000;
GRANT ALL ON TABLE archi_i1503 TO a747430;
GRANT ALL ON TABLE archi_i1503 TO d902950;
GRANT ALL ON TABLE archi_i1503 TO m024533;
GRANT ALL ON TABLE archi_i1503 TO e728162;


--
-- Name: archi_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE archi_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE archi_id_seq FROM m024000;
GRANT ALL ON SEQUENCE archi_id_seq TO m024000;
GRANT ALL ON SEQUENCE archi_id_seq TO e728162;
GRANT ALL ON SEQUENCE archi_id_seq TO h686120;
GRANT ALL ON SEQUENCE archi_id_seq TO a747430;
GRANT ALL ON SEQUENCE archi_id_seq TO m024533;
GRANT ALL ON SEQUENCE archi_id_seq TO d902950;


--
-- Name: archi_naseca1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_naseca1503 FROM PUBLIC;
REVOKE ALL ON TABLE archi_naseca1503 FROM m024000;
GRANT ALL ON TABLE archi_naseca1503 TO m024000;
GRANT ALL ON TABLE archi_naseca1503 TO a747430;
GRANT ALL ON TABLE archi_naseca1503 TO d902950;
GRANT ALL ON TABLE archi_naseca1503 TO m024533;
GRANT ALL ON TABLE archi_naseca1503 TO e728162;


--
-- Name: archi_nasecg1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_nasecg1502 FROM PUBLIC;
REVOKE ALL ON TABLE archi_nasecg1502 FROM m024000;
GRANT ALL ON TABLE archi_nasecg1502 TO m024000;
GRANT ALL ON TABLE archi_nasecg1502 TO a747430;
GRANT ALL ON TABLE archi_nasecg1502 TO d902950;
GRANT ALL ON TABLE archi_nasecg1502 TO m024533;
GRANT ALL ON TABLE archi_nasecg1502 TO e728162;


--
-- Name: archi_nasecg1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_nasecg1503 FROM PUBLIC;
REVOKE ALL ON TABLE archi_nasecg1503 FROM m024000;
GRANT ALL ON TABLE archi_nasecg1503 TO m024000;
GRANT ALL ON TABLE archi_nasecg1503 TO a747430;
GRANT ALL ON TABLE archi_nasecg1503 TO d902950;
GRANT ALL ON TABLE archi_nasecg1503 TO m024533;
GRANT ALL ON TABLE archi_nasecg1503 TO e728162;


--
-- Name: archi_test1; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE archi_test1 FROM PUBLIC;
REVOKE ALL ON TABLE archi_test1 FROM m024000;
GRANT ALL ON TABLE archi_test1 TO m024000;
GRANT ALL ON TABLE archi_test1 TO a747430;
GRANT ALL ON TABLE archi_test1 TO d902950;
GRANT ALL ON TABLE archi_test1 TO m024533;


--
-- Name: auth_cas; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_cas FROM PUBLIC;
REVOKE ALL ON TABLE auth_cas FROM m024000;
GRANT ALL ON TABLE auth_cas TO m024000;
GRANT ALL ON TABLE auth_cas TO a747430;
GRANT ALL ON TABLE auth_cas TO m024533;
GRANT ALL ON TABLE auth_cas TO d902950;
GRANT ALL ON TABLE auth_cas TO e728162;


--
-- Name: auth_cas_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_cas_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_cas_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_cas_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_cas_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_cas_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_cas_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_cas_id_seq TO e728162;


--
-- Name: auth_event; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_event FROM PUBLIC;
REVOKE ALL ON TABLE auth_event FROM m024000;
GRANT ALL ON TABLE auth_event TO m024000;
GRANT ALL ON TABLE auth_event TO a747430;
GRANT ALL ON TABLE auth_event TO d902950;
GRANT ALL ON TABLE auth_event TO m024533;
GRANT ALL ON TABLE auth_event TO e728162;


--
-- Name: auth_event_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_event_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_event_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_event_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_event_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_event_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_event_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_event_id_seq TO e728162;


--
-- Name: auth_group; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_group FROM PUBLIC;
REVOKE ALL ON TABLE auth_group FROM m024000;
GRANT ALL ON TABLE auth_group TO m024000;
GRANT ALL ON TABLE auth_group TO a747430;
GRANT ALL ON TABLE auth_group TO m024533;
GRANT ALL ON TABLE auth_group TO d902950;
GRANT ALL ON TABLE auth_group TO e728162;


--
-- Name: auth_group_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_group_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_group_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_group_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_group_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_group_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_group_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_group_id_seq TO e728162;


--
-- Name: auth_membership; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_membership FROM PUBLIC;
REVOKE ALL ON TABLE auth_membership FROM m024000;
GRANT ALL ON TABLE auth_membership TO m024000;
GRANT ALL ON TABLE auth_membership TO d902950;
GRANT ALL ON TABLE auth_membership TO m024533;
GRANT ALL ON TABLE auth_membership TO a747430;
GRANT ALL ON TABLE auth_membership TO e728162;


--
-- Name: auth_membership_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_membership_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_membership_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_membership_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_membership_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_membership_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_membership_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_membership_id_seq TO e728162;


--
-- Name: auth_permission; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_permission FROM PUBLIC;
REVOKE ALL ON TABLE auth_permission FROM m024000;
GRANT ALL ON TABLE auth_permission TO m024000;
GRANT ALL ON TABLE auth_permission TO m024533;
GRANT ALL ON TABLE auth_permission TO d902950;
GRANT ALL ON TABLE auth_permission TO a747430;
GRANT ALL ON TABLE auth_permission TO e728162;


--
-- Name: auth_permission_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_permission_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_permission_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_permission_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_permission_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_permission_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_permission_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_permission_id_seq TO e728162;


--
-- Name: auth_user; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE auth_user FROM PUBLIC;
REVOKE ALL ON TABLE auth_user FROM m024000;
GRANT ALL ON TABLE auth_user TO m024000;
GRANT ALL ON TABLE auth_user TO a747430;
GRANT ALL ON TABLE auth_user TO d902950;
GRANT ALL ON TABLE auth_user TO m024533;
GRANT ALL ON TABLE auth_user TO e728162;


--
-- Name: auth_user_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE auth_user_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auth_user_id_seq FROM m024000;
GRANT ALL ON SEQUENCE auth_user_id_seq TO m024000;
GRANT ALL ON SEQUENCE auth_user_id_seq TO m024533;
GRANT ALL ON SEQUENCE auth_user_id_seq TO a747430;
GRANT ALL ON SEQUENCE auth_user_id_seq TO d902950;
GRANT ALL ON SEQUENCE auth_user_id_seq TO e728162;


--
-- Name: capitalizzazioni; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE capitalizzazioni FROM PUBLIC;
REVOKE ALL ON TABLE capitalizzazioni FROM m024000;
GRANT ALL ON TABLE capitalizzazioni TO m024000;
GRANT ALL ON TABLE capitalizzazioni TO d902950;
GRANT ALL ON TABLE capitalizzazioni TO a747430;
GRANT ALL ON TABLE capitalizzazioni TO m024533;
GRANT ALL ON TABLE capitalizzazioni TO e728162;


--
-- Name: conflitti; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE conflitti FROM PUBLIC;
REVOKE ALL ON TABLE conflitti FROM m024000;
GRANT ALL ON TABLE conflitti TO m024000;
GRANT ALL ON TABLE conflitti TO e728162;
GRANT ALL ON TABLE conflitti TO a747430;
GRANT ALL ON TABLE conflitti TO d902950;
GRANT ALL ON TABLE conflitti TO h686120;
GRANT ALL ON TABLE conflitti TO m024533;


--
-- Name: conflitti_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE conflitti_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE conflitti_id_seq FROM m024000;
GRANT ALL ON SEQUENCE conflitti_id_seq TO m024000;
GRANT ALL ON SEQUENCE conflitti_id_seq TO a747430;
GRANT ALL ON SEQUENCE conflitti_id_seq TO d902950;
GRANT ALL ON SEQUENCE conflitti_id_seq TO h686120;
GRANT ALL ON SEQUENCE conflitti_id_seq TO e728162;
GRANT ALL ON SEQUENCE conflitti_id_seq TO m024533;


--
-- Name: cr_request; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE cr_request FROM PUBLIC;
REVOKE ALL ON TABLE cr_request FROM m024000;
GRANT ALL ON TABLE cr_request TO m024000;
GRANT ALL ON TABLE cr_request TO d902950;
GRANT ALL ON TABLE cr_request TO m024533;
GRANT ALL ON TABLE cr_request TO a747430;
GRANT ALL ON TABLE cr_request TO e728162;


--
-- Name: cr_request_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE cr_request_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE cr_request_id_seq FROM m024000;
GRANT ALL ON SEQUENCE cr_request_id_seq TO m024000;
GRANT ALL ON SEQUENCE cr_request_id_seq TO d902950;
GRANT ALL ON SEQUENCE cr_request_id_seq TO a747430;
GRANT ALL ON SEQUENCE cr_request_id_seq TO m024533;
GRANT ALL ON SEQUENCE cr_request_id_seq TO e728162;


--
-- Name: dati; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati FROM PUBLIC;
REVOKE ALL ON TABLE dati FROM m024000;
GRANT ALL ON TABLE dati TO m024000;
GRANT ALL ON TABLE dati TO e728162;
GRANT ALL ON TABLE dati TO a747430;
GRANT ALL ON TABLE dati TO d902950;
GRANT ALL ON TABLE dati TO h686120;
GRANT ALL ON TABLE dati TO m024533;


--
-- Name: dati_biss; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_biss FROM PUBLIC;
REVOKE ALL ON TABLE dati_biss FROM m024000;
GRANT ALL ON TABLE dati_biss TO m024000;
GRANT ALL ON TABLE dati_biss TO a747430;
GRANT ALL ON TABLE dati_biss TO d902950;
GRANT ALL ON TABLE dati_biss TO m024533;
GRANT ALL ON TABLE dati_biss TO e728162;


--
-- Name: dati_cf10; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf10 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf10 FROM m024000;
GRANT ALL ON TABLE dati_cf10 TO m024000;
GRANT ALL ON TABLE dati_cf10 TO a747430;
GRANT ALL ON TABLE dati_cf10 TO e728162;
GRANT ALL ON TABLE dati_cf10 TO d902950;
GRANT ALL ON TABLE dati_cf10 TO m024533;
GRANT ALL ON TABLE dati_cf10 TO h686120;


--
-- Name: dati_cf1304; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1304 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1304 FROM m024000;
GRANT ALL ON TABLE dati_cf1304 TO m024000;
GRANT ALL ON TABLE dati_cf1304 TO a747430;
GRANT ALL ON TABLE dati_cf1304 TO e728162;
GRANT ALL ON TABLE dati_cf1304 TO d902950;
GRANT ALL ON TABLE dati_cf1304 TO m024533;
GRANT ALL ON TABLE dati_cf1304 TO h686120;


--
-- Name: dati_cf1401; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1401 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1401 FROM m024000;
GRANT ALL ON TABLE dati_cf1401 TO m024000;
GRANT ALL ON TABLE dati_cf1401 TO a747430;
GRANT ALL ON TABLE dati_cf1401 TO e728162;
GRANT ALL ON TABLE dati_cf1401 TO d902950;
GRANT ALL ON TABLE dati_cf1401 TO m024533;
GRANT ALL ON TABLE dati_cf1401 TO h686120;


--
-- Name: dati_cf1402; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1402 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1402 FROM m024000;
GRANT ALL ON TABLE dati_cf1402 TO m024000;
GRANT ALL ON TABLE dati_cf1402 TO a747430;
GRANT ALL ON TABLE dati_cf1402 TO e728162;
GRANT ALL ON TABLE dati_cf1402 TO d902950;
GRANT ALL ON TABLE dati_cf1402 TO m024533;
GRANT ALL ON TABLE dati_cf1402 TO h686120;


--
-- Name: dati_cf1403; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1403 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1403 FROM m024000;
GRANT ALL ON TABLE dati_cf1403 TO m024000;
GRANT ALL ON TABLE dati_cf1403 TO a747430;
GRANT ALL ON TABLE dati_cf1403 TO e728162;
GRANT ALL ON TABLE dati_cf1403 TO d902950;
GRANT ALL ON TABLE dati_cf1403 TO m024533;
GRANT ALL ON TABLE dati_cf1403 TO h686120;


--
-- Name: dati_cf1404; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1404 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1404 FROM m024000;
GRANT ALL ON TABLE dati_cf1404 TO m024000;
GRANT ALL ON TABLE dati_cf1404 TO a747430;
GRANT ALL ON TABLE dati_cf1404 TO e728162;
GRANT ALL ON TABLE dati_cf1404 TO d902950;
GRANT ALL ON TABLE dati_cf1404 TO m024533;
GRANT ALL ON TABLE dati_cf1404 TO h686120;


--
-- Name: dati_cf1501; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1501 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1501 FROM m024000;
GRANT ALL ON TABLE dati_cf1501 TO m024000;
GRANT ALL ON TABLE dati_cf1501 TO a747430;
GRANT ALL ON TABLE dati_cf1501 TO e728162;
GRANT ALL ON TABLE dati_cf1501 TO d902950;
GRANT ALL ON TABLE dati_cf1501 TO m024533;
GRANT ALL ON TABLE dati_cf1501 TO h686120;


--
-- Name: dati_cf1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1502 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1502 FROM m024000;
GRANT ALL ON TABLE dati_cf1502 TO m024000;
GRANT ALL ON TABLE dati_cf1502 TO a747430;
GRANT ALL ON TABLE dati_cf1502 TO e728162;
GRANT ALL ON TABLE dati_cf1502 TO d902950;
GRANT ALL ON TABLE dati_cf1502 TO m024533;
GRANT ALL ON TABLE dati_cf1502 TO h686120;


--
-- Name: dati_cf1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_cf1503 FROM PUBLIC;
REVOKE ALL ON TABLE dati_cf1503 FROM m024000;
GRANT ALL ON TABLE dati_cf1503 TO m024000;
GRANT ALL ON TABLE dati_cf1503 TO a747430;
GRANT ALL ON TABLE dati_cf1503 TO d902950;
GRANT ALL ON TABLE dati_cf1503 TO m024533;
GRANT ALL ON TABLE dati_cf1503 TO e728162;


--
-- Name: dati_i1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_i1502 FROM PUBLIC;
REVOKE ALL ON TABLE dati_i1502 FROM m024000;
GRANT ALL ON TABLE dati_i1502 TO m024000;
GRANT ALL ON TABLE dati_i1502 TO a747430;
GRANT ALL ON TABLE dati_i1502 TO d902950;
GRANT ALL ON TABLE dati_i1502 TO m024533;
GRANT ALL ON TABLE dati_i1502 TO e728162;


--
-- Name: dati_i1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_i1503 FROM PUBLIC;
REVOKE ALL ON TABLE dati_i1503 FROM m024000;
GRANT ALL ON TABLE dati_i1503 TO m024000;
GRANT ALL ON TABLE dati_i1503 TO a747430;
GRANT ALL ON TABLE dati_i1503 TO d902950;
GRANT ALL ON TABLE dati_i1503 TO m024533;
GRANT ALL ON TABLE dati_i1503 TO e728162;


--
-- Name: dati_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE dati_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE dati_id_seq FROM m024000;
GRANT ALL ON SEQUENCE dati_id_seq TO m024000;
GRANT ALL ON SEQUENCE dati_id_seq TO a747430;
GRANT ALL ON SEQUENCE dati_id_seq TO d902950;
GRANT ALL ON SEQUENCE dati_id_seq TO e728162;
GRANT ALL ON SEQUENCE dati_id_seq TO m024533;
GRANT ALL ON SEQUENCE dati_id_seq TO h686120;


--
-- Name: dati_naseca1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_naseca1503 FROM PUBLIC;
REVOKE ALL ON TABLE dati_naseca1503 FROM m024000;
GRANT ALL ON TABLE dati_naseca1503 TO m024000;
GRANT ALL ON TABLE dati_naseca1503 TO a747430;
GRANT ALL ON TABLE dati_naseca1503 TO d902950;
GRANT ALL ON TABLE dati_naseca1503 TO m024533;
GRANT ALL ON TABLE dati_naseca1503 TO e728162;


--
-- Name: dati_nasecg1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_nasecg1502 FROM PUBLIC;
REVOKE ALL ON TABLE dati_nasecg1502 FROM m024000;
GRANT ALL ON TABLE dati_nasecg1502 TO m024000;
GRANT ALL ON TABLE dati_nasecg1502 TO a747430;
GRANT ALL ON TABLE dati_nasecg1502 TO d902950;
GRANT ALL ON TABLE dati_nasecg1502 TO m024533;
GRANT ALL ON TABLE dati_nasecg1502 TO e728162;


--
-- Name: dati_nasecg1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_nasecg1503 FROM PUBLIC;
REVOKE ALL ON TABLE dati_nasecg1503 FROM m024000;
GRANT ALL ON TABLE dati_nasecg1503 TO m024000;
GRANT ALL ON TABLE dati_nasecg1503 TO a747430;
GRANT ALL ON TABLE dati_nasecg1503 TO d902950;
GRANT ALL ON TABLE dati_nasecg1503 TO m024533;
GRANT ALL ON TABLE dati_nasecg1503 TO e728162;


--
-- Name: dati_pne; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_pne FROM PUBLIC;
REVOKE ALL ON TABLE dati_pne FROM m024000;
GRANT ALL ON TABLE dati_pne TO m024000;
GRANT ALL ON TABLE dati_pne TO a747430;
GRANT ALL ON TABLE dati_pne TO d902950;
GRANT ALL ON TABLE dati_pne TO m024533;
GRANT ALL ON TABLE dati_pne TO e728162;


--
-- Name: dati_preload; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_preload FROM PUBLIC;
REVOKE ALL ON TABLE dati_preload FROM m024000;
GRANT ALL ON TABLE dati_preload TO m024000;
GRANT ALL ON TABLE dati_preload TO a747430;
GRANT ALL ON TABLE dati_preload TO d902950;
GRANT ALL ON TABLE dati_preload TO m024533;
GRANT ALL ON TABLE dati_preload TO e728162;


--
-- Name: dati_rm; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_rm FROM PUBLIC;
REVOKE ALL ON TABLE dati_rm FROM m024000;
GRANT ALL ON TABLE dati_rm TO m024000;
GRANT ALL ON TABLE dati_rm TO a747430;
GRANT ALL ON TABLE dati_rm TO d902950;
GRANT ALL ON TABLE dati_rm TO m024533;
GRANT ALL ON TABLE dati_rm TO e728162;


--
-- Name: dati_test1; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE dati_test1 FROM PUBLIC;
REVOKE ALL ON TABLE dati_test1 FROM m024000;
GRANT ALL ON TABLE dati_test1 TO m024000;
GRANT ALL ON TABLE dati_test1 TO a747430;
GRANT ALL ON TABLE dati_test1 TO d902950;
GRANT ALL ON TABLE dati_test1 TO m024533;


--
-- Name: elaborazioni; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE elaborazioni FROM PUBLIC;
REVOKE ALL ON TABLE elaborazioni FROM m024000;
GRANT ALL ON TABLE elaborazioni TO m024000;
GRANT ALL ON TABLE elaborazioni TO m024533;
GRANT ALL ON TABLE elaborazioni TO h686120;
GRANT ALL ON TABLE elaborazioni TO e728162;
GRANT ALL ON TABLE elaborazioni TO a747430;
GRANT ALL ON TABLE elaborazioni TO d902950;


--
-- Name: elaborazioni_dati; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE elaborazioni_dati FROM PUBLIC;
REVOKE ALL ON TABLE elaborazioni_dati FROM m024000;
GRANT ALL ON TABLE elaborazioni_dati TO m024000;
GRANT ALL ON TABLE elaborazioni_dati TO m024533;
GRANT ALL ON TABLE elaborazioni_dati TO d902950;
GRANT ALL ON TABLE elaborazioni_dati TO h686120;
GRANT ALL ON TABLE elaborazioni_dati TO a747430;
GRANT ALL ON TABLE elaborazioni_dati TO e728162;


--
-- Name: elaborazioni_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE elaborazioni_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE elaborazioni_id_seq FROM m024000;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO m024000;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO h686120;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO m024533;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO a747430;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO e728162;
GRANT ALL ON SEQUENCE elaborazioni_id_seq TO d902950;


--
-- Name: formule; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule FROM PUBLIC;
REVOKE ALL ON TABLE formule FROM m024000;
GRANT ALL ON TABLE formule TO m024000;
GRANT ALL ON TABLE formule TO a747430;
GRANT ALL ON TABLE formule TO e728162;
GRANT ALL ON TABLE formule TO d902950;
GRANT ALL ON TABLE formule TO h686120;
GRANT ALL ON TABLE formule TO m024533;


--
-- Name: formule_biss; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_biss FROM PUBLIC;
REVOKE ALL ON TABLE formule_biss FROM m024000;
GRANT ALL ON TABLE formule_biss TO m024000;
GRANT ALL ON TABLE formule_biss TO a747430;
GRANT ALL ON TABLE formule_biss TO d902950;
GRANT ALL ON TABLE formule_biss TO m024533;
GRANT ALL ON TABLE formule_biss TO e728162;


--
-- Name: formule_cf10; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf10 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf10 FROM m024000;
GRANT ALL ON TABLE formule_cf10 TO m024000;
GRANT ALL ON TABLE formule_cf10 TO a747430;
GRANT ALL ON TABLE formule_cf10 TO e728162;
GRANT ALL ON TABLE formule_cf10 TO d902950;
GRANT ALL ON TABLE formule_cf10 TO m024533;
GRANT ALL ON TABLE formule_cf10 TO h686120;


--
-- Name: formule_cf1304; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1304 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1304 FROM m024000;
GRANT ALL ON TABLE formule_cf1304 TO m024000;
GRANT ALL ON TABLE formule_cf1304 TO a747430;
GRANT ALL ON TABLE formule_cf1304 TO e728162;
GRANT ALL ON TABLE formule_cf1304 TO d902950;
GRANT ALL ON TABLE formule_cf1304 TO m024533;
GRANT ALL ON TABLE formule_cf1304 TO h686120;


--
-- Name: formule_cf1401; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1401 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1401 FROM m024000;
GRANT ALL ON TABLE formule_cf1401 TO m024000;
GRANT ALL ON TABLE formule_cf1401 TO a747430;
GRANT ALL ON TABLE formule_cf1401 TO e728162;
GRANT ALL ON TABLE formule_cf1401 TO d902950;
GRANT ALL ON TABLE formule_cf1401 TO m024533;
GRANT ALL ON TABLE formule_cf1401 TO h686120;


--
-- Name: formule_cf1402; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1402 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1402 FROM m024000;
GRANT ALL ON TABLE formule_cf1402 TO m024000;
GRANT ALL ON TABLE formule_cf1402 TO a747430;
GRANT ALL ON TABLE formule_cf1402 TO e728162;
GRANT ALL ON TABLE formule_cf1402 TO d902950;
GRANT ALL ON TABLE formule_cf1402 TO m024533;
GRANT ALL ON TABLE formule_cf1402 TO h686120;


--
-- Name: formule_cf1403; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1403 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1403 FROM m024000;
GRANT ALL ON TABLE formule_cf1403 TO m024000;
GRANT ALL ON TABLE formule_cf1403 TO a747430;
GRANT ALL ON TABLE formule_cf1403 TO e728162;
GRANT ALL ON TABLE formule_cf1403 TO d902950;
GRANT ALL ON TABLE formule_cf1403 TO m024533;
GRANT ALL ON TABLE formule_cf1403 TO h686120;


--
-- Name: formule_cf1404; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1404 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1404 FROM m024000;
GRANT ALL ON TABLE formule_cf1404 TO m024000;
GRANT ALL ON TABLE formule_cf1404 TO a747430;
GRANT ALL ON TABLE formule_cf1404 TO e728162;
GRANT ALL ON TABLE formule_cf1404 TO d902950;
GRANT ALL ON TABLE formule_cf1404 TO m024533;
GRANT ALL ON TABLE formule_cf1404 TO h686120;


--
-- Name: formule_cf1501; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1501 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1501 FROM m024000;
GRANT ALL ON TABLE formule_cf1501 TO m024000;
GRANT ALL ON TABLE formule_cf1501 TO a747430;
GRANT ALL ON TABLE formule_cf1501 TO e728162;
GRANT ALL ON TABLE formule_cf1501 TO d902950;
GRANT ALL ON TABLE formule_cf1501 TO m024533;
GRANT ALL ON TABLE formule_cf1501 TO h686120;


--
-- Name: formule_cf1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1502 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1502 FROM m024000;
GRANT ALL ON TABLE formule_cf1502 TO m024000;
GRANT ALL ON TABLE formule_cf1502 TO a747430;
GRANT ALL ON TABLE formule_cf1502 TO e728162;
GRANT ALL ON TABLE formule_cf1502 TO d902950;
GRANT ALL ON TABLE formule_cf1502 TO m024533;
GRANT ALL ON TABLE formule_cf1502 TO h686120;


--
-- Name: formule_cf1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_cf1503 FROM PUBLIC;
REVOKE ALL ON TABLE formule_cf1503 FROM m024000;
GRANT ALL ON TABLE formule_cf1503 TO m024000;
GRANT ALL ON TABLE formule_cf1503 TO a747430;
GRANT ALL ON TABLE formule_cf1503 TO d902950;
GRANT ALL ON TABLE formule_cf1503 TO m024533;
GRANT ALL ON TABLE formule_cf1503 TO e728162;


--
-- Name: formule_i1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_i1502 FROM PUBLIC;
REVOKE ALL ON TABLE formule_i1502 FROM m024000;
GRANT ALL ON TABLE formule_i1502 TO m024000;
GRANT ALL ON TABLE formule_i1502 TO a747430;
GRANT ALL ON TABLE formule_i1502 TO d902950;
GRANT ALL ON TABLE formule_i1502 TO m024533;
GRANT ALL ON TABLE formule_i1502 TO e728162;


--
-- Name: formule_i1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_i1503 FROM PUBLIC;
REVOKE ALL ON TABLE formule_i1503 FROM m024000;
GRANT ALL ON TABLE formule_i1503 TO m024000;
GRANT ALL ON TABLE formule_i1503 TO a747430;
GRANT ALL ON TABLE formule_i1503 TO d902950;
GRANT ALL ON TABLE formule_i1503 TO m024533;
GRANT ALL ON TABLE formule_i1503 TO e728162;


--
-- Name: formule_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE formule_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE formule_id_seq FROM m024000;
GRANT ALL ON SEQUENCE formule_id_seq TO m024000;
GRANT ALL ON SEQUENCE formule_id_seq TO m024533;
GRANT ALL ON SEQUENCE formule_id_seq TO h686120;
GRANT ALL ON SEQUENCE formule_id_seq TO d902950;
GRANT ALL ON SEQUENCE formule_id_seq TO e728162;
GRANT ALL ON SEQUENCE formule_id_seq TO a747430;


--
-- Name: formule_naseca1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_naseca1503 FROM PUBLIC;
REVOKE ALL ON TABLE formule_naseca1503 FROM m024000;
GRANT ALL ON TABLE formule_naseca1503 TO m024000;
GRANT ALL ON TABLE formule_naseca1503 TO a747430;
GRANT ALL ON TABLE formule_naseca1503 TO d902950;
GRANT ALL ON TABLE formule_naseca1503 TO m024533;
GRANT ALL ON TABLE formule_naseca1503 TO e728162;


--
-- Name: formule_nasecg1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_nasecg1502 FROM PUBLIC;
REVOKE ALL ON TABLE formule_nasecg1502 FROM m024000;
GRANT ALL ON TABLE formule_nasecg1502 TO m024000;
GRANT ALL ON TABLE formule_nasecg1502 TO a747430;
GRANT ALL ON TABLE formule_nasecg1502 TO d902950;
GRANT ALL ON TABLE formule_nasecg1502 TO m024533;
GRANT ALL ON TABLE formule_nasecg1502 TO e728162;


--
-- Name: formule_nasecg1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_nasecg1503 FROM PUBLIC;
REVOKE ALL ON TABLE formule_nasecg1503 FROM m024000;
GRANT ALL ON TABLE formule_nasecg1503 TO m024000;
GRANT ALL ON TABLE formule_nasecg1503 TO a747430;
GRANT ALL ON TABLE formule_nasecg1503 TO d902950;
GRANT ALL ON TABLE formule_nasecg1503 TO m024533;
GRANT ALL ON TABLE formule_nasecg1503 TO e728162;


--
-- Name: formule_pne; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_pne FROM PUBLIC;
REVOKE ALL ON TABLE formule_pne FROM m024000;
GRANT ALL ON TABLE formule_pne TO m024000;
GRANT ALL ON TABLE formule_pne TO a747430;
GRANT ALL ON TABLE formule_pne TO d902950;
GRANT ALL ON TABLE formule_pne TO m024533;
GRANT ALL ON TABLE formule_pne TO e728162;


--
-- Name: formule_rm; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_rm FROM PUBLIC;
REVOKE ALL ON TABLE formule_rm FROM m024000;
GRANT ALL ON TABLE formule_rm TO m024000;
GRANT ALL ON TABLE formule_rm TO a747430;
GRANT ALL ON TABLE formule_rm TO d902950;
GRANT ALL ON TABLE formule_rm TO m024533;
GRANT ALL ON TABLE formule_rm TO e728162;


--
-- Name: formule_test1; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE formule_test1 FROM PUBLIC;
REVOKE ALL ON TABLE formule_test1 FROM m024000;
GRANT ALL ON TABLE formule_test1 TO m024000;
GRANT ALL ON TABLE formule_test1 TO a747430;
GRANT ALL ON TABLE formule_test1 TO d902950;
GRANT ALL ON TABLE formule_test1 TO m024533;


--
-- Name: grafi; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE grafi FROM PUBLIC;
REVOKE ALL ON TABLE grafi FROM m024000;
GRANT ALL ON TABLE grafi TO m024000;
GRANT ALL ON TABLE grafi TO e728162;
GRANT ALL ON TABLE grafi TO a747430;
GRANT ALL ON TABLE grafi TO d902950;
GRANT ALL ON TABLE grafi TO h686120;
GRANT ALL ON TABLE grafi TO m024533;


--
-- Name: grafi_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE grafi_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE grafi_id_seq FROM m024000;
GRANT ALL ON SEQUENCE grafi_id_seq TO m024000;
GRANT ALL ON SEQUENCE grafi_id_seq TO m024533;
GRANT ALL ON SEQUENCE grafi_id_seq TO h686120;
GRANT ALL ON SEQUENCE grafi_id_seq TO d902950;
GRANT ALL ON SEQUENCE grafi_id_seq TO e728162;
GRANT ALL ON SEQUENCE grafi_id_seq TO a747430;


--
-- Name: history; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE history FROM PUBLIC;
REVOKE ALL ON TABLE history FROM m024000;
GRANT ALL ON TABLE history TO m024000;
GRANT ALL ON TABLE history TO a747430;
GRANT ALL ON TABLE history TO e728162;
GRANT ALL ON TABLE history TO d902950;
GRANT ALL ON TABLE history TO h686120;
GRANT ALL ON TABLE history TO m024533;


--
-- Name: history_biss; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE history_biss FROM PUBLIC;
REVOKE ALL ON TABLE history_biss FROM m024000;
GRANT ALL ON TABLE history_biss TO m024000;
GRANT ALL ON TABLE history_biss TO a747430;
GRANT ALL ON TABLE history_biss TO d902950;
GRANT ALL ON TABLE history_biss TO m024533;
GRANT ALL ON TABLE history_biss TO e728162;


--
-- Name: history_cf10; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE history_cf10 FROM PUBLIC;
REVOKE ALL ON TABLE history_cf10 FROM m024000;
GRANT ALL ON TABLE history_cf10 TO m024000;
GRANT ALL ON TABLE history_cf10 TO a747430;
GRANT ALL ON TABLE history_cf10 TO e728162;
GRANT ALL ON TABLE history_cf10 TO d902950;
GRANT ALL ON TABLE history_cf10 TO m024533;
GRANT ALL ON TABLE history_cf10 TO h686120;


--
-- Name: history_pne; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE history_pne FROM PUBLIC;
REVOKE ALL ON TABLE history_pne FROM m024000;
GRANT ALL ON TABLE history_pne TO m024000;
GRANT ALL ON TABLE history_pne TO a747430;
GRANT ALL ON TABLE history_pne TO d902950;
GRANT ALL ON TABLE history_pne TO m024533;
GRANT ALL ON TABLE history_pne TO e728162;


--
-- Name: metadati; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati FROM PUBLIC;
REVOKE ALL ON TABLE metadati FROM m024000;
GRANT ALL ON TABLE metadati TO m024000;
GRANT ALL ON TABLE metadati TO e728162;
GRANT ALL ON TABLE metadati TO a747430;
GRANT ALL ON TABLE metadati TO d902950;
GRANT ALL ON TABLE metadati TO h686120;
GRANT ALL ON TABLE metadati TO m024533;


--
-- Name: metadati_cf10; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf10 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf10 FROM m024000;
GRANT ALL ON TABLE metadati_cf10 TO m024000;
GRANT ALL ON TABLE metadati_cf10 TO a747430;
GRANT ALL ON TABLE metadati_cf10 TO e728162;
GRANT ALL ON TABLE metadati_cf10 TO d902950;
GRANT ALL ON TABLE metadati_cf10 TO m024533;
GRANT ALL ON TABLE metadati_cf10 TO h686120;


--
-- Name: metadati_cf1304; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1304 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1304 FROM m024000;
GRANT ALL ON TABLE metadati_cf1304 TO m024000;
GRANT ALL ON TABLE metadati_cf1304 TO a747430;
GRANT ALL ON TABLE metadati_cf1304 TO e728162;
GRANT ALL ON TABLE metadati_cf1304 TO d902950;
GRANT ALL ON TABLE metadati_cf1304 TO m024533;
GRANT ALL ON TABLE metadati_cf1304 TO h686120;


--
-- Name: metadati_cf1401; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1401 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1401 FROM m024000;
GRANT ALL ON TABLE metadati_cf1401 TO m024000;
GRANT ALL ON TABLE metadati_cf1401 TO a747430;
GRANT ALL ON TABLE metadati_cf1401 TO e728162;
GRANT ALL ON TABLE metadati_cf1401 TO d902950;
GRANT ALL ON TABLE metadati_cf1401 TO m024533;
GRANT ALL ON TABLE metadati_cf1401 TO h686120;


--
-- Name: metadati_cf1402; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1402 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1402 FROM m024000;
GRANT ALL ON TABLE metadati_cf1402 TO m024000;
GRANT ALL ON TABLE metadati_cf1402 TO a747430;
GRANT ALL ON TABLE metadati_cf1402 TO e728162;
GRANT ALL ON TABLE metadati_cf1402 TO d902950;
GRANT ALL ON TABLE metadati_cf1402 TO m024533;
GRANT ALL ON TABLE metadati_cf1402 TO h686120;


--
-- Name: metadati_cf1403; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1403 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1403 FROM m024000;
GRANT ALL ON TABLE metadati_cf1403 TO m024000;
GRANT ALL ON TABLE metadati_cf1403 TO a747430;
GRANT ALL ON TABLE metadati_cf1403 TO e728162;
GRANT ALL ON TABLE metadati_cf1403 TO d902950;
GRANT ALL ON TABLE metadati_cf1403 TO m024533;
GRANT ALL ON TABLE metadati_cf1403 TO h686120;


--
-- Name: metadati_cf1404; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1404 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1404 FROM m024000;
GRANT ALL ON TABLE metadati_cf1404 TO m024000;
GRANT ALL ON TABLE metadati_cf1404 TO a747430;
GRANT ALL ON TABLE metadati_cf1404 TO e728162;
GRANT ALL ON TABLE metadati_cf1404 TO d902950;
GRANT ALL ON TABLE metadati_cf1404 TO m024533;
GRANT ALL ON TABLE metadati_cf1404 TO h686120;


--
-- Name: metadati_cf1501; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1501 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1501 FROM m024000;
GRANT ALL ON TABLE metadati_cf1501 TO m024000;
GRANT ALL ON TABLE metadati_cf1501 TO a747430;
GRANT ALL ON TABLE metadati_cf1501 TO e728162;
GRANT ALL ON TABLE metadati_cf1501 TO d902950;
GRANT ALL ON TABLE metadati_cf1501 TO m024533;
GRANT ALL ON TABLE metadati_cf1501 TO h686120;


--
-- Name: metadati_cf1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1502 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1502 FROM m024000;
GRANT ALL ON TABLE metadati_cf1502 TO m024000;
GRANT ALL ON TABLE metadati_cf1502 TO a747430;
GRANT ALL ON TABLE metadati_cf1502 TO e728162;
GRANT ALL ON TABLE metadati_cf1502 TO d902950;
GRANT ALL ON TABLE metadati_cf1502 TO m024533;
GRANT ALL ON TABLE metadati_cf1502 TO h686120;


--
-- Name: metadati_cf1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_cf1503 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_cf1503 FROM m024000;
GRANT ALL ON TABLE metadati_cf1503 TO m024000;
GRANT ALL ON TABLE metadati_cf1503 TO a747430;
GRANT ALL ON TABLE metadati_cf1503 TO d902950;
GRANT ALL ON TABLE metadati_cf1503 TO m024533;
GRANT ALL ON TABLE metadati_cf1503 TO e728162;


--
-- Name: metadati_i1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_i1502 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_i1502 FROM m024000;
GRANT ALL ON TABLE metadati_i1502 TO m024000;
GRANT ALL ON TABLE metadati_i1502 TO a747430;
GRANT ALL ON TABLE metadati_i1502 TO d902950;
GRANT ALL ON TABLE metadati_i1502 TO m024533;
GRANT ALL ON TABLE metadati_i1502 TO e728162;


--
-- Name: metadati_i1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_i1503 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_i1503 FROM m024000;
GRANT ALL ON TABLE metadati_i1503 TO m024000;
GRANT ALL ON TABLE metadati_i1503 TO a747430;
GRANT ALL ON TABLE metadati_i1503 TO d902950;
GRANT ALL ON TABLE metadati_i1503 TO m024533;
GRANT ALL ON TABLE metadati_i1503 TO e728162;


--
-- Name: metadati_id_seq; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON SEQUENCE metadati_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE metadati_id_seq FROM m024000;
GRANT ALL ON SEQUENCE metadati_id_seq TO m024000;
GRANT ALL ON SEQUENCE metadati_id_seq TO h686120;
GRANT ALL ON SEQUENCE metadati_id_seq TO d902950;
GRANT ALL ON SEQUENCE metadati_id_seq TO e728162;
GRANT ALL ON SEQUENCE metadati_id_seq TO a747430;
GRANT ALL ON SEQUENCE metadati_id_seq TO m024533;


--
-- Name: metadati_naseca1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_naseca1503 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_naseca1503 FROM m024000;
GRANT ALL ON TABLE metadati_naseca1503 TO m024000;
GRANT ALL ON TABLE metadati_naseca1503 TO a747430;
GRANT ALL ON TABLE metadati_naseca1503 TO d902950;
GRANT ALL ON TABLE metadati_naseca1503 TO m024533;
GRANT ALL ON TABLE metadati_naseca1503 TO e728162;


--
-- Name: metadati_nasecg1502; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_nasecg1502 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_nasecg1502 FROM m024000;
GRANT ALL ON TABLE metadati_nasecg1502 TO m024000;
GRANT ALL ON TABLE metadati_nasecg1502 TO a747430;
GRANT ALL ON TABLE metadati_nasecg1502 TO d902950;
GRANT ALL ON TABLE metadati_nasecg1502 TO m024533;
GRANT ALL ON TABLE metadati_nasecg1502 TO e728162;


--
-- Name: metadati_nasecg1503; Type: ACL; Schema: public; Owner: m024000
--

REVOKE ALL ON TABLE metadati_nasecg1503 FROM PUBLIC;
REVOKE ALL ON TABLE metadati_nasecg1503 FROM m024000;
GRANT ALL ON TABLE metadati_nasecg1503 TO m024000;
GRANT ALL ON TABLE metadati_nasecg1503 TO a747430;
GRANT ALL ON TABLE metadati_nasecg1503 TO d902950;
GRANT ALL ON TABLE metadati_nasecg1503 TO m024533;
GRANT ALL ON TABLE metadati_nasecg1503 TO e728162;


--
-- PostgreSQL database dump complete
--

