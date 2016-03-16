--
-- Name: archi; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE IF NOT EXISTS archi (
    id integer NOT NULL,
    partenza character varying(256),
    arrivo character varying(256),
    tag character varying(256),
    last_updated datetime,
    autore character varying(256) NOT NULL
);

--
-- Name: archi_cf10; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE SEQUENCE archi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

--
-- Name: archi_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE archi_id_seq OWNED BY archi.id;


CREATE TABLE capitalizzazioni (
    isin character(12) NOT NULL,
    name character varying(255) NOT NULL,
    mv real,
    descrizione character varying(255) NOT NULL,
    data_contabile integer NOT NULL,
    data datetime NOT NULL,
    autore character varying(255) NOT NULL
);


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
    date datetime NOT NULL,
    formula text
);

--
-- Name: conflitti_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE conflitti_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

--
-- Name: conflitti_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE conflitti_id_seq OWNED BY conflitti.id;

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
    last_updated datetime  NOT NULL,
    autore character varying(256) NOT NULL,
    stato smallint DEFAULT 0 NOT NULL,
    notes text,
    stock smallint DEFAULT 0
);

--
-- Name: dati_biss; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--


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
-- Name: elaborazioni; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE elaborazioni (
    id integer NOT NULL,
    name character varying(256) NOT NULL,
    inizio datetime NOT NULL,
    fine datetime,
    tag character varying(256) NOT NULL,
    status character varying(256) DEFAULT 'RUNNING'::character varying,
    notes character varying(4096)
);


--
-- Name: elaborazioni_dati; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE elaborazioni_dati (
    dati_id integer,
    elaborazioni_id integer
);

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
    last_updated datetime,
    autore character varying(256) NOT NULL,
    commenti text
);


--
-- Name: formule_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE formule_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

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
    last_updated datetime NOT NULL,
    autore character varying(256) NOT NULL,
);

--
-- Name: grafi_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE grafi_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

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
    last_updated datetime  NOT NULL,
    autore character varying(255) NOT NULL
);

CREATE TABLE metadati (
    id integer NOT NULL,
    name text,
    key character varying(256),
    value text,
    tag character varying(256),
    last_updated datetime,
    autore character varying(256) NOT NULL
);



--
-- Name: metadati_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE metadati_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: metadati_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE metadati_id_seq OWNED BY metadati.id;



--
-- Name: producers; Type: TABLE; Schema: public; Owner: m024000; Tablespace: 
--

CREATE TABLE producers (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    starting datetime,
    ending datetime,
    command text NOT NULL,
    type character varying(8) NOT NULL,
    category character varying(255),
    freq smallint
);

--
-- Name: producers_id_seq; Type: SEQUENCE; Schema: public; Owner: m024000
--

CREATE SEQUENCE producers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

--
-- Name: producers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: m024000
--

ALTER SEQUENCE producers_id_seq OWNED BY producers.id;




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

ALTER TABLE ONLY elaborazioni ALTER COLUMN id SET DEFAULT nextval('elaborazioni_id_seq'::regclass);


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
