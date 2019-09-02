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
    
    EXECUTE 'INSERT INTO archi_' || NEW.tag ||'(tag, partenza, arrivo, autore) '
    || ' SELECT ($1).tag, ($1).partenza, ($1).arrivo, ($1).autore'
    USING NEW;

    RETURN NULL;
    

END;
$_$;




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
    
    EXECUTE 'INSERT INTO dati_' || NEW.tag ||'(tag, name, anno, periodo, freq, dati, autore, stato, notes, stock) '
    || ' SELECT ($1).tag, ($1).name, ($1).anno, ($1).periodo, ($1).freq, ($1).dati, ($1).autore, ($1).stato, ($1).notes, ($1).stock'
    USING NEW;

    RETURN NULL;
    

END;
$_$;




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
