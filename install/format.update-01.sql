----------------------------------------------------------------------------------------------------
-- Formats update-1
----------------------------------------------------------------------------------------------------

ALTER TABLE formats ADD COLUMN lang varchar(32);

UPDATE formats SET lang = 'COMMON-LISP';