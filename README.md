DBTools
=======

SQL Server store procedure con diversas consultas útiles.

PARAMETROS:
-----------

@Opcion			- Accion a ejecutar.@TMP			- Usos multiples, su sintaxis varia acorde a la @Opcion que se elija.
@BaseDeDatos	- Indica la base de datos sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual.
@Esquema		- Indica el esquema sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual.
@Tabla			- Solo para la opcion [GI].
@N				- Multiples usos, habitualmente indica el TOP maximo de resultados a mostrar dependiendo la opcion.
@Print			- Indica si se requiere ver los querys dinamicos(1=Si | 0=No). 


OPCIONES Y SINTAXIS:
--------------------
