DBTools
=======

SQL Server store procedure con diversas consultas útiles.

PARAMETROS:
-----------

@Opcion CHAR(4),				        --> Accion a ejecutar
@TMP NVARCHAR(MAX)=NULL,		    --> Usos multiples, su sintaxis varia acorde a la @Opcion que se elija.
@BaseDeDatos NVARCHAR(50)=NULL,	--> Indica la base de datos sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual. 
@Esquema CHAR(3)=NULL,	        --> Indica el esquema sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual.
@Tabla NVARCHAR(100)=NULL,	    --> Solo para la opcion [GI]
@N SMALLINT=10,	                --> Multiples usos, habitualmente indica el TOP maximo de resultados a mostrar dependiendo la opcion.
@Print BIT=0		                --> Indica si se requiere ver los querys dinamicos(1=Si | 0=No). 

OPCIONES Y SINTAXIS:
--------------------

--[H]	 ->	Mostrar texto de procedimientos almacenados, trigers y funciones. 
          Ej: DBTools 'H','DBTools'
          
--[R]	 ->	Buscar referencias de un texto en procedimientos almacenados, trigers y funciones. 
          Ej: DBTools 'R','rapcx1981'	 
          
--[T]  ->	Buscar tablas que contengan un campo con el texto proporcionado. 
          Ej: DBTools 'T','Orders'

--[ET] -> Muestra el esquema de la tabla (campos, tipo de datos, longitud). 
            Ej: DBTools 'ET','FactCallCenter'

--[GI] -> Generar inserts condicionados. En @TMP van las condiciones que corresponderian al WHERE. Se ignora el WHERE ya que se a incluido como fijo
			   'WHERE 1=1' en caso de que se quieran generar toda la tabla. Para ingresar las condiciones hay que tomar en cuenta lo siguiente:

			    En caso de ser un campo char, varchar o similares	-->	[' AND <campo>=''''<valor>'''' ']
			    En caso de ser un campo int, smallint o similares	-->	[' AND <campo>=<valor>			]
			    En caso de ser un campo bit (donde <valor> = 0 | 1)-->	[' AND <campo>=<valor>			]	
			    
			    Ej: DBTools 'GI',' ',NULL,NULL,'FactCallCenter'	
				      DBTools 'GI','AND FactCallCenterID>5',NULL,NULL,'FactCallCenter'		

--[PE] -> Muestra plan de ejecucion, toma como valor el campo <plan_handle> 
          Ej: DBTools 'PE','0x05000E0055685A0340C1BF82010000000000000000000000'

--[RD] -> Buscar Registros duplicados. El formato para ingresar es: '<campo1>,<campo2>,<campoN> FROM <tabla>' 
          Ej: DBTools 'RD','FinanceKey,Amount FROM FactFinance'

--[XML]-> Regresar valores de consulta con formato XML. Si se requiere datos en particular se ingresa la consulta si es toda la tabal solo el nombre de la tabla. 
			    Ej: DBTools 'XML','FactCallCenter'  
				      DBTools 'XML','SELECT * FROM FactCallCenter WHERE FactCallCenterID>5 ' 

--[UUP]-> Muestra el TOP @N de las tablas a las que se les ejecuto UPDATE. 
          Ej: DBTools 'UUP',NULL,NULL,NULL,NULL,25 
  				    DBTools 'UPP'

--[RXT]-> Registros, numero de columnas, fecha de creacion y fecha de ultima modificacion de las primeras @N tablas ordenadas descendentemente por numero maximo de registros.

--[UOM]-> Lista de los ultimos objetos modificados en los ultimos "X" (@N) dias.

--[BTT]-> Busca un texto dentro de todas las tablas de la base de datos. 
			    -Si se buscara el texto en todas las tablas:
					  Ej: DBTools 'BTT','<Texto a Buscar>'	Ej: DBTools 'BTT','A Bike Store'
						
					-Si se buscara el texto en un grupo especifico de tablas:
					  Ej: DBTools 'BTT','<Texto a Buscar> IN <Tabla 1>,<Tabla 2>, <Tabla N>' 
					  Ej: DBTools 'BTT','A Bike Store IN dimreseller,dimaccount,dimdate' 
					-Opcionalmente se puede especificar 1 despues del texto para indicar si se requiere mostrar el query generado en la pestaña de "Mensajes". Si no se 
					 indica nada automaticamente asume que no se requiere.	 

Las opciones siguientes sirven para especificar como va a ordenarse el reporte segun lo que se busque. 
	         Ej: DBTools 'ACPU' 
	         
	         -Si se requiere limitar el numero de registros, en el ejemplo, 50:
	    	   Ej: DBTools 'ACPU',NULL,NULL,NULL,NULL,50 

--[ACPU]-> Representa uso promedio de CPU (Average CPU Usage). 
--[TCPU]-> Representa uso total de CPU (Total CPU usage).
--[AE]	-> Representa tiempo promedio transcurrido (Average Elapsed Time).
--[TE]	-> Representa tiempo total transcurrido(Total Elapsed Time).
--[EC]	-> Representa numero de ejecuciones (Execution Count).
--[AIO]	-> Representa IO promedio(Average IOs).  
--[TIO]	-> Representa IO Total (Total IOs). 
--[ALR]	-> Representa lecturas logicas promedio (Average Logical Reads).
--[TLR]	-> Representa lecturas logicas totales (Total Logical Reads).              
--[ALW]	-> Representa escrituras promedio totales (Average Logical Writes).
--[TLW]	-> Representa total de escrituras logicas (Total Logical Writes).
--[APR]	-> Representa promedio de lectura fisicas (Average Physical Reads).
--[TPR]	-> Representa total de lecturas fisicas (Total Physical Read).
