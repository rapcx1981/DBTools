CREATE PROC DBTools
(
@Opcion CHAR(4),				--> Accion a ejecutar
@TMP NVARCHAR(MAX)=NULL,		--> Usos multiples, su sintaxis varia acorde a la @Opcion que se elija.
@BaseDeDatos NVARCHAR(50)=NULL,	--> Indica la base de datos sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual. 
@Esquema CHAR(3)		 =NULL,	--> Indica el esquema sobre la que se ejecutara la consulta. Si no se indica ninguno toma el actual.
@Tabla NVARCHAR(100)	 =NULL,	--> Solo para la opcion [GI]
@N SMALLINT				 =10,	--> Multiples usos, habitualmente indica el TOP maximo de resultados a mostrar dependiendo la opcion.
@Print BIT				 =0		--> Indica si se requiere ver los querys dinamicos.
)
AS
/*Autor:		Rodrigo Anael Perez Castro | rapcx1981@gmail.com  
  Creado:		30/12/2013 | Ultima Actualizacion:03/01/2014
  ---------------------------------------------------------------------------------------------------------------------------------------------------
  @TMP			--> Toma los valores generales dependiendo la @Opcion, podria enviarse un texto general, un campo, algun nombre de
					sp, trigger o funcion.
  @Opcion		--> Establece la accion a ejecutar.			
  @BaseDeDatos	--> Establece la base de datos en la que se quiere consultar -en el caso de que se tengan varias. Por default tomara
					en la que estamos posicionados.
  @Esquema		--> Fija el esquema que queremos consultar, si no se establece toma el esquema en el que estemos posicionados. 
  @N			--> Se usa generalmente para establecer un TOP limite de resultado para las consultas.
  @Print		--> Indicamos si queremos que imprima la consulta o no (1=Si | 0=No). 					
*/
--OPCIONES-------------------------------------------------------------------------------------------------
--[H]	-->	Mostrar texto de procedimientos almacenados, trigers y funciones. Ej: DBTools 'H','DBTools'
--[R]	-->	Buscar referencias de un texto en procedimientos almacenados, trigers y funciones. Ej: DBTools 'R','rapcx1981'	 
--[T]	-->	Buscar tablas que contengan un campo con el texto proporcionado. Ej: DBTools 'T','Orders'
--[ET]	--> Muestra el esquema de la tabla (campos, tipo de datos, longitud). Ej: DBTools 'ET','FactCallCenter'
--[GI]	--> Generar inserts condicionados. En @TMP va las condiciones que corresponderian al WHERE. Se ignora el WHERE ya que se a incluido como fijo
--			'WHERE 1=1' en caso de que se quieran generar toda la tabla. Para ingresar las condiciones hay que tomar en cuenta lo siguiente:
--			En caso de ser un campo char, varchar o similares	-->	[' AND <campo>=''''<valor>'''' ']
--			En caso de ser un campo int, smallint o similares	-->	[' AND <campo>=<valor>			]
--			En caso de ser un campo bit (donde <valor> = 0 | 1)-->	[' AND <campo>=<valor>			]	
--			Ej: DBTools 'GI',' ',NULL,NULL,'FactCallCenter'	
--				DBTools 'GI','AND FactCallCenterID>5',NULL,NULL,'FactCallCenter'		
--[PE]	--> Muestra plan de ejecucion, toma como valor el campo <plan_handle> Ej: DBTools 'PE','0x05000E0055685A0340C1BF82010000000000000000000000'
--[RD]	--> Buscar Registros duplicados. El formato para ingresar es: '<campo1>,<campo2>,<campoN> FROM <tabla>' Ej: DBTools 'RD','FinanceKey,Amount FROM FactFinance'
--[XML]	--> Regresar valores de consulta con formato XML. Si se requiere datos en particular se ingresa la consulta si es toda la tabal solo el nombre de la tabla. 
--			Ej: DBTools 'XML','FactCallCenter'  
--				DBTools 'XML','SELECT * FROM FactCallCenter WHERE FactCallCenterID>5 ' 
--[UUP]	--> Muestra el TOP @N de las tablas a las que se les ejecuto UPDATE. 
--			Ej: DBTools 'UUP',NULL,NULL,NULL,NULL,25 
--				DBTools 'UPP'
--[RXT] --> Registros, numero de columnas, fecha de creacion y fecha de ultima modificacion de las primeras @N tablas ordenadas descendentemente por numero maximo de registros.
--[UOM]	--> Lista de los ultimos objetos modificados en los ultimos "X" (@N) dias.
--[BTT] --> Busca un texto dentro de todas las tablas de la base de datos. 
--			SINTAXIS:	-Si se buscara el texto en todas las tablas:
--						 DBTools 'BTT','<Texto a Buscar>'	Ej: DBTools 'BTT','A Bike Store'
--						-Si se buscara el texto en un grupo especifico de tablas:
--						 DBTools 'BTT','<Texto a Buscar> IN <Tabla 1>,<Tabla 2>, <Tabla N>' Ej:   DBTools 'BTT','A Bike Store IN dimreseller,dimaccount,dimdate' 
--						-Opcionalmente se puede especificar 1 despues del texto para indicar si se requiere mostrar el query generado en la pestaña de "Mensajes". Si no se 
--						indica nada automaticamente asume que no se requiere.	 

--LAS OPCIONES SIGUIENTES SIRVEN PARA ESPECIFICAR COMO VA A ORDENARSE EL REPORTE SEGUN LO QUE SE BUSQUE. EL REPORTE ESTA ORIENTADO A ENCONTRAR PROCESOS
--QUE SON CANDIDATOS A OPTIMAR.
--	Ej: DBTools 'ACPU' 
--		DBTools 'ACPU',NULL,NULL,NULL,NULL,50 
--[ACPU]--> Representa uso promedio de CPU (Average CPU Usage). 
--[TCPU]--> Representa uso total de CPU (Total CPU usage).
--[AE]	--> Representa tiempo promedio transcurrido (Average Elapsed Time).
--[TE]	--> Representa tiempo total transcurrido(Total Elapsed Time).
--[EC]	--> Representa numero de ejecuciones (Execution Count).
--[AIO]	--> Representa IO promedio(Average IOs).  
--[TIO]	--> Representa IO Total (Total IOs). 
--[ALR]	--> Representa lecturas logicas promedio (Average Logical Reads).
--[TLR]	--> Representa lecturas logicas totales (Total Logical Reads).              
--[ALW]	--> Representa escrituras promedio totales (Average Logical Writes).
--[TLW]	--> Representa total de escrituras logicas (Total Logical Writes).
--[APR]	--> Representa promedio de lectura fisicas (Average Physical Reads).
--[TPR]	--> Representa total de lecturas fisicas (Total Physical Read).
BEGIN
--VARIABLES DE ENTORNO-------------------------------------------------------------------------------------
DECLARE @Query VARCHAR(MAX)=''

SET NOCOUNT ON
IF (@BaseDeDatos IS NULL)
BEGIN
	SET @BaseDeDatos=RTRIM(DB_NAME())
END
IF (@Esquema IS NULL)
BEGIN
	SET @Esquema=RTRIM(SCHEMA_NAME())
END
IF (@N IS NULL OR @N=0)
BEGIN
	SET @N=10
END

IF (@BaseDeDatos<>DB_NAME())
BEGIN
	SET @Query='	USE '+@BaseDeDatos+CHAR(13)
END
--OPCIONES-------------------------------------------------------------------------------------------------
IF (@Opcion='H' AND LEN(@TMP)>0)
BEGIN
	--NECESITE CONSULTAR:http://blog.sqlauthority.com/2007/09/13/sql-server-difference-between-exec-and-execute-vs-exec-use-execexecute-for-sp-always/
	SET @Query=@Query+' EXEC sp_helptext ['+RTRIM(@Esquema)+'.'+RTRIM(@TMP)+'] '
END

IF (@Opcion='R' AND LEN(@TMP)>0)
BEGIN
	SET @Query=@Query+' 
	SELECT 
	ROUTINE_TYPE AS TIPO,
	ROUTINE_SCHEMA AS ESQUEMA, 
	ROUTINE_NAME AS [SP/TRIGGER/FUNCTION], 
	ROUTINE_DEFINITION AS [TEXTO],
	CREATED AS CREACION,
	LAST_ALTERED AS [ULTIMA MODIFICACION] 
	FROM INFORMATION_SCHEMA.ROUTINES(NOLOCK) 
    WHERE ROUTINE_DEFINITION LIKE ''%'+RTRIM(@TMP)+'%'''
END

IF (@Opcion='T' AND LEN(@TMP)>0)
BEGIN
	SET @Query=@Query+'
	SELECT 
	TABLE_CATALOG AS BASE_DE_DATOS,
	TABLE_SCHEMA AS ESQUEMA,  
	TABLE_NAME AS TABLA,
	DATA_TYPE AS TIPO_DE_DATOS,
	IS_NULLABLE AS [NULL]
	FROM INFORMATION_SCHEMA.COLUMNS(NOLOCK) 
	WHERE COLUMN_NAME='''+RTRIM(@TMP)+'''
	AND TABLE_CATALOG='''+RTRIM(@BaseDeDatos)+'''
	AND TABLE_SCHEMA='''+RTRIM(@Esquema)+''' 
	ORDER BY TABLE_NAME'
END

IF (@Opcion='PE' AND LEN(@TMP)>0)
BEGIN
	SET @Query=@Query+' SELECT query_plan AS [PLAN DE EJECUCION] FROM sys.dm_exec_query_plan('+RTRIM(CONVERT(VARCHAR(60),@TMP))+')'
END

IF (@Opcion='ET' AND LEN(@TMP)>0)
BEGIN
	SET @Query='
	SELECT
	TABLE_CATALOG AS BASE_DE_DATOS,
	TABLE_SCHEMA AS ESQUEMA,
	TABLE_NAME AS TABLA,
	COLUMN_NAME AS COLUMNA,
	ORDINAL_POSITION AS  POSICION,
	IS_NULLABLE AS ES_NULO,
	DATA_TYPE AS TIPO_DE_DATO,
	CHARACTER_MAXIMUM_LENGTH AS LONGITUD 
	FROM Information_schema.columns ISC(NOLOCK)
	WHERE TABLE_CATALOG='''+RTRIM(@BaseDeDatos)+'''  
	AND TABLE_SCHEMA='''+RTRIM(@Esquema)+'''                               
	AND TABLE_NAME='''+RTRIM(@TMP)+'''        
	AND COLUMN_NAME NOT IN (''SyncDestination'',''PendingSyncDestination'',''SkuID'',''SaleCreditedto'') '
END

IF (@Opcion='RD' AND LEN(@TMP)>0)
BEGIN
	SET @Query=@Query+'
	SELECT '+(SELECT SUBSTRING(@TMP,0,(CHARINDEX('FROM',@TMP)-1)))+', COUNT(1) AS Registros
	FROM '+(SELECT SUBSTRING(@TMP,(SELECT CHARINDEX('FROM',@TMP)+4),LEN(@TMP)))+' (NOLOCK)
	GROUP BY '+(SELECT SUBSTRING(@TMP,0,(CHARINDEX('FROM',@TMP)-1)))+'
	HAVING COUNT(1) > 1'
END

IF (@Opcion='XML' AND LEN(@TMP)>0)
BEGIN
	IF ((SELECT CHARINDEX('WHERE', @TMP))>0)
	BEGIN
		SET @Query=@Query+LTRIM(RTRIM(@TMP))+' FOR XML AUTO'
	END
	ELSE
	BEGIN
		SET @Query=@Query+' SELECT * FROM '+RTRIM(@TMP)+' FOR XML AUTO'
	END
END

IF (@Opcion='GI' AND LEN(@Tabla)>0)
BEGIN
IF (@TMP IS  NULL)
	BEGIN                             
		SET @TMP=' '              
	END

SET @Query='
DECLARE @Columnas  TABLE ([N] SMALLINT , Columna VARCHAR(Max) )  
DECLARE @NoColumnas SMALLINT=0                              
DECLARE @Ciclos SMALLINT=1 
DECLARE @InsertInto VARCHAR(MAX)=''''  
DECLARE @InnerQuery VARCHAR(MAX)=''''

USE '+@BaseDeDatos+'
INSERT INTO @Columnas 
SELECT
ORDINAL_POSITION, 
COLUMN_NAME
FROM Information_schema.columns ISC
WHERE TABLE_SCHEMA='''+@Esquema+                              
  ''' AND TABLE_NAME='''+@Tabla+       
  ''' AND TABLE_CATALOG='''+@BaseDeDatos+
  ''' AND COLUMN_NAME NOT IN (''SyncDestination'',''PendingSyncDestination'',''SkuID'',''SaleCreditedto'')
ORDER BY ISC.ORDINAL_POSITION
USE '+RTRIM(CONVERT(VARCHAR(100),DB_NAME()))+'     

SELECT @NoColumnas= MAX([N]) FROM  @Columnas  

WHILE (@Ciclos<=@NoColumnas )                              
	BEGIN                               
		SELECT @InsertInto= @InsertInto+''[''+Columna+''],''            
		FROM @Columnas                              
		WHERE [N]=@Ciclos                          

		SELECT	@InnerQuery=@InnerQuery+'' +CASE WHEN [''+Columna+''] IS NULL THEN ''''Null'''' ELSE ''''''''''''''''+                              
				REPLACE(CONVERT(VARCHAR(MAX),RTRIM([''+Columna+''])) ,'''''''''''''''',''''''''  )                              
				+'''''''''''''''' END+'+'''+'''''','''''''+'                               
		FROM @Columnas                              
		WHERE [N]=@Ciclos                              

		SET @Ciclos=@Ciclos+1                              
	END     
                             
SELECT @InnerQuery=LEFT(@InnerQuery,LEN(@InnerQuery)-4)              
SELECT @InsertInto= SUBSTRING(@InsertInto,0, LEN(@InsertInto))    
SELECT @InnerQuery='' SELECT  ''+''''''INSERT INTO '+@BaseDeDatos+'.'+@Esquema+'.'+@Tabla+ '''+''(''+@InsertInto+'')'' 
                  +'' VALUES ( ''+'''''''' + ''+''+@InnerQuery+''+''+ '''''')'''' AS [BKINSERTS]''
                  +'' FROM  ' +@BaseDeDatos+'.'+@Esquema+'.'+@Tabla+'(NOLOCK) '' 
                  +'' WHERE 1=1 '+@TMP+''''+CHAR(13)+               

+' EXEC (@InnerQuery) '  
END

IF (@Opcion='UUP')
BEGIN
	SET @Query=@Query+'
	SELECT TOP '+LTRIM(RTRIM(CONVERT(CHAR(6),@N)))+' 
	tbl.name AS TABLA,
	ius.last_user_update AS ULTIMO_UPDATE
	FROM sys.dm_db_index_usage_stats ius(NOLOCK) 
	INNER JOIN sys.tables tbl ON (tbl.OBJECT_ID = ius.OBJECT_ID)
	WHERE ius.database_id = DB_ID()
	AND ius.last_user_update IS NOT NULL
	ORDER BY last_user_update DESC '
END

IF (@Opcion='RXT')
BEGIN
	--BASADO EN:http://www.sqlservercentral.com/scripts/tables/97194/
	SET @Query='
	SELECT TOP '+LTRIM(RTRIM(CONVERT(CHAR(6),@N)))+' 
	sys.tables.name AS TABLA,
	sys.partitions.rows AS REGISTROS, 
	sys.tables.max_column_id_used AS No_COLUMNAS, 
	sys.tables.create_date AS FECHA_CREACION, 
	sys.tables.modify_date AS FECHA_ULTIMA_MODIFICACION 
	FROM sys.allocation_units(NOLOCK) 
	INNER JOIN sys.partitions(NOLOCK)  ON sys.allocation_units.container_id = sys.partitions.partition_id 
	INNER JOIN (SELECT TOP (100) PERCENT object_id, index_id, type AS TYPE, COUNT(*) AS COUNT_TYPE
				FROM sys.indexes(NOLOCK)  AS indexes_1 
				GROUP BY object_id, type, index_id 
				ORDER BY object_id) AS INDEXES ON sys.partitions.object_id = INDEXES.object_id AND sys.partitions.index_id = INDEXES.index_id 
	RIGHT OUTER JOIN sys.database_principals 
	RIGHT OUTER JOIN sys.tables ON sys.database_principals.principal_id = sys.tables.principal_id ON INDEXES.object_id = sys.tables.object_id 
	GROUP BY sys.tables.name, sys.tables.create_date, sys.tables.modify_date, 
	CASE 
		WHEN sys.database_principals.name IS NULL THEN SCHEMA_NAME(sys.tables.schema_id) 
		ELSE sys.database_principals.name 
	END, 
	sys.tables.max_column_id_used, sys.partitions.rows
	ORDER BY REGISTROS DESC '
END

IF (@Opcion='UOM')
BEGIN
	--BASADO EN:http://www.sqlservercentral.com/scripts/Administration/98329/
	SET @Query=@Query+'
	SELECT 
	name AS OBJETO,
	type_desc AS TIPO,
	create_date AS FECHA_CREACION,
	modify_date AS FECHA_MODIFICACION
	FROM sys.objects (NOLOCK)
	WHERE 1=1 
	AND DATEDIFF(D,modify_date, GETDATE()) <'+@N+'
	ORDER BY modify_date DESC '
END

IF (@Opcion='BTT' AND LEN(@TMP)>1 )
BEGIN

SET @Query=N'
--VARIABLES DE ENTORNO--------------------------------------------------------
DECLARE @Buscar VARCHAR(MAX)=NULL
DECLARE @Ciclos TINYINT=0'
--REFERENCIAS: http://sqlservercodebook.blogspot.mx/2008/03/check-if-temporary-table-exists.html
SET @Query=@Query+'
IF OBJECT_ID(''tempdb..#Results'') IS NOT NULL  
	BEGIN
		DROP TABLE #Results
	END

CREATE TABLE #Results (ColumnName NVARCHAR(370), ColumnValue NVARCHAR(3630))

SET NOCOUNT ON'

IF ((SELECT CHARINDEX('IN',@TMP))>0)
	BEGIN 
	SET @Query=@Query+'
		SET @Buscar=QUOTENAME(''%'' +(SELECT LTRIM(RTRIM(SUBSTRING('''+@TMP+''',0,(CHARINDEX(''IN'','''+@TMP+''')-1)))))+ ''%'','''''''')'
		--SEPARACION DE CADENA PARA FORMAR TABLA---------------------------------------------------------
		--BASADO EN LA FUNCION: http://emmersonmiranda-net.blogspot.mx/2008/08/generando-mltiples-filas-de-un-string.html
		--Writen By: Emmerson Miranda
	SET @Query=@Query+
	   'DECLARE @Tablas TABLE (Tabla NVARCHAR(75),N TINYINT IDENTITY)
		DECLARE @Array VARCHAR(MAX)=(SELECT LTRIM(RTRIM((SELECT SUBSTRING('''+@TMP+''',(SELECT CHARINDEX(''IN'','''+@TMP+''')+2),LEN('''+@TMP+'''))))))
		DECLARE @SeparatorPosition INT=0
		DECLARE @ArrayValue VARCHAR(MAX)=NULL

		SET @Array = @Array + '',''

		WHILE PATINDEX(''%'' + '','' + ''%'' , @Array) <> 0
		BEGIN
			SET @SeparatorPosition = PATINDEX(''%'' + '','' + ''%'' , @Array)
			SET @ArrayValue = SUBSTRING(@Array, 0, @SeparatorPosition)
			SET @Array = STUFF(@Array, 1, @SeparatorPosition, '''')
			INSERT INTO @Tablas SELECT ''[dbo].[''+@ArrayValue+'']'' AS Tabla
		END
		'
		--SEPARACION DE CADENA PARA FORMAR TABLA---------------------------------------------------------
	END
ELSE
	BEGIN
		SET @Query=@Query+'
		SET @Buscar=QUOTENAME(''%'' +(LTRIM(RTRIM('''+@TMP+''')))+ ''%'','''''''')'
	END

SET @Query=@Query+'
		DECLARE @TableName NVARCHAR(256), @ColumnName NVARCHAR(128)
		SET  @TableName = '''''
	
IF((SELECT CHARINDEX('IN',@TMP))>0)
	BEGIN
		SET @Query=@Query+'
		SET @Ciclos=(SELECT MAX(N) FROM @Tablas)
		WHILE (@Ciclos>0)'
	END	
ELSE
	BEGIN
		SET @Query=@Query+'
		WHILE @TableName IS NOT NULL'
	END
	
		SET @Query=@Query+'
		BEGIN
		SET @ColumnName = '''''

IF((SELECT CHARINDEX('IN',@TMP))>0)
	BEGIN
		SET @Query=@Query+'
		SET @TableName =(SELECT REPLACE(Tabla,'' '', '''')  FROM @Tablas WHERE N=@Ciclos)'
	END	
ELSE
	BEGIN
		SET @Query=@Query+'
		SET @TableName = 
		(
			SELECT MIN(QUOTENAME(TABLE_SCHEMA) + ''.'' + QUOTENAME(TABLE_NAME))
			FROM 	INFORMATION_SCHEMA.TABLES
			WHERE 		TABLE_TYPE = ''BASE TABLE''
				AND	QUOTENAME(TABLE_SCHEMA) + ''.'' + QUOTENAME(TABLE_NAME) > @TableName
				AND	OBJECTPROPERTY(
						OBJECT_ID(
							QUOTENAME(TABLE_SCHEMA) + ''.'' + QUOTENAME(TABLE_NAME)
							 ), ''IsMSShipped''
						       ) = 0
		)'
	END
		
SET @Query=@Query+'		
		WHILE (@TableName IS NOT NULL) AND (@ColumnName IS NOT NULL)
		BEGIN
			SET @ColumnName =
			(
				SELECT MIN(QUOTENAME(COLUMN_NAME))
				FROM 	INFORMATION_SCHEMA.COLUMNS
				WHERE 		TABLE_SCHEMA	= PARSENAME(@TableName, 2)
					AND	TABLE_NAME	= PARSENAME(@TableName, 1)
					AND	DATA_TYPE IN (''char'', ''varchar'', ''nchar'', ''nvarchar'')
					AND	QUOTENAME(COLUMN_NAME) > @ColumnName
			)
	
			IF @ColumnName IS NOT NULL
			BEGIN
				INSERT INTO #Results
				EXEC
				(
					''SELECT '''''' + @TableName + ''.'' + @ColumnName + '''''', LEFT('' + @ColumnName + '', 3630) 
					FROM '' + @TableName + '' (NOLOCK) '' +
					'' WHERE '' + @ColumnName + '' LIKE '' + @Buscar
				)
			END
		END'	
		
IF((SELECT CHARINDEX('IN',@TMP))>0)
	BEGIN
		SET @Query=@Query+'
		SET @Ciclos = @Ciclos-1'
	END	

SET @Query=@Query+'
	END

	SELECT DISTINCT ColumnName AS [TABLA/COLUMNA] , ColumnValue AS [VALOR] FROM #Results
	DROP TABLE #Results
	
SET NOCOUNT OFF'

IF(@Print=1)
BEGIN
	PRINT @Query 
END
 
END

IF (@Opcion IN ('ACPU','TCPU','AE','TE','EC','AIO','TIO','ALR','TLR','ALW','TLW','APR','TPR'))
BEGIN
	--COPIADO Y ADAPTADO TOTALMENTE DE:http://www.databasejournal.com/features/mssql/article.php/3802936/Finding-the-Worst-Performing-T-SQL-Statements-on-an-Instance.htm
	--ESCRITO POR / WRITTEN BY: Gregory A. Larsen
	--REFERENCIA: http://technet.microsoft.com/es-es/library/ms191475(v=sql.105).aspx
	
	-- Check for valid @Opcion parameter
	IF ((SELECT CASE WHEN 
			  @Opcion in ('ACPU','TCPU','AE','TE','EC','AIO','TIO','ALR','TLR','ALW','TLW','APR','TPR') 
				 THEN 1 ELSE 0 END) = 0)
	BEGIN 
	-- Abort if invalid @Opcion parameter entered
	   RAISERROR('@Opcion parameter not APCU, TCPU, AE, TE, EC, AIO, TIO, ALR, TLR, ALW, TLW, APR or TPR',11,1)
	   RETURN
	 END
	 SELECT TOP (@N) 
	 COALESCE(DB_NAME(ST.dbid), DB_NAME(CAST(PA.value AS INT))+'*', 'Resource') AS [BaseDeDatos]  
	 ,OBJECT_SCHEMA_NAME(ST.objectid,ST.dbid) [Esquema] 
	 ,OBJECT_NAME(ST.objectid,ST.dbid) [Objecto] 
	 ,objtype [Tipo]
	-- Find the offset of the actual statement being executed
	 ,SUBSTRING(text, 
					 CASE WHEN statement_start_offset = 0 
						  OR statement_start_offset IS NULL  
						  THEN 1  
						  ELSE statement_start_offset/2 + 1 
					 END, 
					 CASE WHEN statement_end_offset = 0 
						  OR statement_end_offset = -1  
						  OR statement_end_offset IS NULL  
						  THEN LEN(text)  
						  ELSE statement_end_offset/2 
					 END - 
					 CASE WHEN statement_start_offset = 0 
						  OR statement_start_offset IS NULL 
						  THEN 1  
						  ELSE statement_start_offset/2  
					 END + 1 
	 )  AS [Statement]  
	 ,execution_count [Execution Count]  
	 ,(total_logical_reads + total_logical_writes + total_physical_reads )/execution_count [Average IOs] 
	 ,total_logical_reads + total_logical_writes + total_physical_reads [Total IOs]  
	 ,total_logical_reads/execution_count [Avg Logical Reads] 
	 ,total_logical_reads [Total Logical Reads]  
	 ,total_logical_writes/execution_count [Avg Logical Writes]  
	 ,total_logical_writes [Total Logical Writes]  
	 ,total_physical_reads/execution_count [Avg Physical Reads] 
	 ,total_physical_reads [Total Physical Reads]   
	 ,total_worker_time / execution_count [Avg CPU] 
	 ,total_worker_time [Total CPU] 
	 ,total_elapsed_time / execution_count [Avg Elapsed Time] 
	 ,total_elapsed_time  [Total Elasped Time] 
	 ,last_execution_time [Last Execution Time]
	 ,QS.plan_handle
	 FROM sys.dm_exec_query_stats QS(NOLOCK)  
	 JOIN sys.dm_exec_cached_plans CP(NOLOCK) ON QS.plan_handle = CP.plan_handle 
	 CROSS APPLY sys.dm_exec_sql_text(QS.plan_handle) ST
	 OUTER APPLY sys.dm_exec_plan_attributes(QS.plan_handle) PA
	 WHERE attribute = 'dbid' 
	 AND CASE WHEN @BaseDeDatos = '<not supplied>' 
			  THEN '<not supplied>'
			  ELSE COALESCE(DB_NAME(ST.dbid), DB_NAME(CAST(PA.value AS INT)) + '*', 'Resource') 
		 END
	 IN (RTRIM(@BaseDeDatos),RTRIM(@BaseDeDatos) + '*')  
	 ORDER BY CASE 
				  WHEN @Opcion = 'ACPU' THEN total_worker_time / execution_count 
				  WHEN @Opcion = 'TCPU'  THEN total_worker_time
				  WHEN @Opcion = 'AE'   THEN total_elapsed_time / execution_count
				  WHEN @Opcion = 'TE'   THEN total_elapsed_time  
				  WHEN @Opcion = 'EC'   THEN execution_count
				  WHEN @Opcion = 'AIO'  THEN (total_logical_reads + total_logical_writes + total_physical_reads) / execution_count  
				  WHEN @Opcion = 'TIO'  THEN total_logical_reads + total_logical_writes + total_physical_reads
				  WHEN @Opcion = 'ALR'  THEN total_logical_reads  / execution_count
				  WHEN @Opcion = 'TLR'  THEN total_logical_reads 
				  WHEN @Opcion = 'ALW'  THEN total_logical_writes / execution_count
				  WHEN @Opcion = 'TLW'  THEN total_logical_writes  
				  WHEN @Opcion = 'APR'  THEN total_physical_reads / execution_count 
				  WHEN @Opcion = 'TPR'  THEN total_physical_reads
			  END 
	 DESC
END

--EJECUCION------------------------------------------------------------------------------------------------
IF (@Opcion NOT IN ('ACPU','TCPU','AE','TE','EC','AIO','TIO','ALR','TLR','ALW','TLW','APR','TPR'))
BEGIN
	IF (LEN(@Query)>1 )
		BEGIN
			IF (@BaseDeDatos<>DB_NAME())
			BEGIN
				SET @Query=@Query+CHAR(13)+'	USE '+RTRIM(CONVERT(VARCHAR(100),DB_NAME()))
			END
			
			IF (@Print=1)
			BEGIN
				PRINT (@Query)
			END
			
			EXEC (@Query)
		END
	ELSE
		BEGIN
			SELECT '[ERROR] --> PARAMETRO @TMP INCORRECTO'
			PRINT (@Query)
		END
END

SET NOCOUNT OFF

END

    
