/*********************************************************************************************************************
@parameterName
Parameters are indicated using a @ prefix, and are replaced with the actual values provided in the renderSql call.

{DEFAULT @parameterName = parameterValue}
Default values for parameters can be defined using curly and the DEFAULT keyword.

{if}?{then}:{else}
The if-then-else pattern is used to turn on or off blocks of SQL code.
*********************************************************************************************************************/

{DEFAULT @table = 'table_name_here' }

select * from @table