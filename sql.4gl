DATABASE mydb
SCHEMA mydb

TYPE cust_t RECORD
           store_num     LIKE customer.store_num,
           store_name    LIKE customer.store_name,
           city          LIKE customer.city
       END RECORD

DEFINE cust_arr DYNAMIC ARRAY OF cust_t

FUNCTION custlist_fill(where_clause)
  DEFINE where_clause STRING
  DEFINE idx SMALLINT
  DEFINE cust_rec cust_t

   DECLARE custlist_curs CURSOR FROM  
   " SELECT store_num,store_name,city "||
    " FROM customer"||
    " WHERE "||where_clause||
    " ORDER BY store_num"

  LET idx = 0
  CALL cust_arr.clear()
  FOREACH custlist_curs INTO cust_rec.*
    LET idx = idx + 1
    LET cust_arr[idx].* = cust_rec.*
  END FOREACH

END FUNCTION
  
FUNCTION display_custlist()

  DEFINE ret_num LIKE customer.store_num
  DEFINE ret_name LIKE customer.store_name
  DEFINE where_clause STRING
  DEFINE idx  SMALLINT

  OPEN WINDOW wcust WITH FORM "sql"
   
  LET ret_num = 0
  LET ret_name = NULL

  DIALOG ATTRIBUTES(UNBUFFERED)

     CONSTRUCT BY NAME where_clause ON customer.store_name
     END CONSTRUCT

     DISPLAY ARRAY cust_arr TO sa_cust.* 
     END DISPLAY

     BEFORE DIALOG
        CALL custlist_fill("1 = 1")

     ON ACTION fetch
        CALL custlist_fill(where_clause)

     ON ACTION accept
        LET idx = DIALOG.getCurrentRow("sa_cust")
        IF idx > 0 THEN
           LET ret_num = cust_arr[idx].store_num
           LET ret_name = cust_arr[idx].store_name
           EXIT DIALOG
        END IF

     ON ACTION cancel
        EXIT DIALOG

  END DIALOG   
  
  CLOSE WINDOW wcust

  RETURN ret_num, ret_name 

END FUNCTION

