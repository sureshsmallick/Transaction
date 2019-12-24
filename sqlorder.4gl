DATABASE mydb
SCHEMA mydb

TYPE order_t RECORD
           store_num    LIKE orders.store_num,
           store_name   LIKE customer.store_name,
           order_num    LIKE orders.order_num,
           order_date   LIKE orders.order_date,
           fac_code     LIKE orders.fac_code,
           ship_instr   LIKE orders.ship_instr,
           promo        LIKE orders.promo
      END RECORD,
      item_t RECORD
           stock_num    LIKE items.stock_num,
           description  LIKE stock.description,
           quantity     LIKE items.quantity,
           unit         LIKE stock.unit,
           price        LIKE items.price,
           line_total   DECIMAL(9,2)
      END RECORD
DEFINE order_rec order_t,
       arr_ordnums DYNAMIC ARRAY OF INTEGER,
       orders_index INTEGER,
       arr_items DYNAMIC ARRAY OF item_t,
       order_total DECIMAL(9,2)

CONSTANT title1 = "Orders"
CONSTANT title2 = "Items"

CONSTANT msg01 = "You must query first"
CONSTANT msg02 = "Enter search criteria"
CONSTANT msg03 = "Canceled by user"
CONSTANT msg04 = "No rows found, enter new search criteria"
CONSTANT msg05 = "End of list"
CONSTANT msg06 = "Beginning of list"
CONSTANT msg07 = "Invalid stock number"
CONSTANT msg08 = "Row added to the database"
CONSTANT msg09 = "Row updated in the database"
CONSTANT msg10 = "Row deleted from the database"
CONSTANT msg11 = "New order record created"
CONSTANT msg12 = "This customer does not exist"
CONSTANT msg13 = "Quantity must be greather than zero"
CONSTANT msg14 = "%1 orders found in the database"
CONSTANT msg15 = "There are no orders selected, exit program?"
CONSTANT msg16 = "Item is not available in current factory %1"
CONSTANT msg17 = "Order %1 saved in database"
CONSTANT msg18 = "Order input program, version 1.01"
CONSTANT msg19 = "To save changes, move focus to another row or to the order header"

CONSTANT move_first = -2
CONSTANT move_prev  = -1
CONSTANT move_next  = 1
CONSTANT move_last  = 2

MAIN
  DEFER INTERRUPT

  CONNECT TO "mydb"
  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "sqlform"

  CALL orditems_dialog()

  CLOSE WINDOW w1

END MAIN

FUNCTION orditems_dialog()
  DEFINE query_ok SMALLINT,
         id INTEGER,
         name LIKE customer.store_name,
         opflag CHAR(1),
         curr_pa INTEGER

  DIALOG ATTRIBUTES(UNBUFFERED)

   INPUT BY NAME order_rec.*, order_total
     ATTRIBUTES(WITHOUT DEFAULTS, NAME="order")

    ON ACTION find
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       CALL order_query()

    ON ACTION new
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       IF NOT order_new() THEN
          EXIT PROGRAM
       END IF

    ON ACTION save
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF

    ON CHANGE store_num
       IF NOT order_check_store_num() THEN NEXT FIELD CURRENT END IF

    ON ACTION zoom1
       CALL display_custlist() RETURNING id, name
       IF id > 0 THEN
          LET order_rec.store_num = id
          LET order_rec.store_name = name
          CALL DIALOG.setFieldTouched("store_num", TRUE)
       END IF

    AFTER INPUT
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF

    ON ACTION first
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       CALL order_move(move_first)
    ON ACTION previous
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       CALL order_move(move_prev)
    ON ACTION next
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       CALL order_move(move_next)
    ON ACTION last
       IF NOT order_update(DIALOG) THEN NEXT FIELD CURRENT END IF
       CALL order_move(move_last)

  END INPUT

  INPUT ARRAY arr_items FROM sa_items.*
    ATTRIBUTES (WITHOUT DEFAULTS, INSERT ROW = FALSE)

    BEFORE INPUT
      MESSAGE msg19

    BEFORE ROW
      LET opflag = "N"
      LET curr_pa = DIALOG.getCurrentRow("sa_items")
      CALL DIALOG.setFieldActive("stock_num", FALSE)

    BEFORE INSERT
      LET opflag = "T"
      LET arr_items[curr_pa].quantity = 1
      CALL DIALOG.setFieldActive("stock_num", TRUE)

    AFTER INSERT
      LET opflag = "I"

    BEFORE DELETE
      IF opflag="N" THEN
         IF NOT item_delete(curr_pa) THEN
            CANCEL DELETE
         END IF
      END IF

    AFTER DELETE
      LET opflag="N"

    ON ROW CHANGE
      IF opflag != "I" THEN LET opflag = "M" END IF

    AFTER ROW
      IF opflag == "I" THEN
         IF NOT item_insert(curr_pa) THEN
            NEXT FIELD CURRENT
         END IF
         CALL items_line_total(curr_pa)
      END IF
      IF opflag == "M" THEN
         IF NOT item_update(curr_pa) THEN
            NEXT FIELD CURRENT
         END IF
         CALL items_line_total(curr_pa)
      END IF

    ON ACTION zoom2
       LET id = display_stocklist()
       IF id > 0 THEN
          IF NOT get_stock_info(curr_pa,id) THEN
             LET arr_items[curr_pa].stock_num = NULL
          ELSE
             LET arr_items[curr_pa].stock_num = id
          END IF
          CALL DIALOG.setFieldTouched("stock_num", TRUE)
       END IF

    ON CHANGE stock_num
       IF NOT get_stock_info(curr_pa,
                  arr_items[curr_pa].stock_num) THEN
          LET arr_items[curr_pa].stock_num = NULL
          CALL __mbox_ok(title2,msg07,"stop")
          NEXT FIELD stock_num
       ELSE
          CALL items_line_total(curr_pa)
       END IF

    ON CHANGE quantity
       IF arr_items[curr_pa].quantity <= 0 THEN
          CALL __mbox_ok(title2,msg13,"stop")
          NEXT FIELD quantity
       ELSE
          CALL items_line_total(curr_pa)
       END IF

  END INPUT

  BEFORE DIALOG
     IF NOT order_select("1=1") THEN
        CALL order_query()
     END IF

  ON ACTION about
     CALL __mbox_ok(title1,msg18,"information")

  ON ACTION quit
     EXIT DIALOG

  END DIALOG

END FUNCTION

FUNCTION order_new()

  SELECT MAX(order_num)+1 INTO order_rec.order_num
    FROM orders
  IF order_rec.order_num IS NULL
   OR order_rec.order_num == 0 THEN
     LET order_rec.order_num = 1
  END IF
  LET order_total = 0
  -- We keep the same store...
  LET order_rec.order_date = TODAY
  LET order_rec.fac_code = "ASC"
  LET order_rec.ship_instr = "FEDEX"
  LET order_rec.promo = "N"
  

  WHENEVER ERROR CONTINUE
  INSERT INTO orders (
     store_num,
     order_num,
     order_date,
     fac_code,
     ship_instr,
     promo
  ) VALUES (
     order_rec.store_num,
     order_rec.order_num,
     order_rec.order_date,
     order_rec.fac_code,
     order_rec.ship_instr,
     order_rec.promo
  )
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE <> 0 THEN
     CLEAR FORM
     CALL __mbox_ok(title1,SQLERRMESSAGE,"stop")
     RETURN FALSE
  END IF

  CALL arr_ordnums.insertElement(1)
  LET arr_ordnums[1] = order_rec.order_num
  CALL arr_items.clear()

  MESSAGE msg11

  RETURN TRUE

END FUNCTION

FUNCTION order_check_store_num()

  SELECT store_name INTO order_rec.store_name
         FROM customer
        WHERE store_num = order_rec.store_num

  IF SQLCA.SQLCODE == NOTFOUND THEN
     ERROR msg12
     RETURN FALSE
  END IF

  RETURN TRUE

END FUNCTION

FUNCTION order_validate(d)
  DEFINE d ui.Dialog

  IF NOT d.getFieldTouched("orders.*") THEN
     RETURN TRUE
  END IF

  IF d.validate("orders.*") < 0 THEN
     RETURN FALSE
  END IF

  IF NOT order_check_store_num() THEN
     RETURN FALSE
  END IF

  RETURN TRUE

END FUNCTION

FUNCTION order_update(d)
  DEFINE d ui.Dialog

  IF NOT order_validate(d) THEN RETURN FALSE END IF

  WHENEVER ERROR CONTINUE
  UPDATE orders SET
           store_num  = order_rec.store_num,
           order_date = order_rec.order_date,
           fac_code   = order_rec.fac_code,
           ship_instr = order_rec.ship_instr,
           promo      = order_rec.promo
     WHERE orders.order_num = order_rec.order_num
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE <> 0 THEN
     CALL __mbox_ok(title1,SQLERRMESSAGE,"stop")
     RETURN FALSE
  END IF

  CALL d.setFieldTouched("orders.*", FALSE)
  MESSAGE SFMT(msg17, order_rec.order_num)

  RETURN TRUE

END FUNCTION

FUNCTION order_query()
  DEFINE where_clause STRING,
         id INTEGER, name STRING

  MESSAGE msg02
  CLEAR FORM

  WHILE TRUE

  LET int_flag = FALSE
  CONSTRUCT BY NAME where_clause ON
      orders.store_num,
      customer.store_name,
      orders.order_num,
      orders.order_date,
      orders.fac_code

    ON ACTION zoom1
       CALL display_custlist() RETURNING id, name
       IF id > 0 THEN
          DISPLAY id TO orders.store_num
          DISPLAY name TO customer.store_name
       END IF

    ON ACTION about
       CALL __mbox_ok(title1,msg18,"information")

  END CONSTRUCT

  IF int_flag THEN
     MESSAGE msg03
     IF arr_ordnums.getLength()==0 THEN
        IF __mbox_yn(title1,msg15,"stop") THEN
           EXIT PROGRAM
        END IF
        CONTINUE WHILE
     END IF
     RETURN
  ELSE
     IF order_select(where_clause) THEN
        EXIT WHILE
     END IF
  END IF

  END WHILE

END FUNCTION

FUNCTION order_select(where_clause)
  DEFINE where_clause STRING,
         sql_text STRING,
         index, ordnum INTEGER

  CALL arr_ordnums.clear()
  LET orders_index = 0

  DECLARE order_curs CURSOR FROM
         "SELECT orders.order_num"
      || " FROM orders, customer"
      || " WHERE orders.store_num = customer.store_num"
      || " AND " || where_clause

  LET index = 1
  FOREACH order_curs INTO ordnum
      LET arr_ordnums[index] = ordnum
      LET index = index + 1
  END FOREACH

  FREE order_curs

  IF arr_ordnums.getLength() == 0 THEN
     CALL __mbox_ok(title1,msg04,"information")
  ELSE
     MESSAGE SFMT(msg14, arr_ordnums.getLength())
  END IF

  RETURN order_fetch(move_first)

END FUNCTION

FUNCTION order_fetch(dir)
  DEFINE dir SMALLINT

  CASE dir
    WHEN move_first
      LET orders_index = 1
    WHEN move_prev
      IF orders_index > 1 THEN
         LET orders_index = orders_index - 1
      ELSE
         RETURN FALSE
      END IF
    WHEN move_next
      IF orders_index < arr_ordnums.getLength() THEN
         LET orders_index = orders_index + 1
      ELSE
         RETURN FALSE
      END IF
    WHEN move_last
      LET orders_index = arr_ordnums.getLength()
  END CASE

  SELECT orders.store_num,
         customer.store_name,
         orders.order_num,
         orders.order_date,
         orders.fac_code,
         orders.ship_instr,
         orders.promo
    INTO order_rec.*
    FROM orders, customer
    WHERE orders.store_num = customer.store_num
     AND orders.order_num = arr_ordnums[orders_index]

  CALL items_fetch()

  RETURN TRUE

END FUNCTION

FUNCTION items_fetch()
  DEFINE item_cnt INTEGER,
         item_rec item_t

  IF order_rec.order_num IS NULL THEN
     RETURN
  END IF

  DECLARE items_curs CURSOR FOR
     SELECT items.stock_num,
            stock.description,
            items.quantity,
            stock.unit,
            items.price,
            items.price * items.quantity line_total
        FROM items, stock
       WHERE items.order_num = order_rec.order_num
         AND items.stock_num = stock.stock_num

  LET item_cnt = 0
  CALL arr_items.clear()
  FOREACH items_curs INTO item_rec.*
      LET item_cnt = item_cnt + 1
      LET arr_items[item_cnt].* = item_rec.*
  END FOREACH
  FREE items_curs

  CALL order_total()

END FUNCTION

FUNCTION order_move(dir)
  DEFINE dir SMALLINT

  MESSAGE " "
  IF NOT order_fetch(dir) THEN
     IF dir == move_next THEN MESSAGE msg05 END IF
     IF dir == move_prev THEN MESSAGE msg06 END IF
  END IF

END FUNCTION

FUNCTION items_line_total(curr_pa)
  DEFINE curr_pa SMALLINT
  LET arr_items[curr_pa].line_total =
      arr_items[curr_pa].quantity * arr_items[curr_pa].price
END FUNCTION

FUNCTION order_total()
  DEFINE i SMALLINT

  LET order_total = 0
  FOR i = 1 TO arr_items.getLength()
      IF arr_items[i].line_total IS NOT NULL THEN
         LET order_total = order_total + arr_items[i].line_total
      END IF
  END FOR

END FUNCTION

FUNCTION get_stock_info(curr_pa, id)
  DEFINE curr_pa SMALLINT,
         id INTEGER,
         sqltext STRING

  IF id IS NULL THEN
     RETURN FALSE
  END IF

  LET sqltext="SELECT description, unit,"
  IF order_rec.promo = "N" THEN
     LET sqltext=sqltext || "reg_price"
  ELSE
     LET sqltext=sqltext || "promo_price"
  END IF
  LET sqltext=sqltext ||
      " FROM stock WHERE stock_num = ? AND fac_code = ?"

  WHENEVER ERROR CONTINUE
  PREPARE get_stock_cursor FROM sqltext
  EXECUTE get_stock_cursor
        INTO arr_items[curr_pa].description,
             arr_items[curr_pa].unit,
             arr_items[curr_pa].price
        USING id, order_rec.fac_code
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE == NOTFOUND THEN
     CALL __mbox_ok(title1,SFMT(msg16,order_rec.fac_code),"information")
     LET arr_items[curr_pa].description = NULL
     LET arr_items[curr_pa].unit = NULL
     LET arr_items[curr_pa].price = NULL
  END IF

  RETURN (SQLCA.SQLCODE == 0)

END FUNCTION

FUNCTION item_insert(curr_pa)
  DEFINE curr_pa SMALLINT

  WHENEVER ERROR CONTINUE
  INSERT INTO items (
     order_num,
     stock_num,
     quantity,
     price
  ) VALUES (
     order_rec.order_num,
     arr_items[curr_pa].stock_num,
     arr_items[curr_pa].quantity,
     arr_items[curr_pa].price
  )
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE == 0 THEN
     MESSAGE msg08
     CALL order_total()
     RETURN TRUE
  ELSE
     CALL __mbox_ok(title2,SQLERRMESSAGE,"stop")
     RETURN FALSE
  END IF

END FUNCTION

FUNCTION item_update(curr_pa)
  DEFINE curr_pa SMALLINT

  WHENEVER ERROR CONTINUE
  UPDATE items SET
    items.quantity  = arr_items[curr_pa].quantity
     WHERE items.stock_num = arr_items[curr_pa].stock_num
       AND items.order_num = order_rec.order_num
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE == 0 THEN
     MESSAGE msg09
     CALL order_total()
     RETURN TRUE
  ELSE
     CALL __mbox_ok(title2,SQLERRMESSAGE,"stop")
     RETURN FALSE
  END IF

END FUNCTION

FUNCTION item_delete(curr_pa)
  DEFINE curr_pa SMALLINT

  WHENEVER ERROR CONTINUE
  DELETE FROM items
     WHERE items.stock_num = arr_items[curr_pa].stock_num
     AND items.order_num = order_rec.order_num
  WHENEVER ERROR STOP

  IF SQLCA.SQLCODE == 0 THEN
     MESSAGE msg10
     CALL order_total()
     RETURN TRUE
  ELSE
     CALL __mbox_ok(title2,SQLERRMESSAGE,"stop")
     RETURN FALSE
  END IF

END FUNCTION

FUNCTION __mbox_ok(title,message,icon)
  DEFINE title, message, icon STRING
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "OK"
  END MENU
END FUNCTION

FUNCTION __mbox_yn(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r SMALLINT
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "Yes" LET r=TRUE
     COMMAND "No"  LET r=FALSE
  END MENU
  RETURN r
END FUNCTION
