*&---------------------------------------------------------------------*
*& Report Z_ALV_TREE_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_ALV_TREE_REPORT.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS run.
    METHODS pbo_0100.
    METHODS pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_master,
             master_id TYPE c LENGTH 5,
             name      TYPE c LENGTH 30,
             category  TYPE c LENGTH 15,
             status    TYPE c LENGTH 10,
           END OF ty_master.

    TYPES: BEGIN OF ty_detail,
             master_id TYPE c LENGTH 5,
             item_no   TYPE n LENGTH 3,
             item_text TYPE c LENGTH 40,
             amount    TYPE p LENGTH 10 DECIMALS 2,
             currency  TYPE c LENGTH 3,
           END OF ty_detail.

    TYPES: BEGIN OF ty_tree_line,
             node_text TYPE lvc_value,
             item_text TYPE c LENGTH 40,
             amount    TYPE p LENGTH 10 DECIMALS 2,
             currency  TYPE c LENGTH 3,
           END OF ty_tree_line.

    DATA: mt_master      TYPE STANDARD TABLE OF ty_master,
          mt_detail      TYPE STANDARD TABLE OF ty_detail,
          mt_tree_outtab TYPE STANDARD TABLE OF ty_tree_line,
          mo_main        TYPE REF TO cl_gui_container,
          mo_tree        TYPE REF TO cl_gui_alv_tree,
          mv_built       TYPE abap_bool.

    METHODS fill_test_data.
    METHODS build_ui_once.
    METHODS build_tree.
    METHODS get_fcat RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    fill_test_data( ).
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD fill_test_data.
    mt_master = VALUE #(
      ( master_id = 'M0001' name = 'Vertrag A' category = 'Standard' status = 'AKTIV' )
      ( master_id = 'M0002' name = 'Vertrag B' category = 'Premium'  status = 'AKTIV' )
      ( master_id = 'M0003' name = 'Vertrag C' category = 'Basic'    status = 'INAKTIV' )
      ( master_id = 'M0004' name = 'Vertrag D' category = 'Premium'  status = 'AKTIV' ) ).

    mt_detail = VALUE #(
      ( master_id = 'M0001' item_no = '001' item_text = 'Leistung 1' amount = '120.50' currency = 'EUR' )
      ( master_id = 'M0001' item_no = '002' item_text = 'Leistung 2' amount = '89.90'  currency = 'EUR' )
      ( master_id = 'M0002' item_no = '001' item_text = 'Service A'  amount = '199.00' currency = 'EUR' )
      ( master_id = 'M0002' item_no = '002' item_text = 'Service B'  amount = '49.99'  currency = 'EUR' )
      ( master_id = 'M0003' item_no = '001' item_text = 'Paket X'    amount = '15.00'  currency = 'EUR' )
      ( master_id = 'M0004' item_no = '001' item_text = 'Option Q'   amount = '350.00' currency = 'EUR' )
      ( master_id = 'M0004' item_no = '002' item_text = 'Option R'   amount = '75.50'  currency = 'EUR' ) ).
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS 'ZHR_MEY001'.
    IF mv_built = abap_false.
      build_ui_once( ).
      mv_built = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'CANC' or '&F03'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD build_ui_once.
    mo_main = cl_gui_container=>screen0.
    build_tree( ).
  ENDMETHOD.

  METHOD get_fcat.
    rt_fcat = VALUE lvc_t_fcat(
      ( fieldname = 'NODE_TEXT' coltext = 'Knoten' outputlen = 60 )
      ( fieldname = 'ITEM_TEXT' coltext = 'Position' outputlen = 40 )
      ( fieldname = 'AMOUNT'    coltext = 'Betrag'   outputlen = 15 )
      ( fieldname = 'CURRENCY'  coltext = 'Whrg'     outputlen = 5 ) ).
  ENDMETHOD.

  METHOD build_tree.
    DATA: ls_hhdr       TYPE treev_hhdr,
          lt_fcat       TYPE lvc_t_fcat,
          ls_tree_line  TYPE ty_tree_line,
          ls_master     TYPE ty_master,
          ls_detail     TYPE ty_detail,
          lv_master_key TYPE lvc_nkey,
          lv_detail_key TYPE lvc_nkey.

    lt_fcat = get_fcat( ).

    ls_hhdr-heading = 'Verträge und Detailpositionen'.
    ls_hhdr-width = 60.

    CREATE OBJECT mo_tree
      EXPORTING
        parent              = mo_main
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_single.

    mo_tree->set_table_for_first_display(
      EXPORTING
        is_hierarchy_header = ls_hhdr
      CHANGING
        it_outtab           = mt_tree_outtab
        it_fieldcatalog     = lt_fcat ).

    LOOP AT mt_master INTO ls_master.
      CLEAR ls_tree_line.
      ls_tree_line-node_text = |{ ls_master-master_id } - { ls_master-name } ({ ls_master-status })|.
      APPEND ls_tree_line TO mt_tree_outtab.

      mo_tree->add_node(
        EXPORTING
          i_relat_node_key = ''
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = ls_tree_line-node_text
          is_outtab_line   = ls_tree_line
        IMPORTING
          e_new_node_key   = lv_master_key ).

      LOOP AT mt_detail INTO ls_detail WHERE master_id = ls_master-master_id.
        CLEAR ls_tree_line.
        ls_tree_line-node_text = |{ ls_detail-master_id }/{ ls_detail-item_no }|.
        ls_tree_line-item_text = ls_detail-item_text.
        ls_tree_line-amount    = ls_detail-amount.
        ls_tree_line-currency  = ls_detail-currency.
        APPEND ls_tree_line TO mt_tree_outtab.

        mo_tree->add_node(
          EXPORTING
            i_relat_node_key = lv_master_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = ls_tree_line-node_text
            is_outtab_line   = ls_tree_line
          IMPORTING
            e_new_node_key   = lv_detail_key ).
      ENDLOOP.
    ENDLOOP.

    mo_tree->frontend_update( ).
  ENDMETHOD.
ENDCLASS.

DATA go_app TYPE REF TO lcl_app.

START-OF-SELECTION.
  CREATE OBJECT go_app.
  go_app->run( ).

MODULE status_0100 OUTPUT.
  go_app->pbo_0100( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  go_app->pai_0100( sy-ucomm ).
ENDMODULE.
