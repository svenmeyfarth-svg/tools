*&---------------------------------------------------------------------*
*& Report Z_ALV_TREE_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_alv_tree_report.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS run.
    METHODS pbo_0100.
    METHODS pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_master,
             master_id TYPE c LENGTH 8,
             name      TYPE c LENGTH 30,
             category  TYPE c LENGTH 15,
             status    TYPE c LENGTH 10,
           END OF ty_master.

    TYPES: BEGIN OF ty_detail,
             master_id TYPE c LENGTH 8,
             item_no   TYPE n LENGTH 3,
             item_text TYPE lvc_value,
             amount    TYPE p LENGTH 10 DECIMALS 2,
             currency  TYPE c LENGTH 3,
           END OF ty_detail.

    TYPES: BEGIN OF ty_tree_line,
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
    DATA: ls_master TYPE ty_master,
          lv_index  TYPE i,
          lv_amount TYPE p LENGTH 10 DECIMALS 2,
          lv_monat  TYPE string.

    mt_master = VALUE #(
    ( master_id = '48291736' name = 'Anna Schmidt'  category = 'Vertrieb' status = 'AKTIV'   )
    ( master_id = '90531427' name = 'Ben Wagner'    category = 'IT'       status = 'AKTIV'   )
    ( master_id = '17645289' name = 'Clara Neumann' category = 'HR'       status = 'INAKTIV' )
    ( master_id = '73402851' name = 'David Keller'  category = 'Finanz'   status = 'AKTIV'   )
  ).

    CLEAR mt_detail.
    LOOP AT mt_master INTO ls_master.
      lv_index = sy-tabix.
      DO 12 TIMES.
        lv_amount = 2000 + ( ( lv_index * 217 + sy-index * 131 ) MOD 3001 ).
        CASE sy-index.
          WHEN 1.
            lv_monat = 'Januar'.
          WHEN 2.
            lv_monat = 'Februar'.
          WHEN 3.
            lv_monat = 'März'.
          WHEN 4.
            lv_monat = 'April'.
          WHEN 5.
            lv_monat = 'Mai'.
          WHEN 6.
            lv_monat = 'Juni'.
          WHEN 7.
            lv_monat = 'Juli'.
          WHEN 8.
            lv_monat = 'August'.
          WHEN 9.
            lv_monat = 'September'.
          WHEN 10.
            lv_monat = 'Oktober'.
          WHEN 11.
            lv_monat = 'November'.
          WHEN 12.
            lv_monat = 'Dezember'.
          WHEN OTHERS.
            CLEAR lv_monat.
        ENDCASE.
        APPEND VALUE ty_detail(
  master_id = ls_master-master_id
  item_no   = sy-index
  item_text = |Gehalt Monat { lv_monat } 2026|
  amount    = lv_amount
  currency  = 'EUR' ) TO mt_detail.
      ENDDO.
    ENDLOOP.
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
      WHEN 'BACK' OR 'EXIT' OR 'CANC' OR '&F03'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD build_ui_once.
    mo_main = cl_gui_container=>screen0.
    build_tree( ).
  ENDMETHOD.

  METHOD get_fcat.
    rt_fcat = VALUE lvc_t_fcat(
      ( fieldname = 'ITEM_TEXT' coltext = '' outputlen = 40 )
      ( fieldname = 'AMOUNT'    coltext = 'Betrag'   outputlen = 15 )
      ( fieldname = 'CURRENCY'  coltext = 'Whrg'     outputlen = 5 ) ).
  ENDMETHOD.

  METHOD build_tree.
    DATA: ls_hhdr         TYPE treev_hhdr,
          lt_fcat         TYPE lvc_t_fcat,
          ls_tree_line    TYPE ty_tree_line,
          ls_master       TYPE ty_master,
          ls_detail       TYPE ty_detail,
          lv_master_key   TYPE lvc_nkey,
          lv_detail_key   TYPE lvc_nkey,
          lv_sum_amount   TYPE p LENGTH 10 DECIMALS 2,
          lv_sum_currency TYPE c LENGTH 3.

    DATA: ls_node_layout TYPE lvc_s_layn,
          lv_node_key    TYPE lvc_nkey,
          lv_parent_key  TYPE lvc_nkey.

    lt_fcat = get_fcat( ).

    ls_hhdr-heading = 'Mitarbeiter und Monatsgehälter'.
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
      "Summe bilden
      CLEAR: lv_sum_amount, lv_sum_currency.
      LOOP AT mt_detail INTO ls_detail WHERE master_id = ls_master-master_id.
        lv_sum_amount = lv_sum_amount + ls_detail-amount.
        IF lv_sum_currency IS INITIAL.
          lv_sum_currency = ls_detail-currency.
        ENDIF.
      ENDLOOP.

      CLEAR ls_tree_line.
      ls_tree_line-item_text    = 'Summe'.
      ls_tree_line-amount    = lv_sum_amount.
      ls_tree_line-currency  = lv_sum_currency.
      APPEND ls_tree_line TO mt_tree_outtab.

      CLEAR ls_node_layout.
      ls_node_layout-isfolder = abap_true.
      ls_node_layout-n_image  = icon_employee.
      ls_node_layout-exp_image = icon_employee.


      "Master-Node
      mo_tree->add_node(
        EXPORTING
          i_relat_node_key = ''
          i_relationship   = cl_gui_column_tree=>relat_last_child
          is_node_layout   = ls_node_layout
          i_node_text      = |{ ls_master-master_id } - { ls_master-name } ({ ls_master-category }) ({ ls_master-status })|
          is_outtab_line   = ls_tree_line
        IMPORTING
          e_new_node_key   = lv_master_key ).

      "Detail node
      LOOP AT mt_detail INTO ls_detail WHERE master_id = ls_master-master_id.

        CLEAR ls_node_layout.
        ls_node_layout-isfolder = abap_false.
        ls_node_layout-n_image  = icon_green_light.
        ls_node_layout-exp_image = icon_green_light.
        IF ls_detail-item_no NOT BETWEEN 1 AND 2.
          ls_node_layout-n_image  = icon_yellow_light.
          ls_node_layout-exp_image = icon_yellow_light.
          ls_node_layout-disabled = 'X'.
        ENDIF.

        CLEAR ls_tree_line.
        ls_tree_line-item_text = ''. "ls_detail-item_text.
        ls_tree_line-amount    = ls_detail-amount.
        ls_tree_line-currency  = ls_detail-currency.
        APPEND ls_tree_line TO mt_tree_outtab.

        mo_tree->add_node(
          EXPORTING
            i_relat_node_key = lv_master_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            is_node_layout   = ls_node_layout
            i_node_text      = ls_detail-item_text   "|{ ls_detail-master_id }/{ ls_detail-item_no }|
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
