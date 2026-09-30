REPORT zhr_bonus_factor_test.

DATA gv_awart TYPE p2001-awart.

PARAMETERS:
  p_pernr TYPE pernr_d DEFAULT '00050209' OBLIGATORY,
  p_begda TYPE begda DEFAULT '20260101' OBLIGATORY,
  p_endda TYPE endda DEFAULT '20261231' OBLIGATORY.

" Leer: alle ueber T554C-REF01 ermittelten unbezahlten Arten.
" Gefuellt: zusaetzliche Einschraenkung, keine Bewertungsuebersteuerung.
SELECT-OPTIONS s_awart FOR gv_awart.

PARAMETERS:
  p_month RADIOBUTTON GROUP view DEFAULT 'X',
  p_day   RADIOBUTTON GROUP view.

AT SELECTION-SCREEN.
  IF p_begda > p_endda OR p_begda(4) <> p_endda(4).
    MESSAGE 'Beginn und Ende muessen geordnet im selben Jahr liegen'
      TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  DATA:
    lt_awart   TYPE zcl_hr_bonus_factor=>ty_t_awart,
    lt_months  TYPE zcl_hr_bonus_factor=>ty_t_month,
    lt_days    TYPE zcl_hr_bonus_factor=>ty_t_day,
    lt_out_mon TYPE STANDARD TABLE OF zcl_hr_bonus_factor=>ty_month,
    lt_out_day TYPE STANDARD TABLE OF zcl_hr_bonus_factor=>ty_day,
    lo_alv     TYPE REF TO cl_salv_table.

  " Intervalle, Einzelwerte und Ausschluesse gegen Customizing aufloesen.
  " DISTINCT vermeidet doppelte Schluessel verschiedener Gruppierungen.
  IF s_awart[] IS NOT INITIAL.
    SELECT DISTINCT subty
      FROM t554s
      WHERE subty IN @s_awart
      INTO TABLE @lt_awart.

    IF lt_awart IS INITIAL.
      MESSAGE 'Keine Abwesenheitsart passt zur Selektion in T554S'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  zcl_hr_bonus_factor=>get_factors(
    EXPORTING
      iv_pernr        = p_pernr
      iv_begda        = p_begda
      iv_endda        = p_endda
      it_unpaid_awart = lt_awart
      iv_modif        = '01'
    IMPORTING
      et_months       = lt_months
      et_days         = lt_days
    EXCEPTIONS
      invalid_input   = 1
      infotype_error  = 2
      schedule_error  = 3
      customizing_error = 4
      OTHERS          = 5 ).

  CASE sy-subrc.
    WHEN 1.
      MESSAGE 'Ungueltige Personalnummer, Datumsgrenzen oder Abwesenheitsarten'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    WHEN 2.
      MESSAGE 'Fehler beim Lesen der Infotypen; leere Infotypen sind erlaubt'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    WHEN 3.
      MESSAGE 'Arbeitszeitplan unvollstaendig oder SAP-Planerzeugung fehlerhaft'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    WHEN 4.
      MESSAGE 'Organisatorische Zuordnung fehlt im Customizing T001P'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    WHEN 5.
      MESSAGE 'Unerwarteter Fehler bei der Faktorberechnung'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDCASE.

  TRY.
      IF p_month = abap_true.
        lt_out_mon = lt_months.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING  t_table      = lt_out_mon ).
      ELSE.
        lt_out_day = lt_days.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING  t_table      = lt_out_day ).
      ENDIF.

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->get_display_settings( )->set_list_header(
        |Pernr { p_pernr }: { p_begda DATE = USER } - { p_endda DATE = USER }| ).

      IF p_month = abap_true.
        DATA(lo_column) = lo_alv->get_columns( )->get_column( 'MONAT' ).
        lo_column->set_long_text( 'Monat (JJJJMM)' ).
        lo_column->set_medium_text( 'Monat (JJJJMM)' ).
        lo_column->set_short_text( 'Monat' ).

        lo_column = lo_alv->get_columns( )->get_column( 'SOLLTAGE' ).
        lo_column->set_long_text( 'Soll-Arbeitstage' ).
        lo_column->set_medium_text( 'Soll-Arbeitstage' ).
        lo_column->set_short_text( 'Solltage' ).

        lo_column = lo_alv->get_columns( )->get_column( 'UNBEZAHLTE_TAGE' ).
        lo_column->set_long_text( 'Unbezahlte ganze Arbeitstage' ).
        lo_column->set_medium_text( 'Unbezahlte Tage' ).
        lo_column->set_short_text( 'Unbezahlt' ).

        lo_column = lo_alv->get_columns( )->get_column( 'ANRECHENBAR' ).
        lo_column->set_long_text( 'Anrechenbare Arbeitstage' ).
        lo_column->set_medium_text( 'Anrechenbare Tage' ).
        lo_column->set_short_text( 'Anrechenb.' ).

        lo_column = lo_alv->get_columns( )->get_column( 'FAKTOR' ).
        lo_column->set_long_text( 'Auszahlungsfaktor (0 bis 1)' ).
        lo_column->set_medium_text( 'Auszahlungsfaktor' ).
        lo_column->set_short_text( 'Faktor' ).

        lo_column = lo_alv->get_columns( )->get_column( 'FAKTOR_GUELTIG' ).
        lo_column->set_long_text( 'Faktor gueltig (leer: keine Solltage)' ).
        lo_column->set_medium_text( 'Faktor gueltig' ).
        lo_column->set_short_text( 'Gueltig' ).
      ENDIF.

      lo_alv->display( ).
    CATCH cx_salv_msg cx_salv_not_found INTO DATA(lx_alv).
      DATA(lv_message) = lx_alv->get_text( ).
      MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
