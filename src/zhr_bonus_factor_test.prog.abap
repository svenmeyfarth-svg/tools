REPORT zhr_bonus_factor_test.

DATA: gv_pernr TYPE pernr_d,
      gv_awart TYPE p2001-awart.

SELECT-OPTIONS s_pernr FOR gv_pernr DEFAULT '00050209' OBLIGATORY.
PARAMETERS:
  p_begda TYPE begda DEFAULT '20260101' OBLIGATORY,
  p_endda TYPE endda DEFAULT '20261231' OBLIGATORY.

" Leer: alle unbezahlten Arten laut T554C-REF01.
" Zeiten ausserhalb der Beschaeftigung werden immer gekuerzt.
SELECT-OPTIONS s_awart FOR gv_awart.
PARAMETERS:
  p_month RADIOBUTTON GROUP view DEFAULT 'X',
  p_day   RADIOBUTTON GROUP view,
  p_abs   RADIOBUTTON GROUP view.

TYPES: BEGIN OF ty_month_row.
TYPES pernr TYPE pernr_d.
INCLUDE TYPE zcl_hr_bonus_factor=>ty_month.
TYPES meldung TYPE c LENGTH 120.
TYPES END OF ty_month_row.

TYPES: BEGIN OF ty_day_row.
TYPES pernr TYPE pernr_d.
INCLUDE TYPE zcl_hr_bonus_factor=>ty_day.
TYPES meldung TYPE c LENGTH 120.
TYPES END OF ty_day_row.

TYPES: BEGIN OF ty_abs_row.
TYPES pernr TYPE pernr_d.
INCLUDE TYPE zcl_hr_bonus_factor=>ty_absence.
TYPES meldung TYPE c LENGTH 120.
TYPES END OF ty_abs_row.

TYPES:
  BEGIN OF ty_label,
    name TYPE lvc_fname,
    short TYPE scrtext_s,
    medium TYPE scrtext_m,
    long TYPE scrtext_l,
  END OF ty_label,
  ty_t_labels TYPE STANDARD TABLE OF ty_label WITH DEFAULT KEY.

AT SELECTION-SCREEN.
  IF p_begda > p_endda OR p_begda(4) <> p_endda(4).
    MESSAGE 'Beginn und Ende muessen geordnet im selben Jahr liegen' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  DATA:
    lt_awart TYPE zcl_hr_bonus_factor=>ty_t_awart,
    lt_months TYPE zcl_hr_bonus_factor=>ty_t_month,
    lt_days TYPE zcl_hr_bonus_factor=>ty_t_day,
    lt_absences TYPE zcl_hr_bonus_factor=>ty_t_absence,
    lt_out_abs TYPE STANDARD TABLE OF ty_abs_row,
    ls_out_abs TYPE ty_abs_row,
    lt_out_mon TYPE STANDARD TABLE OF ty_month_row,
    lt_out_day TYPE STANDARD TABLE OF ty_day_row,
    ls_out_mon TYPE ty_month_row,
    ls_out_day TYPE ty_day_row,
    lv_message TYPE c LENGTH 120,
    lo_alv TYPE REF TO cl_salv_table.

  IF s_awart[] IS NOT INITIAL.
    SELECT DISTINCT subty FROM t554s
      WHERE subty IN @s_awart INTO TABLE @lt_awart.
    IF lt_awart IS INITIAL.
      MESSAGE 'Keine Abwesenheitsart passt zur Selektion in T554S'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  " Nur Kandidaten ermitteln. Infotypdaten liest die Klasse ueber HR-API.
  " Keine Datumseinschraenkung: auch vor Eintritt/nach Austritt auswerten.
  SELECT DISTINCT pernr FROM pa0000
    WHERE pernr IN @s_pernr
    INTO TABLE @DATA(lt_pernr).
  SORT lt_pernr BY pernr.
  IF lt_pernr IS INITIAL.
    MESSAGE 'Keine Personalnummer passt zur Auswahl'
      TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT lt_pernr INTO DATA(ls_pernr).
    CLEAR: lt_months, lt_days, lt_absences, lv_message,
           ls_out_mon, ls_out_day, ls_out_abs.
    zcl_hr_bonus_factor=>get_factors(
      EXPORTING
        iv_pernr          = ls_pernr-pernr
        iv_begda          = p_begda
        iv_endda          = p_endda
        it_unpaid_awart   = lt_awart
        iv_modif          = '01'
      IMPORTING
        et_months         = lt_months
        et_days           = lt_days
        et_absences       = lt_absences
      EXCEPTIONS
        invalid_input     = 1
        infotype_error    = 2
        schedule_error    = 3
        customizing_error = 4
        OTHERS            = 5 ).

    CASE sy-subrc.
      WHEN 1.
        lv_message = 'Ungueltige Eingabe'.
      WHEN 2.
        lv_message = 'Infotyp-Lesefehler oder kein belegtes Arbeitsverhaeltnis in IT0000'.
      WHEN 3.
        lv_message = 'Arbeitszeitplan nicht erzeugbar; insbesondere IT0001/0007 und Planpruefung beachten'.
      WHEN 4.
        lv_message = 'Organisatorische Zuordnung fehlt in T001P'.
      WHEN 5.
        lv_message = 'Unerwarteter Fehler bei der Faktorberechnung'.
    ENDCASE.

    IF lv_message IS NOT INITIAL.
      " Fehler nur fuer diese Person; die weiteren Personen weiterbearbeiten.
      ls_out_mon-pernr = ls_pernr-pernr.
      ls_out_mon-meldung = lv_message.
      APPEND ls_out_mon TO lt_out_mon.
      ls_out_day-pernr = ls_pernr-pernr.
      ls_out_day-meldung = lv_message.
      APPEND ls_out_day TO lt_out_day.
      ls_out_abs-pernr = ls_pernr-pernr.
      ls_out_abs-meldung = lv_message.
      APPEND ls_out_abs TO lt_out_abs.
      CONTINUE.
    ENDIF.

    LOOP AT lt_absences INTO DATA(ls_absence).
      ls_out_abs = CORRESPONDING #( ls_absence ).
      ls_out_abs-pernr = ls_pernr-pernr.
      APPEND ls_out_abs TO lt_out_abs.
    ENDLOOP.
    IF lt_absences IS INITIAL.
      CLEAR ls_out_abs.
      ls_out_abs-pernr = ls_pernr-pernr.
      ls_out_abs-meldung = 'Keine beruecksichtigte Abwesenheit in einem gueltigen Monat mit Faktor < 1'.
      APPEND ls_out_abs TO lt_out_abs.
    ENDIF.
    LOOP AT lt_months INTO DATA(ls_month).
      ls_out_mon = CORRESPONDING #( ls_month ).
      ls_out_mon-pernr = ls_pernr-pernr.
      IF ls_month-bsgrd_fehlende_tage > 0.
        ls_out_mon-meldung = 'Kein Faktor: IT0008 fehlt oder enthaelt widerspruechliche Beschaeftigungsgrade'.
      ELSEIF ls_month-faktor_gueltig = abap_false.
        ls_out_mon-meldung = 'Kein Faktor: keine Soll-Arbeitstage bei bestehender Beschaeftigung'.
      ENDIF.
      APPEND ls_out_mon TO lt_out_mon.
    ENDLOOP.
    LOOP AT lt_days INTO DATA(ls_day).
      ls_out_day = CORRESPONDING #( ls_day ).
      ls_out_day-pernr = ls_pernr-pernr.
      IF ls_day-beschaeftigt = abap_true AND ls_day-bsgrd_gueltig = abap_false.
        ls_out_day-meldung = 'Kein eindeutiger gueltiger Beschaeftigungsgrad aus IT0008'.
      ENDIF.
      APPEND ls_out_day TO lt_out_day.
    ENDLOOP.
  ENDLOOP.

  TRY.
      IF p_month = abap_true.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING t_table = lt_out_mon ).
      ELSEIF p_day = abap_true.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING t_table = lt_out_day ).
      ELSE.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv
          CHANGING t_table = lt_out_abs ).
      ENDIF.
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->get_display_settings( )->set_list_header(
        |Einmalzahlung: { p_begda DATE = USER } - { p_endda DATE = USER }| ).

      DATA(lt_labels) = VALUE ty_t_labels(
        ( name = 'PERNR' short = 'Pers.-Nr.' medium = 'Personalnummer' long = 'Personalnummer' )
        ( name = 'MONAT' short = 'Monat' medium = 'Monat (JJJJMM)' long = 'Monat (JJJJMM)' )
        ( name = 'SOLLTAGE' short = 'Solltage' medium = 'Soll-Arbeitstage' long = 'Soll-Arbeitstage im Auswertungszeitraum' )
        ( name = 'UNBEZAHLTE_TAGE' short = 'Unbezahlt' medium = 'Unbezahlt gesamt' long = 'Unbezahlte Arbeitstage insgesamt' )
        ( name = 'AUSSERHALB_TAGE' short = 'Ausserhalb' medium = 'Ausserhalb Vertrag' long = 'Davon ausserhalb der Beschaeftigung' )
        ( name = 'ABWESENHEIT_TAGE' short = 'Abwesenh.' medium = 'Unbezahlte Abw.' long = 'Davon unbezahlte ganzt. Abwesenheiten' )
        ( name = 'BESCHAEFTIGUNGSTAGE' short = 'Kal.-Tage' medium = 'Kal.-Tage im Vertrag' long = 'Kalendertage mit Beschaeftigung' )
        ( name = 'ANRECHENBAR' short = 'Anrechenb.' medium = 'Anrechenbare Tage' long = 'Anrechenbare Arbeitstage' )
        ( name = 'FAKTOR' short = 'Faktor' medium = 'Auszahlungsfaktor' long = 'Auszahlungsfaktor inkl. Beschaeft.grad' )
        ( name = 'FAKTOR_GUELTIG' short = 'Gueltig' medium = 'Faktor gueltig' long = 'Faktor gueltig (X = ja)' )
        ( name = 'DATUM' short = 'Datum' medium = 'Datum' long = 'Kalendertag' )
        ( name = 'TPROG' short = 'Tagesplan' medium = 'Tagesarbeitszeitplan' long = 'Tagesarbeitszeitplan' )
        ( name = 'SOLLSTUNDEN' short = 'Sollstd.' medium = 'Sollstunden' long = 'Geplante Arbeitsstunden' )
        ( name = 'SOLLTAG' short = 'Solltag' medium = 'Soll-Arbeitstag' long = 'Soll-Arbeitstag (1 = ja)' )
        ( name = 'UNBEZAHLT_TAG' short = 'Unbezahlt' medium = 'Unbezahlter Tag' long = 'Unbezahlter Arbeitstag (1 = ja)' )
        ( name = 'BESCHAEFTIGT' short = 'Im Vertrag' medium = 'Beschaeftigt' long = 'In Beschaeftigung (X = ja)' )
        ( name = 'AUSSERHALB_TAG' short = 'Ausserhalb' medium = 'Ausserhalb Vertrag' long = 'Arbeitstag ausserhalb Beschaeftigung' )
        ( name = 'ABWESENHEIT_TAG' short = 'Abwesenh.' medium = 'Unbezahlte Abw.' long = 'Arbeitstag mit unbezahlter Abwesenheit' )
        ( name = 'BSGRD' short = 'Beschgr.%' medium = 'Beschaeft.grad %' long = 'Beschaeftigungsgrad IT0008 in Prozent' )
        ( name = 'BSGRD_GUELTIG' short = 'Grad OK' medium = 'Beschaeft.grad OK' long = 'Gueltiger IT0008-Beschaeftigungsgrad' )
        ( name = 'GEWICHTET' short = 'Gew.Tage' medium = 'Gewichtete Tage' long = 'Mit Beschaeftigungsgrad gewichtete Tage' )
        ( name = 'BSGRD_DURCHSCHNITT' short = 'Durchs.%' medium = 'Durchschn. Grad %' long = 'Durchschnitt % anrechenbarer Arbeitstage' )
        ( name = 'BSGRD_FEHLENDE_TAGE' short = 'IT8 fehlt' medium = 'Tage ohne IT8-Grad' long = 'Anrechenbare Tage ohne IT0008-Grad' )
        ( name = 'AWART' short = 'Abw.-Art' medium = 'Abwesenheitsarten' long = 'Unbezahlte Abwesenheitsarten' )
        ( name = 'ATEXT' short = 'Abw.-Text' medium = 'Abwesenheitstexte' long = 'Texte der unbezahlten Abwesenheitsarten' )
        ( name = 'BEGDA' short = 'Beginn' medium = 'Abwesenheit Beginn' long = 'Urspruenglicher Beginn der Abwesenheit' )
        ( name = 'ENDDA' short = 'Ende' medium = 'Abwesenheit Ende' long = 'Urspruengliches Ende der Abwesenheit' )
        ( name = 'BEWERTET_VON' short = 'Erster Tag' medium = 'Erster Kuerzungstag' long = 'Erster beruecksichtigter Arbeitstag' )
        ( name = 'BEWERTET_BIS' short = 'Letz. Tag' medium = 'Letzter Kuerzungstag' long = 'Letzter beruecksichtigter Arbeitstag' )
        ( name = 'TAGE' short = 'Tage/Abw.' medium = 'Arbeitstage je Abw.' long = 'Tage je Abwesenheit, nicht summieren' )
        ( name = 'MELDUNG' short = 'Hinweis' medium = 'Hinweis / Fehler' long = 'Hinweis / Fehler fuer diese Person' ) ).

      LOOP AT lt_labels INTO DATA(ls_label).
        TRY.
            DATA(lo_column) = lo_alv->get_columns( )->get_column( ls_label-name ).
            lo_column->set_short_text( ls_label-short ).
            lo_column->set_medium_text( ls_label-medium ).
            lo_column->set_long_text( ls_label-long ).
          CATCH cx_salv_not_found.
            " Spalte gehoert zur jeweils anderen Ausgabeansicht.
        ENDTRY.
      ENDLOOP.
      IF p_abs = abap_true.
        lo_alv->get_columns( )->get_column( 'OBJPS' )->set_technical( abap_true ).
        lo_alv->get_columns( )->get_column( 'SEQNR' )->set_technical( abap_true ).
      ENDIF.
      lo_alv->display( ).
    CATCH cx_salv_msg cx_salv_not_found INTO DATA(lx_alv).
      DATA(lv_alv_message) = lx_alv->get_text( ).
      MESSAGE lv_alv_message TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
