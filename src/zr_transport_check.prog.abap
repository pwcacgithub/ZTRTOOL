*&---------------------------------------------------------------------*
*& Report  ZR_TRANSPORT_CHECK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT
  zr_transport_check
  NO STANDARD PAGE HEADING
  LINE-SIZE 1023.

INCLUDE zi_transport_list_top.
INCLUDE zi_transport_list_s01.
INCLUDE zi_transport_list_f01.
INCLUDE zi_transport_list_alv.
*&---------------------------------------------------------------------*
*& Data initialization
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM selection_defaults.
  PERFORM get_default_variant USING p_vari.
*&---------------------------------------------------------------------*
*& Data validation
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON p_vari.
  PERFORM check_variant USING p_vari.
*&---------------------------------------------------------------------*
*& Value request help
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant USING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sys_id.
  PERFORM f4_for_system USING p_sys_id.

*&---------------------------------------------------------------------*
*& Data selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM global_data_refresh.
  PERFORM transports_selection.
  IF p_tasks EQ 'X'.
    PERFORM tasks_selection.
    IF p_tsk_ad EQ 'X'.
      PERFORM addl_transport_selection.
    ELSEIF p_tsk_on EQ 'X'.
      PERFORM subs_transport_selection.
    ENDIF.
  ENDIF.

  IF p_obj_ck EQ 'X'.
    PERFORM obj_transport_selection.
  ENDIF.

  IF p_dep_ck EQ 'X'.
    PERFORM dep_transport_selection.
  ENDIF.

*** Begin of change - TR Tool V2
  PERFORM check_object_typ_comp.
*** End of change - TR Tool V2

  PERFORM co_owner_selection.
  PERFORM get_cts_project.
*&---------------------------------------------------------------------*
*& Data processing
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM release_data_selection.

  PERFORM list_display.
