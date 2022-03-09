*&---------------------------------------------------------------------*
*&  Include           ZI_TRANSPORT_LIST_ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& ALV Subroutines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&   Form ADD_FIELDCAT
*&---------------------------------------------------------------------*
* Add field to field catalog for ALV
*----------------------------------------------------------------------*
FORM add_fieldcat USING pv_fnam
             pv_tabnam
             pv_seltext
         CHANGING pt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    lw_afield TYPE slis_fieldcat_alv.

  CLEAR lw_afield.
  lw_afield-tabname   = pv_tabnam.
  lw_afield-fieldname  = pv_fnam.
  lw_afield-seltext_s  = pv_seltext.
  lw_afield-seltext_l  = pv_seltext.
  lw_afield-seltext_m  = pv_seltext.
  lw_afield-reptext_ddic = pv_seltext.

  APPEND lw_afield TO pt_fieldcat.

ENDFORM.          "add_fieldcat

*&---------------------------------------------------------------------*
*&   Form ADD_REF_FIELD
*&---------------------------------------------------------------------*
* Add reference table and field to field catalog for ALV
*----------------------------------------------------------------------*
FORM add_ref_field USING pv_fnam
             pv_tabnam
             pv_ref_tabnam
             pv_ref_fnam
         CHANGING pt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:
    lw_afield TYPE slis_fieldcat_alv.

  CLEAR lw_afield.

  READ TABLE pt_fieldcat INTO lw_afield
    WITH KEY
     tabname = pv_tabnam
     fieldname = pv_fnam.
  IF sy-subrc EQ 0.
    lw_afield-ref_tabname   = pv_ref_tabnam.
    lw_afield-ref_fieldname  = pv_ref_fnam.
    MODIFY pt_fieldcat FROM lw_afield INDEX sy-tabix.
  ELSE.
    lw_afield-tabname   = pv_tabnam.
    lw_afield-fieldname  = pv_fnam.
    lw_afield-ref_tabname   = pv_ref_tabnam.
    lw_afield-ref_fieldname  = pv_ref_fnam.
    APPEND lw_afield TO pt_fieldcat.
  ENDIF.

ENDFORM.          "add_fieldcat
*&---------------------------------------------------------------------*
*&   Form LAYOUT_SETUP
*&---------------------------------------------------------------------*
*    Set up the default layout for the ALV output
*----------------------------------------------------------------------*
FORM layout_setup .

  gw_layout-zebra = 'X'.
  gw_layout-colwidth_optimize = 'X'.

ENDFORM.          " LAYOUT_SETUP
*&---------------------------------------------------------------------*
*&   Form FIELDCAT_SETUP
*&---------------------------------------------------------------------*
*    Set up the default field catalog for the ALV output
*----------------------------------------------------------------------*
FORM fieldcat_setup .

  PERFORM add_fieldcat USING:
    'TRANSPORT' 'GT_TRANSPORT_LIST' 'Transport'
     CHANGING gt_fieldcat,
    'PROJECT' 'GT_TRANSPORT_LIST' 'Project'
     CHANGING gt_fieldcat,
    'STATUS'  'GT_TRANSPORT_LIST' 'Status'
     CHANGING gt_fieldcat,
    'TARGET'  'GT_TRANSPORT_LIST' 'Target'
     CHANGING gt_fieldcat,
    'CATEGORY' 'GT_TRANSPORT_LIST' 'Category'
     CHANGING gt_fieldcat,
    'CLIENT'  'GT_TRANSPORT_LIST' 'Client'
     CHANGING gt_fieldcat,
    'OWNER'   'GT_TRANSPORT_LIST' 'Owner'
     CHANGING gt_fieldcat,
    'CO_OWNER' 'GT_TRANSPORT_LIST' 'Developer'
     CHANGING gt_fieldcat,
    'DESC'   'GT_TRANSPORT_LIST' 'Description'
     CHANGING gt_fieldcat,
*** Begin of change - TR Tool V2
    'OBJ_TYP'   'GT_TRANSPORT_LIST' 'Object Type'
     CHANGING gt_fieldcat,
    'COMP_TYP'   'GT_TRANSPORT_LIST' 'Complexity'
     CHANGING gt_fieldcat,
*** End of change - TR Tool V2
    'ABD_STAT' 'GT_TRANSPORT_LIST' 'Abnd?'
     CHANGING gt_fieldcat,
    'DEP_STAT' 'GT_TRANSPORT_LIST' 'Dep?'
     CHANGING gt_fieldcat,
    'DEV_STAT' 'GT_TRANSPORT_LIST' 'DEV Status'
     CHANGING gt_fieldcat,
*    'QA_STAT'  'GT_TRANSPORT_LIST' 'SIT Status'
*     CHANGING gt_fieldcat,
    'QA2_STAT'  'GT_TRANSPORT_LIST' 'QA Status'
     CHANGING gt_fieldcat,
*    'QA3_STAT'  'GT_TRANSPORT_LIST' 'PAT Status'
*     CHANGING gt_fieldcat,
    'PRD_STAT' 'GT_TRANSPORT_LIST' 'PRD Status'
     CHANGING gt_fieldcat,
    'CHG_DATE' 'GT_TRANSPORT_LIST' 'Changed Date'
     CHANGING gt_fieldcat,
    'CHG_TIME' 'GT_TRANSPORT_LIST' 'Changed Time'
     CHANGING gt_fieldcat,
    'REL_DATE' 'GT_TRANSPORT_LIST' 'Release Date'
     CHANGING gt_fieldcat,
    'REL_TIME' 'GT_TRANSPORT_LIST' 'Release Time'
     CHANGING gt_fieldcat,
    'REL_RC'  'GT_TRANSPORT_LIST' 'Release Code'
     CHANGING gt_fieldcat,
*    'QA_DATE'  'GT_TRANSPORT_LIST' 'SIT Date'
*     CHANGING gt_fieldcat,
*    'QA_TIME'  'GT_TRANSPORT_LIST' 'SIT Time'
*     CHANGING gt_fieldcat,
*    'QA_RC'   'GT_TRANSPORT_LIST' 'SIT Code'
*     CHANGING gt_fieldcat,
    'QA2_DATE'  'GT_TRANSPORT_LIST' 'QA Date'
     CHANGING gt_fieldcat,
    'QA2_TIME'  'GT_TRANSPORT_LIST' 'QA Time'
     CHANGING gt_fieldcat,
    'QA2_RC'   'GT_TRANSPORT_LIST' 'QA Code'
     CHANGING gt_fieldcat,
*    'QA3_DATE'  'GT_TRANSPORT_LIST' 'PAT Date'
*     CHANGING gt_fieldcat,
*    'QA3_TIME'  'GT_TRANSPORT_LIST' 'PAT Time'
*     CHANGING gt_fieldcat,
*    'QA3_RC'   'GT_TRANSPORT_LIST' 'PAT Code'
*     CHANGING gt_fieldcat,
    'PRD_DATE' 'GT_TRANSPORT_LIST' 'PRD Date'
     CHANGING gt_fieldcat,
    'PRD_TIME' 'GT_TRANSPORT_LIST' 'PRD Time'
     CHANGING gt_fieldcat,
    'PRD_RC'  'GT_TRANSPORT_LIST' 'PRD Code'
     CHANGING gt_fieldcat.

  PERFORM add_ref_field USING:
    'TRANSPORT' 'GT_TRANSPORT_LIST' 'E070' 'TRKORR'
     CHANGING gt_fieldcat,
    'STATUS' 'GT_TRANSPORT_LIST' 'E070' 'TRSTATUS'
     CHANGING gt_fieldcat,
    'TARGET'  'GT_TRANSPORT_LIST' 'E070' 'TARSYSTEM'
     CHANGING gt_fieldcat,
    'CATEGORY' 'GT_TRANSPORT_LIST' 'E070' 'KORRDEV'
     CHANGING gt_fieldcat,
    'CLIENT'  'GT_TRANSPORT_LIST' 'E070C' 'CLIENT'
     CHANGING gt_fieldcat,
    'OWNER'   'GT_TRANSPORT_LIST' 'E070' 'AS4USER'
     CHANGING gt_fieldcat,
    'CO_OWNER' 'GT_TRANSPORT_LIST' 'E070' 'AS4USER'
     CHANGING gt_fieldcat,
    'CHG_DATE' 'GT_TRANSPORT_LIST' 'E070' 'AS4DATE'
     CHANGING gt_fieldcat,
    'CHG_TIME' 'GT_TRANSPORT_LIST' 'E070' 'AS4TIME'
     CHANGING gt_fieldcat,
    'DESC'   'GT_TRANSPORT_LIST' 'E07T' 'AS4TEXT'
     CHANGING gt_fieldcat.


ENDFORM.          " FIELDCAT_SETUP
*&---------------------------------------------------------------------*
*&   Form ALV_DISPLAY
*&---------------------------------------------------------------------*
*    Call the ALV display
*----------------------------------------------------------------------*
FORM alv_display .

  DATA: lw_variant TYPE disvariant.

  lw_variant-report = 'ZBC_TRANSPORT_LIST_MAIN'.
  lw_variant-variant = p_vari.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      it_fieldcat              = gt_fieldcat
      is_layout                = gw_layout
      i_callback_program       = sy-repid
      i_callback_user_command  = 'ALV_CALLBACK_USER_COMMAND'
      is_variant               = lw_variant
      i_save                   = 'X'
    TABLES
      t_outtab                 = gt_transport_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.          " ALV_DISPLAY

*&---------------------------------------------------------------------*
*&   Form SUB_ALV_CALLBACK_USER_COMMAND
*&---------------------------------------------------------------------*
*    ALV feedback
*----------------------------------------------------------------------*
*   -->PV_UCOMM   user command
*   -->PW_SELFIELD  field selected
*----------------------------------------------------------------------*
FORM alv_callback_user_command USING pv_ucomm LIKE sy-ucomm
              pw_selfield TYPE slis_selfield.

  CASE pv_ucomm.
*  Double-click - display trasnsport
    WHEN '&IC1'.
      READ TABLE gt_transport_list
       INTO gw_transport
       INDEX pw_selfield-tabindex.
      IF sy-subrc EQ 0.
        PERFORM transport_display
         USING gw_transport-transport.
      ENDIF.
    WHEN '&ZNAV'.
      IF pw_selfield-fieldname =  'TRANSPORT'.
*** Begin of Change
*      SUBMIT ZNM_OBJS_DEP_TRANS WITH r_obj = space
*                                WITH r_tr = 'X'
*                                WITH p_tr = pw_selfield-value
*                                AND RETURN.
        IF p_sys_id IS NOT INITIAL.
          DATA(lv_rfc) = 'X'.
          DATA(lv_sys_id) = p_sys_id.
        ELSE.
          CLEAR lv_rfc.
          CLEAR lv_sys_id.
        ENDIF.
        SUBMIT zr_obj_dep_trans   WITH r_obj = space
                                  WITH r_tr = 'X'
                                  WITH p_tr = pw_selfield-value
                                  WITH p_rfc = lv_rfc
                                  WITH p_rfc_d = lv_sys_id
                                  AND RETURN.
*** End of Change
      ENDIF.
  ENDCASE.

ENDFORM.          " ALV_CALLBACK_USER_COMMAND

*&---------------------------------------------------------------------*
*&   Form f4_for_variant
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING p_vari TYPE disvariant-variant.

  DATA: v_exit(1) TYPE c.
  DATA: wa_variant TYPE disvariant.
  DATA: v_save(1) TYPE c VALUE 'X'.

  wa_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = wa_variant
      i_save     = v_save
    IMPORTING
      e_exit     = v_exit
      es_variant = wa_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'   NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF v_exit = space.
      p_vari = wa_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.          " f4_for_variant

*&---------------------------------------------------------------------
*
*&   Form check_variant
*&---------------------------------------------------------------------
*
FORM check_variant USING p_vari TYPE disvariant-variant.

  DATA: wa_variant TYPE disvariant.
  DATA: v_save(1) TYPE c VALUE 'X'.

  wa_variant-report = sy-repid.

  IF NOT p_vari IS INITIAL.
    MOVE p_vari TO wa_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = v_save
      CHANGING
        cs_variant    = wa_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.          " check_variant
*&---------------------------------------------------------------------*
*&   Form GET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_default_variant CHANGING p_vari TYPE disvariant-variant.

  DATA: wa_variant TYPE disvariant.
  DATA: v_save(1) TYPE c VALUE 'X'.

  wa_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = v_save
    CHANGING
      cs_variant    = wa_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc EQ '0'.
    p_vari = wa_variant-variant.
  ENDIF.

ENDFORM.          " GET_DEFAULT_VARIANT

FORM sub_pf_status USING rt_extab TYPE slis_t_extab.

*--Set the Modified PF status for the ALV.
  SET PF-STATUS 'STATUS' EXCLUDING rt_extab.

ENDFORM.

FORM f4_for_system CHANGING p_sys_id TYPE tmssysnam.

  CALL FUNCTION 'TMS_UI_F4_SYSTEMS'
    CHANGING
      cv_system = p_sys_id.

ENDFORM.
