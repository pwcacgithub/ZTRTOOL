*&---------------------------------------------------------------------*
*&  Include           ZI_TRANSPORT_LIST_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Global data declaration
*&---------------------------------------------------------------------*
TYPE-POOLS:
slis,
ctslg.

TABLES:
  e070,
  e070c,
  e07t,
  e071k,
  e071.

TYPES:
  BEGIN OF gty_transport,
    transport  LIKE e070-trkorr,
    trfunction TYPE trfunction,
    project    LIKE e070a-reference,
    status     LIKE e070-trstatus,
    target     LIKE e070-tarsystem,
    category   LIKE e070-korrdev,
    client     LIKE e070c-client,
    owner      LIKE e070-as4user,
    co_owner   LIKE e070-as4user,
    chg_date   LIKE e070-as4date,
    chg_time   LIKE e070-as4time,
    desc       LIKE e07t-as4text,
    obj_typ    LIKE e07t-as4text,
    comp_typ   LIKE e07t-as4text,
    abd_stat   LIKE icon-id,
    dep_stat   LIKE icon-id,
    dev_stat   LIKE icon-id,
*    qa_stat    LIKE icon-id,
    qa2_stat   LIKE icon-id,
*    qa3_stat   LIKE icon-id,
    prd_stat   LIKE icon-id,
    dev_ind    TYPE i,
*    qa_ind     TYPE i,
    qa2_ind    TYPE i,
*    qa3_ind    TYPE i,
    prd_ind    TYPE i,
    rel_date   LIKE tstrfcofil-trdate,
    rel_time   LIKE tstrfcofil-trtime,
    rel_rc     LIKE tstrfcofil-retcode,
*    qa_date    LIKE tstrfcofil-trdate,
*    qa_time    LIKE tstrfcofil-trtime,
*    qa_rc      LIKE tstrfcofil-retcode,
    qa2_rc     LIKE tstrfcofil-retcode,
    qa2_date   LIKE tstrfcofil-trdate,
    qa2_time   LIKE tstrfcofil-trtime,
*    qa3_date   LIKE tstrfcofil-trdate,
*    qa3_time   LIKE tstrfcofil-trtime,
*    qa3_rc     LIKE tstrfcofil-retcode,
    prd_date   LIKE tstrfcofil-trdate,
    prd_time   LIKE tstrfcofil-trtime,
    prd_rc     LIKE tstrfcofil-retcode,
    deep       TYPE i,
  END OF gty_transport,

  BEGIN OF gty_task,
    transport LIKE e070-strkorr,
    pgmid     LIKE e071-pgmid,
    object    LIKE e071-object,
    obj_name  LIKE e071-obj_name,
    deep      TYPE i,
  END OF gty_task,

  BEGIN OF gty_obj,
    pgmid    LIKE e071-pgmid,
    object   LIKE e071-object,
    obj_name LIKE e071-obj_name,
    deep     TYPE i,
  END OF gty_obj,

  BEGIN OF gty_obj_tab,
    pgmid    LIKE e071k-pgmid,
    object   LIKE e071k-object,
    obj_name LIKE e071k-objname,
    tabkey   LIKE e071k-tabkey,
  END OF gty_obj_tab.
*&---------------------------------------------------------------------*
*& Global constants
*&---------------------------------------------------------------------*
CONSTANTS:
  gc_icon_imp_success LIKE icon-id VALUE '@08@',
  gc_icon_imp_warning LIKE icon-id VALUE '@09@',
  gc_icon_imp_error   LIKE icon-id VALUE '@0A@',
  gc_icon_no_logdata  LIKE icon-id VALUE '@00@',
  gc_icon_abandoned   LIKE icon-id VALUE '@8N@',
  gc_icon_edit        LIKE icon-id VALUE '@0Z@',
  gc_icon_ready       LIKE icon-id VALUE '@DF@',
  gc_icon_related     LIKE icon-id VALUE '@EH@'.

*&---------------------------------------------------------------------*
*& Global data declaration
*&---------------------------------------------------------------------*

DATA:
  gw_transport         TYPE gty_transport,
  gt_transport_list    TYPE TABLE OF gty_transport,
  gw_transport_sub     TYPE gty_transport,
  gt_transport_sublist TYPE TABLE OF gty_transport,
  gw_task              TYPE gty_task,
  gt_task_list         TYPE TABLE OF gty_task,
  gt_transport_obj     TYPE TABLE OF gty_task,
  gt_transport_dep     TYPE TABLE OF gty_task,
  gt_objects           TYPE TABLE OF gty_obj,
  gt_objects_drv       TYPE TABLE OF gty_obj,
  gw_objects           TYPE gty_obj,
  gw_objects_drv       TYPE gty_obj,
  gt_tab_objects       TYPE TABLE OF gty_obj_tab,
* ALV data
  gt_fieldcat          TYPE slis_t_fieldcat_alv,
  gw_layout            TYPE slis_layout_alv.
