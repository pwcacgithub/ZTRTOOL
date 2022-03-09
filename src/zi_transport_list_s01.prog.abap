*&---------------------------------------------------------------------*
*&  Include           ZI_TRANSPORT_LIST_S01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

SELECT-OPTIONS:
s_trans FOR e070-trkorr,           "Transport Request/Tasks
s_status FOR e070-trstatus,        "TR Status
s_target FOR e070-tarsystem,       "TR Target
s_catgry FOR e070-korrdev,         "TR Category
s_client FOR e070c-client,         "TR Src Cleint
s_owner FOR e070-as4user,          "TR Owner
s_chg_dt FOR e070-as4date,         "TR Change Date
s_desc  FOR e07t-as4text.          "TR Description

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.

PARAMETERS:
  p_tasks  AS CHECKBOX DEFAULT ' ',              "Include Tasks
  p_tsk_ad RADIOBUTTON GROUP task DEFAULT 'X',  "
  p_tsk_on RADIOBUTTON GROUP task.

SELECT-OPTIONS:
s_tasks FOR e070-trkorr,
s_stat2 FOR e070-trstatus,
s_owner2 FOR e070-as4user,
s_chg_d2 FOR e070-as4date.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-b04.

PARAMETERS:
p_obj_ck AS CHECKBOX DEFAULT ' '.

SELECT-OPTIONS:
s_pgmid FOR e071-pgmid,
s_object FOR e071-object,
s_objnam FOR e071-obj_name,
s_tabkey FOR e071k-tabkey.

SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.

PARAMETERS:
p_dep_ck AS CHECKBOX DEFAULT ' '.
SELECT-OPTIONS:
  s_trn_dp FOR e070-trkorr,
  s_sta_dp FOR e070-trstatus,
  s_cdt_dt FOR e070-as4date.
SELECTION-SCREEN END OF BLOCK b05.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.

PARAMETERS:
* Dev system
  p_dev    TYPE ctslg_system-systemid OBLIGATORY,
*  p_qa     TYPE ctslg_system-systemid OBLIGATORY,
*  p_qa_mn  TYPE ctslg_step-clientid,
* QA system
  p_qa2    TYPE ctslg_system-systemid OBLIGATORY,
  p_qa2_mn TYPE ctslg_step-clientid,
* Pre-prod system
*p_qa3   type ctslg_system-systemid obligatory,
*p_qa3_mn type ctslg_step-clientid,
* Prod system
  p_prd    TYPE ctslg_system-systemid OBLIGATORY,
  p_prd_mn TYPE ctslg_step-clientid.

SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b98 WITH FRAME TITLE TEXT-b98.
PARAMETERS:
p_sys_id TYPE tmssysnam.  "System Name
SELECTION-SCREEN END OF BLOCK b98.

SELECTION-SCREEN BEGIN OF BLOCK b99 WITH FRAME TITLE TEXT-b99.
PARAMETERS:
p_vari  TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b99.
