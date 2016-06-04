*&---------------------------------------------------------------------*
*& Report  ZRMM007
*&
*&---------------------------------------------------------------------*
*& 外购件发料单打印
*&
*&---------------------------------------------------------------------*

REPORT  zmm220.
TYPE-POOLS: slis.

TABLES:afko,afpo,ztbzps,zrmm220t1,resb,mard,sscrfields.
INCLUDE zrmm220_zcd.                      "转储单数据
*一、数据声明
DATA: BEGIN OF item OCCURS 0,
        werks     LIKE resb-werks,              "工厂
        zdydh     LIKE zrmm220t1-zdydh,            "配送单号
        aufnr     LIKE afko-aufnr,              "生产订单

        lgort     LIKE afpo-lgort,              "车间库---接收库存地
        lgobe     LIKE t001l-lgobe,
        rsnum     LIKE resb-rsnum,              "预留号
        rspos     LIKE resb-rspos,
        matnr     LIKE resb-matnr,              "物料
        maktx     LIKE makt-maktx,              "物料描述
        bdmng     LIKE resb-bdmng,              "需求数量
        menge     LIKE zrmm220t1-menge,            "实收数量
        meins     LIKE resb-meins,              "单位
        sortf     LIKE resb-sortf,              "排序字符串
        zbanz(10),                              "班组
        zgwei(10),                              "工位
        lgfsb     LIKE marc-lgfsb,              "二级库----原来：外部采购仓储地点 = 发出库存地
        lgfse     LIKE t001l-lgobe,
        lgpbe     LIKE mard-lgpbe,              "存储仓位/库管员
        zbzmg     LIKE ztbzps-zbzmg,            "标准配送量
        zrept(10),
        wempf     LIKE resb-wempf,              "备注
        zbez(40),
        zzzt(4),
        rtp04     LIKE afpo-rtp04,              "合并标示
        xloek     LIKE resb-xloek,"删除标示
        beskz     LIKE marc-beskz,
        ablad     LIKE resb-ablad,
        dispo     LIKE afko-dispo,              "MRP控制者
        umlgo     LIKE marc-lgfsb,              "寄售库
        lifnr     LIKE lfa1-lifnr,              "
        sortl     LIKE lfa1-sortl,
        name1     LIKE lfa1-name1,
        aufn2     LIKE zrmm220t1-aufn2,
        chck,
      END OF item.

DATA: gd_repid LIKE sy-repid.
DATA: i_grid_settings TYPE lvc_s_glay .
DATA: zt_h LIKE TABLE OF item WITH HEADER LINE.
*DATA: lt_wempf LIKE TABLE OF item WITH HEADER LINE.
DATA: itab LIKE TABLE OF item WITH HEADER LINE.
DATA: lt_et LIKE TABLE OF item WITH HEADER LINE.
DATA: wa_h LIKE item.
DATA: itac LIKE TABLE OF item WITH HEADER LINE.
DATA: l_aufnr LIKE afko-aufnr, "第一个工单
      l_i(2)."如果输入范围差值
DATA: BEGIN OF it_header OCCURS 0,
        zbanz LIKE zrmm220t1-zbanz,
        umlgo LIKE zrmm220t1-umlgo,
        lgpbe LIKE zrmm220t1-lgpbe,
        lgort LIKE zrmm220t1-lgort,
      END OF it_header.

DATA:gt_dynfields LIKE TABLE OF dynpread WITH HEADER LINE.
DATA:t_line   LIKE TABLE OF tline WITH HEADER LINE,
     p_tdname LIKE          stxl-tdname.

DATA: afield TYPE slis_fieldcat_alv.
DATA: fieldcat TYPE TABLE OF slis_fieldcat_alv WITH HEADER LINE.
DATA: layout       TYPE slis_layout_alv,
      i_grid_title TYPE lvc_title.
DATA: l_pos TYPE i VALUE 1,p_i TYPE i.
DATA: gv_filename TYPE rlgrap-filename.
DATA: l_zbzmg   LIKE ztbzps-zbzmg,l_num TYPE i,p_num TYPE i,l_con(30),z_num TYPE numc4,l_zdydh LIKE zrmm220t1-zdydh,o_num TYPE numc3.
DATA: g_cmd LIKE sy-ucomm.
DATA: s_i(40) TYPE c.
DATA: l_gd10 TYPE i."工单超过10个报错
DATA: zzxrq LIKE sy-datum.
DATA: zzxsj LIKE sy-uzeit.
DATA: l_line LIKE bsvx-sttxt.
DATA: l_objnr LIKE aufk-objnr.
DATA: l_data LIKE sy-datum.
*
DATA: gt_events TYPE slis_t_event.
DATA tem_grid TYPE REF TO cl_gui_alv_grid.
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      update_delta_tables
                    FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD update_delta_tables.

    CLEAR: wa_save_et.
    LOOP AT it_save_et INTO wa_save_et.
      CLEAR wa_save_et-makts.
      SELECT SINGLE  name1 INTO wa_save_et-name1 FROM lfa1 WHERE lifnr = wa_save_et-lifnr.
      SELECT SINGLE  sortl INTO wa_save_et-sortl FROM lfa1 WHERE lifnr = wa_save_et-lifnr.
      MODIFY it_save_et FROM wa_save_et TRANSPORTING name1 sortl.
      CLEAR: wa_save_et.
    ENDLOOP.
    DATA stbl TYPE lvc_s_stbl.
*   稳定刷新
    stbl-row = 'X'." 基于行的稳定刷新
    stbl-col = 'X'." 基于列稳定刷新
    CALL METHOD tem_grid->refresh_table_display
      EXPORTING
        is_stable = stbl.
  ENDMETHOD.                  "update_delta_tables
ENDCLASS.

DATA gt_event_receiver TYPE REF TO lcl_event_receiver .
*
*宏的定义
DEFINE fill.
  afield-col_pos = l_pos.
  afield-fieldname = &1.
  afield-seltext_l = &2.
  afield-no_zero   = &3.
  afield-key       = &4.
  afield-edit      = &5.

  append afield to fieldcat.
  clear afield.
  l_pos = l_pos + 1.
END-OF-DEFINITION.

*二、声明查询屏幕
SELECTION-SCREEN FUNCTION KEY 1 .
SELECTION-SCREEN BEGIN OF BLOCK radio WITH FRAME TITLE text-001.
PARAMETERS: p1 RADIOBUTTON GROUP rad1 USER-COMMAND sele DEFAULT 'X' ,
            p2 RADIOBUTTON GROUP rad1 .
SELECTION-SCREEN END OF BLOCK radio.

SELECTION-SCREEN BEGIN OF BLOCK blc WITH FRAME TITLE text-002.
PARAMETERS: r1 RADIOBUTTON GROUP rad2 DEFAULT 'X' MODIF ID m1 ,
            r2 RADIOBUTTON GROUP rad2 MODIF ID m1 .
SELECTION-SCREEN SKIP.
PARAMETER:       p_werks LIKE resb-werks DEFAULT '6000' .
SELECT-OPTIONS : s_zdydh FOR zrmm220t1-zdydh MODIF ID m2.
SELECT-OPTIONS : s_aufnr  FOR afko-aufnr MODIF ID m1,
                 s_dispo  FOR afko-dispo NO-EXTENSION NO INTERVALS MODIF ID m1,
                 s_umlgo  FOR resb-lgort NO-EXTENSION NO INTERVALS MODIF ID m1,
                 s_lgpbe  FOR mard-lgpbe MODIF ID m1,
                 s_zbanz  FOR zrmm220t1-zbanz MODIF ID m1,                       "班组
                 s_matnr  FOR resb-matnr MODIF ID m1,
                 s_sortf  FOR resb-sortf MODIF ID m1.
SELECTION-SCREEN END OF BLOCK blc.
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW .
PARAMETER p_aufnr LIKE afko-aufnr.
SELECTION-SCREEN END OF SCREEN 1001.

INITIALIZATION.
  sscrfields-functxt_01 = '查询工单组合'.

AT SELECTION-SCREEN .

  CASE sscrfields-ucomm . "
    WHEN 'FC01'.
      PERFORM searchaufnr.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'M1'.
        IF p1 = 'X'.
          screen-active = '1'.

        ELSE.
          screen-active = '0'.
        ENDIF.

      WHEN 'M2'.
        IF p1 = 'X'.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
  IF p1 = 'X'.
    IF p_werks IS INITIAL OR s_aufnr IS INITIAL OR s_dispo IS INITIAL OR s_umlgo IS INITIAL.
      MESSAGE s001(00) WITH '工厂、生产订单、MRP控制者、发出库存地需要输入' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    SELECT * FROM afko WHERE  dispo <> s_dispo-low AND aufnr IN s_aufnr.
      IF sy-subrc = 0.
        MESSAGE s001(00) WITH  '生产订单中存在不同的MRP控制者，不允许打印' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDSELECT.

  ENDIF.

  IF p2 = 'X'.
    IF p_werks IS INITIAL OR s_zdydh IS INITIAL.
      MESSAGE s001(00) WITH '工厂、单据号需要输入' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p1 = 'X'.
    PERFORM frm_get_data1.                 "获取数据
    IF l_gd10 > 20.
      MESSAGE s001(00) WITH '已经超出20个生产订单，请重新合并生产订单' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    LOOP AT item.
      "检查预留库存地点
      IF item-lgort = ''.
        MESSAGE s001(00) WITH '生产订单中物料：' item-matnr '，没有接收库存地点，请联系生产物控部维护！' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.


    IF lt_et[] IS NOT INITIAL.
      PERFORM frm_show_data.
    ENDIF.
  ELSE.
    PERFORM frm_get_data2.                 "获取数据
  ENDIF.

  IF ( item[] IS NOT INITIAL OR it_save[] IS NOT INITIAL ) AND lt_et[] IS INITIAL.
    IF p1 = 'X'.
      PERFORM frm_get_event.
      PERFORM frm_get_jskc.                            "获取寄售可用库存
      PERFORM frm_process_data.
      PERFORM frm_show_mx.                           "显示分配供应商寄售库存的明细数据
    ELSE.
      PERFORM printdata USING 'ZMM220'.               "调用sf
    ENDIF.
  ELSE.
    MESSAGE s001(00) WITH '没有查询到数据'.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data1 .

  SELECT afko~aufnr afpo~pwerk AS werks resb~lgort "afpo~rtp04
         resb~rsnum resb~rspos resb~matnr resb~bdmng resb~meins resb~sortf resb~wempf resb~xloek
         marc~lgfsb marc~beskz resb~ablad afko~dispo
    INTO CORRESPONDING FIELDS OF TABLE item
    FROM afko
    INNER JOIN afpo ON ( afko~aufnr = afpo~aufnr AND afpo~posnr = '0001' )
    INNER JOIN resb ON ( afko~rsnum = resb~rsnum AND resb~werks = afpo~pwerk )"AND resb~xloek = ''  )
    INNER JOIN marc ON ( marc~matnr = resb~matnr AND marc~werks = afpo~pwerk )
    WHERE afpo~pwerk = p_werks AND afko~aufnr IN s_aufnr
                               AND resb~matnr IN s_matnr
                               AND marc~beskz IN ('F','X')
                               AND afko~dispo IN s_dispo
                               AND marc~loggr <> 'A1'
                               "AND resb~WEMPF = ''
                               AND resb~sortf IN s_sortf.


  SORT item BY werks aufnr matnr.
  DELETE item WHERE beskz = 'X' AND ablad <> '外购' OR wempf IS NOT INITIAL.
  SELECT * FROM afko
    WHERE aufnr IN s_aufnr.
    l_gd10 = l_gd10 + 1.
  ENDSELECT.
  SORT s_aufnr BY low.
  READ TABLE s_aufnr INDEX 1.
  l_aufnr = s_aufnr-low."第一个工单（最小工单）作为合并后唯一识别的工单
  CLEAR:itac,itac[].
  LOOP AT item.
    IF item-aufnr IS NOT INITIAL.
      SELECT SINGLE aufk~objnr INTO l_objnr FROM aufk WHERE aufnr = item-aufnr.
      CHECK sy-subrc EQ 0.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = l_objnr
          spras            = sy-langu
        IMPORTING
          line             = l_line
        EXCEPTIONS
          object_not_found = 01.
      SEARCH l_line FOR 'TECO'.
      IF sy-subrc = 0.
        l_line = l_line+sy-fdpos(4).
        item-zzzt = l_line.
      ENDIF.
    ENDIF.
    MODIFY item.
  ENDLOOP.
  DELETE item WHERE zzzt = '' AND xloek = 'X'.
  LOOP AT item.
    itac-aufn2 = l_aufnr.
    itac-werks = item-werks.
    itac-lgort = item-lgort .
    itac-matnr = item-matnr .
    itac-maktx = item-maktx .
    itac-bdmng = item-bdmng .
    itac-meins = item-meins .
    itac-sortf = item-sortf .
    itac-zgwei = item-zgwei .
    CASE item-lgort.
      WHEN '2001'.
        itac-lgfsb = '2005'.
      WHEN '2002'.
        itac-lgfsb = '2006'.
      WHEN '2003'.
        itac-lgfsb = '2007'.
      WHEN '2004'.
        itac-lgfsb = '2008'.
      WHEN '2009'.
        itac-lgfsb = '2010'.
    ENDCASE.

    COLLECT itac.
    CLEAR itac.    """"""""""""""""""""""""""
  ENDLOOP.
  CLEAR:item,item[].
  APPEND LINES OF itac TO item.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""

  LOOP AT item.

*获取其他字段
    SELECT SINGLE * FROM ztbzps
    WHERE werks = item-werks AND matnr = item-matnr.
    IF r1 = 'X'.
      IF sy-subrc = 0.
        DELETE item.
        CONTINUE.
      ENDIF.
    ELSE.
      IF sy-subrc <> 0.
        DELETE item.
        CONTINUE.
      ENDIF.
    ENDIF.
*物料描述
    SELECT SINGLE maktx INTO item-maktx
    FROM makt WHERE matnr = item-matnr AND spras = '1'.
*班组/工位
*    SPLIT item-sortf AT '/' INTO item-zbanz item-zgwei.
*工位
    item-zgwei = item-sortf.

*班组
    SELECT SINGLE zbanz INTO item-zbanz
    FROM ztbzgw WHERE werks = item-werks AND lgort = item-lgort
    AND zgwei = item-zgwei.
*仓管员
    item-umlgo = s_umlgo-low.
    SELECT SINGLE lgpbe INTO item-lgpbe
    FROM mard WHERE matnr = item-matnr AND werks = item-werks
                                       AND lgort = item-umlgo.
    MODIFY item.
    MOVE-CORRESPONDING item TO zt_h.
    CLEAR:zt_h-rspos,zt_h-wempf.
    COLLECT zt_h.
*    lt_wempf-aufnr = item-aufnr.
*    lt_wempf-zgwei = item-sortf.
*    lt_wempf-matnr = item-matnr.
*    lt_wempf-wempf = item-wempf.
*    COLLECT lt_wempf.


*错误数据
    lt_et-matnr = item-matnr.
    lt_et-maktx = item-maktx.
    lt_et-sortf = item-sortf.
    lt_et-lgfsb = item-umlgo.
    lt_et-lgpbe = item-lgpbe.
    IF lt_et-sortf IS INITIAL OR lt_et-lgpbe IS INITIAL.
      COLLECT lt_et.
    ELSE.
    ENDIF.
    CLEAR:lt_et,zt_h.
  ENDLOOP.
  IF s_lgpbe IS NOT INITIAL.
    DELETE zt_h WHERE lgpbe NOT IN s_lgpbe.
    DELETE item WHERE lgpbe NOT IN s_lgpbe.
  ENDIF.
  IF s_zbanz IS NOT INITIAL.
    DELETE zt_h WHERE zbanz NOT IN s_zbanz.
    DELETE item WHERE zbanz NOT IN s_zbanz.
  ENDIF.

ENDFORM.                    " FRM_GET_DATA1
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_data .
  i_grid_settings-edt_cll_cb  = 'X' .            "显示界面可编辑字段上修改了数据，回车后就会立即将内表的数据也修改
  layout-colwidth_optimize    = 'X'.                "设置字段列宽自适应
  layout-zebra             = 'X'.
  i_grid_title             = '物料错误信息'.

  CLEAR afield.
  REFRESH fieldcat.

  fill  'MATNR'      '物料编码'     'X' 'X' ''.
  fill  'MAKTX'      '物料描述'     '' '' ''.
  fill  'SORTF'      '排序字符串'   '' '' ''.
  fill  'LGFSB'      '发出库存地'   'X' '' ''.
  fill  'LGPBE'      '仓管员'       '' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_settings    = i_grid_settings                 "显示界面可编辑字段上修改了数据，回车后就会立即将内表的数据也修改
      i_callback_program = sy-cprog "sy-repid                        "标记当前程序
      it_fieldcat        = fieldcat[]
      is_layout          = layout
      i_grid_title       = i_grid_title
*     i_callback_user_command  = 'USER_COMMAND'
    TABLES
      t_outtab           = lt_et.
ENDFORM.                    " FRM_SHOW_DATA


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data2 .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_save
    FROM zrmm220t1
    WHERE werks = p_werks AND zdydh IN s_zdydh.
  "AND mblnr = ''
  "AND xloek = ''.

  LOOP AT it_save.
    SELECT SINGLE maktx INTO it_save-makts
    FROM makt WHERE matnr = it_save-matnr AND spras = '1'.
    SELECT SINGLE sortl INTO it_save-sortl
      FROM lfa1 WHERE lifnr = it_save-lifnr.
    MODIFY it_save TRANSPORTING makts sortl.
  ENDLOOP.
ENDFORM.                    " FRM_GET_DATA2
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_process_data .
  IF p1 = 'X'.
    CLEAR:zt_save,zt_save[].
    LOOP AT zt_h.
      SELECT * FROM zrmm220t1
        WHERE aufn2 = zt_h-aufn2 AND matnr = zt_h-matnr
                                 AND zbanz = zt_h-zbanz
                                 AND lgort = zt_h-lgort
                                 AND zgwei = zt_h-zgwei
                                 AND xloek = ''.
        IF zrmm220t1-psgz IS INITIAL.
          IF zrmm220t1-zcdhgz IS INITIAL.            "转储未过帐
            zt_h-bdmng = zt_h-bdmng - zrmm220t1-bdmng.
          ELSE.
            zt_h-bdmng = zt_h-bdmng - zrmm220t1-menge.

          ENDIF.
        ELSE.
          zt_h-bdmng = zt_h-bdmng - zrmm220t1-menge.
        ENDIF.
      ENDSELECT.
      IF zt_h-bdmng <= 0.
        DELETE zt_h.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    LOOP AT it_save.
      MOVE-CORRESPONDING it_save TO zt_save.
      LOOP AT zt_h WHERE matnr = it_save-matnr.
        IF zt_h-bdmng > it_save-bdmng.
          zt_save-zbanz = zt_h-zbanz.
          zt_save-zgwei = zt_h-zgwei.
          zt_save-makts = zt_h-maktx.
          zt_save-lgfsb = zt_h-lgfsb.
          zt_save-lgort = zt_h-lgort.
          zt_save-bdmng = it_save-bdmng.
          SELECT SINGLE name1 sortl INTO (zt_save-name1,zt_save-sortl)
            FROM lfa1 WHERE lifnr = it_save-lifnr.
          APPEND zt_save.
          DELETE it_save.
          zt_h-bdmng = zt_h-bdmng - it_save-bdmng.
          MODIFY zt_h TRANSPORTING bdmng.
          EXIT.
        ELSE.
          zt_save-zbanz = zt_h-zbanz.
          zt_save-zgwei = zt_h-zgwei.
          zt_save-makts = zt_h-maktx.
          zt_save-name1 = zt_h-sortl.
          zt_save-lgfsb = zt_h-lgfsb.
          zt_save-lgort = zt_h-lgort.
          zt_save-bdmng = zt_h-bdmng.
          SELECT SINGLE name1 sortl INTO (zt_save-name1,zt_save-sortl)
            FROM lfa1 WHERE lifnr = it_save-lifnr.
          APPEND zt_save.
          DELETE zt_h.
          it_save-bdmng = it_save-bdmng - zt_h-bdmng.
          IF it_save-bdmng = 0.
            EXIT.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    CLEAR:it_save[].
    it_save[] = zt_save[].
  ENDIF.

ENDFORM.                    " FRM_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  SEARCHAUFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM searchaufnr .
  DATA: lt_fldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  CALL SELECTION-SCREEN 1001 STARTING AT 40 5 .
  CHECK sy-subrc = 0 .

  SELECT DISTINCT zjsdh INTO TABLE it_zjsdh FROM zrmm220t2
    WHERE zrmm220t2~aufnr = p_aufnr.
  IF it_zjsdh[] IS NOT INITIAL.
    SELECT DISTINCT aufnr INTO TABLE yt_aufnr FROM zrmm220t2
      FOR ALL ENTRIES IN it_zjsdh
      WHERE zjsdh = it_zjsdh-zjsdh.
    SORT yt_aufnr.

    lt_fldcat-fieldname = 'AUFNR'.
    lt_fldcat-seltext_l = '生产订单'.
    APPEND lt_fldcat.

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title               = '合并打印的生产订单'
        i_tabname             = ''
        i_selection           = ''
        it_fieldcat           = lt_fldcat[]
        i_screen_start_column = 40
        i_screen_start_line   = 5
      TABLES
        t_outtab              = yt_aufnr
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
  ELSE.
    MESSAGE s000(oo) WITH '没有找到'.
  ENDIF.
ENDFORM.                    " SEARCHAUFNR

INCLUDE zmm220_frm_get_jskcf01.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_MX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_mx .
  slayt-colwidth_optimize = 'X'.
  slayt-zebra             = 'X'.
  slayt-info_fieldname = 'COLOR'.
  repid = sy-repid.
  varnt-report = sy-repid.
  varnt-handle = 1.
  SET TITLEBAR 'T100' WITH '寄售转储单打印程序'.

  PERFORM catlg_set USING: 'AUFNR'    '订单号',
                           'RSNUM'    '预留编号',
                           'RSPOS'    '项目',
                           'MATNR'    '物料号',
                           'WERKS'    '工厂',
                           'LGFSB'    '二级库',
                           'LGORT'    '车间库',
                           'BDMNG'    '需求量',
                           'MEINS'    '单位',
                           'XLOEK'    '已删除项目',
                           'STAT '    '对象状态',
                           'ABLAD'    '是否外购',
                           'DISPO'    'MRP控制者',
                           'BESKZ'    '采购类型',
                           'LGPBE'    '仓管员',
                           'QUNUM'    '配额编号'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      it_fieldcat              = it_fldcat[]
      i_save                   = 'A'
      is_variant               = varnt
      is_layout                = slayt
      i_callback_user_command  = 'USER_COMMAND1'
      i_callback_pf_status_set = 'SET_STATUS1'
    TABLES
      t_outtab                 = it_resb[].
ENDFORM.                    " FRM_SHOW_MX
*---------------------------------------------------------------------*
*       FORM frm_catlg_set                                            *
*---------------------------------------------------------------------*
FORM catlg_set USING p_field
                         p_text.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  ls_fieldcat-fieldname     =  p_field.
  ls_fieldcat-seltext_l     =  p_text.

  IF ls_fieldcat-fieldname = 'MENGE' OR
    ls_fieldcat-fieldname = 'BDMNG'.
    ls_fieldcat-qfieldname = 'MEINS'.
  ELSEIF ls_fieldcat-fieldname = 'TBTWR'.
    ls_fieldcat-cfieldname = 'WAERS'.
  ELSEIF ls_fieldcat-fieldname = 'AUFNR' OR
         ls_fieldcat-fieldname = 'LIFNR' .
    ls_fieldcat-edit_mask = '==ALPHA'.
  ELSEIF ls_fieldcat-fieldname = 'MATNR'.
    ls_fieldcat-edit_mask = '==MATN1'.
  ELSEIF ls_fieldcat-fieldname = 'MEINS' .
    ls_fieldcat-edit_mask = '==CUNIT'.
  ENDIF.

  APPEND ls_fieldcat TO it_fldcat .
  CLEAR ls_fieldcat .
ENDFORM.                    "frm_catlg_set
*&--------------------------------------------------------------------*
*&      Form  user_command1
*&--------------------------------------------------------------------*
FORM user_command1 USING r_ucomm LIKE sy-ucomm
                        rs_selfld TYPE slis_selfield.
  READ TABLE it_resb INDEX rs_selfld-tabindex.
  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CASE rs_selfld-fieldname.
        WHEN 'QUNUM'.
          SET PARAMETER ID 'WRK' FIELD it_resb-werks.
          SET PARAMETER ID 'MAT' FIELD it_resb-matnr.
          CALL TRANSACTION 'MEQ3' AND SKIP FIRST SCREEN.
        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD it_resb-aufnr.
          CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN .
      ENDCASE.
    WHEN 'HEBING'.
      PERFORM hebing.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDFORM. "user_com

*&--------------------------------------------------------------------*
*&      Form  set_status
*&--------------------------------------------------------------------*
FORM set_status1 USING rt_extab TYPE slis_t_extab.
  IF disable = 'X'.
    APPEND 'HEBING' TO rt_extab .
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab .
ENDFORM.                    "set_status1
*&---------------------------------------------------------------------*
*&      Form  HEBING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hebing .
  PERFORM frm_get_xc.
*  slayt-colwidth_optimize = 'X'.
*  slayt-zebra             = 'X'.
*  slayt-info_fieldname = 'COLOR'.
*  repid = sy-repid.
*  varnt-report = sy-repid.
*  varnt-handle = 2.
*  SET TITLEBAR 'T100' WITH '寄售转储单合并'.
*
*  CLEAR it_fldcat[].
*  PERFORM catlg_set USING: 'CHCK'     '选择',
*                           'LIFNR'    '供应商',
*                           'NAME1'    '供应商名称',
*                           'WERKS'    '工厂',
*                           'UMLGO'    '发出库',
*                           'LGFSB'    '二级库',
*                           'LGORT'    '车间库',
*                           'LGPBE'    '仓管员',
*                           'ZBANZ'    '班组',
*                           'ZGWEI'    '工位',
*                           'MATNR'    '物料号 ',
*                           'MAKTS'    '物料描述',
*                           'AUFN2'    '订单号 ',
*                           'BDMNG'    '需求量',
*                           'MEINS'    '单位'.
*
*  LOOP AT it_fldcat.
*    IF it_fldcat-fieldname = 'CHCK'.
*      it_fldcat-edit = 'X'.
*      it_fldcat-checkbox = 'X'.
*    ENDIF.
*    MODIFY it_fldcat.
*  ENDLOOP.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = repid
*      it_fieldcat              = it_fldcat[]
*      i_save                   = 'A'
*      is_variant               = varnt
*      is_layout                = slayt
*      i_callback_user_command  = 'USER_COMMAND2'
*      i_callback_pf_status_set = 'SET_STATUS2'
*    TABLES
*      t_outtab                 = it_save_et.
  CLEAR it_fieldcat[].
  i_grid_settings-edt_cll_cb  = 'X' .
  gd_layout-stylefname = 'FIELD_STYLE'.
  gd_layout-cwidth_opt = 'X'.
  gd_layout-zebra             = 'X'.


  wa_fieldcat-fieldname = 'CHCK'.
  wa_fieldcat-scrtext_m = '选择'.
  wa_fieldcat-edit = 'X'.
  wa_fieldcat-checkbox = 'X'.
  wa_fieldcat-col_pos     = 0.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-scrtext_m ='供应商'.
  wa_fieldcat-col_pos     = 1.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname ='NAME1'.
  wa_fieldcat-scrtext_m =  '供应商名称'.
  wa_fieldcat-col_pos     = 2.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname =  'WERKS'.
  wa_fieldcat-scrtext_m =    '工厂'.
  wa_fieldcat-col_pos     = 3.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'UMLGO'.
  wa_fieldcat-scrtext_m = '发出库'.
  wa_fieldcat-col_pos     = 4.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname =  'LGFSB'.
  wa_fieldcat-scrtext_m = '二级库'.
  wa_fieldcat-col_pos     = 5.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'LGORT'.
  wa_fieldcat-scrtext_m =  '车间库'.
  wa_fieldcat-col_pos     = 6.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.


  wa_fieldcat-fieldname = 'LGPBE'.
  wa_fieldcat-scrtext_m =  '仓管员'.
  wa_fieldcat-col_pos     = 7.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'ZBANZ'.
  wa_fieldcat-scrtext_m =   '班组'.
  wa_fieldcat-col_pos     = 8.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'ZGWEI'.
  wa_fieldcat-scrtext_m =   '工位'.
  wa_fieldcat-col_pos     = 9.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-scrtext_m =  '物料号 '.
  wa_fieldcat-col_pos     = 10.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname =  'MAKTS'.
  wa_fieldcat-scrtext_m =   '物料描述'.
  wa_fieldcat-col_pos     = 11.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname =   'AUFN2'.
  wa_fieldcat-scrtext_m =   '订单号 '.
  wa_fieldcat-col_pos     = 12.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'BDMNG'.
  wa_fieldcat-scrtext_m =   '需求量'.
  wa_fieldcat-no_zero = 'X'.
  wa_fieldcat-qfieldname = 'MEINS'.
  wa_fieldcat-inttype = 'X'.
  wa_fieldcat-col_pos     = 13.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  wa_fieldcat-fieldname = 'MEINS'.
  wa_fieldcat-scrtext_m =   '单位'.
  wa_fieldcat-col_pos     = 14.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR  wa_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_grid_settings          = i_grid_settings
      i_callback_program       = sy-cprog
      is_layout_lvc            = gd_layout
      it_fieldcat_lvc          = it_fieldcat
      i_save                   = 'X'
      i_callback_pf_status_set = 'SET_STATUS2'
      i_callback_user_command  = 'USER_COMMAND2'
      it_events                = gt_events "注册回车事件
    TABLES
      t_outtab                 = it_save_et
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&--------------------------------------------------------------------*
*&      Form  user_command
*&--------------------------------------------------------------------*
FORM user_command2 USING r_ucomm LIKE sy-ucomm
                        rs_selfld TYPE slis_selfield.
  rs_selfld-refresh = 'X'.""""""""""""""""""""""""""""""""
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.

  READ TABLE it_resb INDEX rs_selfld-tabindex.
  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CASE rs_selfld-fieldname.
        WHEN ''.
      ENDCASE.
    WHEN 'SAVEPRINT'.
      READ TABLE it_save_et WITH KEY chck = 'X'.
      IF sy-subrc <> 0.
        MESSAGE s000(oo) WITH '至少选择一行'.
        EXIT.
      ENDIF.
      CLEAR: it_save,it_save[].
      LOOP AT it_save_et.
        MOVE-CORRESPONDING it_save_et TO it_save.
        APPEND it_save.
        CLEAR: it_save_et,it_save.
      ENDLOOP.
      PERFORM savedata.
      PERFORM printdata USING 'ZMM220'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN '&ZALL'.
      LOOP AT it_save_et WHERE chck IS INITIAL.
        it_save_et-chck = 'X'.
        MODIFY it_save_et TRANSPORTING chck.
      ENDLOOP.
    WHEN '&ZSAL'.
      LOOP AT it_save_et WHERE chck IS NOT INITIAL.
        it_save_et-chck = ''.
        MODIFY it_save_et TRANSPORTING chck.
      ENDLOOP.
  ENDCASE.
ENDFORM. "user_com

*&--------------------------------------------------------------------*
*&      Form  set_status
*&--------------------------------------------------------------------*
FORM set_status2 USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD2' EXCLUDING rt_extab .
ENDFORM.                    "set_status
*&---------------------------------------------------------------------*
*&      Form  SAVEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM savedata .
  DATA: numc(4) TYPE n.

  DELETE it_save WHERE chck = ''.
  SORT it_save .

  CLEAR:it_header[],zt_save[],zt_save.
  LOOP AT it_save.
    it_header-zbanz = it_save-zbanz.
    it_header-umlgo = it_save-umlgo.
    it_header-lgpbe = it_save-lgpbe.
    it_header-lgort = it_save-lgort.
    COLLECT it_header.
  ENDLOOP.

  LOOP AT it_header.
    IF r1 = 'X'.
      CALL FUNCTION 'ZNUMC4_GET_NEXT'
        EXPORTING
          subobject = '02'
        IMPORTING
          number    = numc
        EXCEPTIONS
          OTHERS    = 9.
      l_zdydh = 'ZP' && sy-datum+2(6) && numc.
      LOOP AT it_save WHERE zbanz = it_header-zbanz AND lgort = it_header-lgort AND lgpbe = it_header-lgpbe.  "
        MOVE-CORRESPONDING it_save TO zt_save.
        zt_save-zdydh = l_zdydh.
        APPEND zt_save.
        CLEAR zt_save.
      ENDLOOP.
    ELSE.
      CLEAR:p_num,mt_save,mt_save[].
      LOOP AT it_save WHERE zbanz = it_header-zbanz AND lgort = it_header-lgort AND lgpbe = it_header-lgpbe.         "每个分组下最大的重复次数
        CLEAR l_zbzmg.
        SELECT SINGLE zbzmg INTO l_zbzmg
        FROM ztbzps WHERE werks = it_save-werks AND matnr = it_save-matnr.
        IF l_zbzmg > 0.
          l_num = ceil( it_save-bdmng / l_zbzmg ).
          DO l_num TIMES.
            MOVE-CORRESPONDING it_save TO mt_save.
            mt_save-bdmng = l_zbzmg.
            IF l_num = sy-index.
              mt_save-bdmng = it_save-bdmng MOD l_zbzmg.
              IF mt_save-bdmng = 0.                     "整除的情况
                mt_save-bdmng = l_zbzmg.
              ENDIF.
            ENDIF.
            mt_save-znumc = sy-index.
            APPEND mt_save.
            CLEAR mt_save.
          ENDDO.
        ELSE.
          l_num = 1.
          MOVE-CORRESPONDING it_save TO mt_save.
          mt_save-znumc = 1.
          APPEND mt_save.
          CLEAR mt_save.
        ENDIF.
        IF l_num > p_num.
          p_num = l_num.
        ENDIF.
      ENDLOOP.
      DO p_num TIMES.
        CALL FUNCTION 'ZNUMC4_GET_NEXT'
          EXPORTING
            subobject = '02'
          IMPORTING
            number    = numc
          EXCEPTIONS
            OTHERS    = 9.
        l_zdydh = 'ZP' && sy-datum+2(6) && numc.
        o_num = sy-index.
        LOOP AT mt_save WHERE znumc = o_num.
          mt_save-zdydh = l_zdydh.
          MODIFY mt_save TRANSPORTING zdydh.
        ENDLOOP.
      ENDDO.
      APPEND LINES OF mt_save TO zt_save.
    ENDIF.
  ENDLOOP.
  SORT zt_save BY werks zdydh zbanz lgpbe zgwei matnr.
  CLEAR:it_save,it_save[],it_header,it_header[].
  it_save[] = zt_save[].

  CALL FUNCTION 'ZNUMC4_GET_NEXT'
    EXPORTING
      subobject = '02'
    IMPORTING
      number    = numc
    EXCEPTIONS
      OTHERS    = 9.

  it_resb-zjsdh = zjsdh = it_save-zjsdh = sy-datum && numc.
  MODIFY it_resb TRANSPORTING zjsdh WHERE zjsdh = ''.
  MODIFY it_save TRANSPORTING zjsdh WHERE zjsdh = ''.
  LOOP AT it_save.
    it_save-zjsdi = sy-tabix.
    it_save-erdat = sy-datum.
    it_save-erzet = sy-uzeit.
    it_save-ernam = sy-uname.
    MODIFY it_save.
  ENDLOOP.

  INSERT zrmm220t1 FROM TABLE it_save.
  INSERT zrmm220t2 FROM TABLE it_resb.
  COMMIT WORK.
ENDFORM.                    " SAVEDATA
*&---------------------------------------------------------------------*
*&      Form  PRINTDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printdata USING p_formname.
  DATA: lf_fm_name TYPE rs38l_fnam.
  DATA: ls_control_param TYPE ssfctrlop.
  DATA: ls_composer_param TYPE ssfcompop.
  DATA: lf_formname TYPE tdsfname.

  DATA: jobinfo TYPE ssfcrescl.
  DATA  job_output_info TYPE ssfcrescl.
  DATA  job_output_options TYPE ssfcresop.
  DATA: gt_objbin LIKE solisti1 OCCURS 0 WITH HEADER LINE.

  lf_formname = p_formname.
**打印参数定义
*  ls_control_param-langu     =  sy-langu.
*  ls_control_param-getotf    = 'X'.   "
*  ls_control_param-no_open   = 'X'.
*  ls_control_param-no_close  = 'X'.
*  ls_control_param-no_dialog = 'X'.
*  ls_composer_param-tdnoprev = 'X'.
*  ls_composer_param-tdimmed  = 'X'.
*  ls_composer_param-tddelete = 'X'.

  ls_control_param-langu     =  sy-langu.              "调用smartforms
  ls_control_param-no_open   = 'X'.
  ls_control_param-no_close  = 'X'.
  ls_composer_param-tdimmed  = 'X'.
  ls_composer_param-tddelete = 'X'.
  ls_composer_param-tdiexit = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_composer_param
      user_settings      = ' '
    IMPORTING
      job_output_options = job_output_options
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR:lt_h,lt_h[].
  LOOP AT it_save.
    lt_h-werks = it_save-werks.
    lt_h-aufn2 = it_save-aufn2.
    lt_h-zdydh = it_save-zdydh.
    lt_h-zbanz = it_save-zbanz.
    lt_h-lgpbe = it_save-lgpbe.        "寄售库仓管员
    lt_h-lgort = it_save-lgort.        "车间库
    lt_h-lgfsb = it_save-lgfsb.        "二级库
    lt_h-umlgo = it_save-umlgo.        "发出库--寄售库
    IF p2 = 'X'.
      SELECT SINGLE * FROM zrmm220t1
        WHERE zdydh = it_save-zdydh AND druck = 'X'.
      IF sy-subrc = 0.
        lt_h-zrept = '重复打印'.
      ENDIF.
    ENDIF.
    COLLECT lt_h.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_save-matnr
      IMPORTING
        output = it_save-matnr.
    MODIFY it_save.
  ENDLOOP.

  LOOP AT lt_h.
    CLEAR:lft_h[],lft_h,lft_i[],lft_i,p_i.
    LOOP AT it_save WHERE zdydh = lt_h-zdydh AND zbanz = lt_h-zbanz AND lgpbe = lt_h-lgpbe AND lgort = lt_h-lgort.
      MOVE-CORRESPONDING it_save TO lft_i.
      p_i = p_i + 1.
      APPEND lft_i.
    ENDLOOP.
    SORT lft_i BY zgwei lifnr matnr.
    MOVE-CORRESPONDING lt_h TO lft_h.
    APPEND lft_h.

    CALL FUNCTION lf_fm_name
      EXPORTING
        control_parameters = ls_control_param
        output_options     = ls_composer_param
        user_settings      = 'X'
*      IMPORTING
*       job_output_info    = job_output_info
*       job_output_options = job_output_options
*     TABLES
*       gt_h               = lft_h
*       gt_i               = lft_i
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = jobinfo
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.
*记录打印日志
    IF jobinfo-outputdone = 'X'.
      LOOP AT it_save.
        UPDATE zrmm220t1 SET druck = 'X'
          WHERE zdydh = it_save-zdydh.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " PRINTDATA

FORM frm_get_xc.
  DATA ls_stylerow TYPE lvc_s_styl.
  DATA lt_styletab TYPE lvc_t_styl.
  DATA l_matnr LIKE zrmm002t1-matnr.
  CLEAR: it_save_et,it_save_et[].
  LOOP AT it_save.
    MOVE-CORRESPONDING it_save TO it_save_et.
    APPEND it_save_et.
  ENDLOOP.
  LOOP AT it_save_et INTO wa_save_et.
    CLEAR l_matnr.
    SELECT SINGLE matnr INTO l_matnr FROM zrmm002t1 WHERE matnr = wa_save_et-matnr.
    IF sy-subrc = 0 AND l_matnr IS NOT INITIAL.
      ls_stylerow-fieldname = 'LIFNR'.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_stylerow INTO TABLE wa_save_et-field_style.

      ls_stylerow-fieldname = 'BDMNG'.
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_stylerow INTO TABLE wa_save_et-field_style.

      MODIFY it_save_et FROM wa_save_et.
      CLEAR wa_save_et.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_event .
  DATA l_events TYPE LINE OF slis_t_event.
  CLEAR l_events.
  l_events-name = 'CALLER_EXIT'.
  l_events-form = 'FM_BUTTON'.
  APPEND l_events TO gt_events.
ENDFORM.                    " FRM_GET_EVENT

FORM fm_button USING e_grid TYPE slis_data_caller_exit.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = tem_grid.

* 设置enter事件
  CALL METHOD tem_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  DATA: gt_event_receiver TYPE REF TO lcl_event_receiver .
  CREATE OBJECT gt_event_receiver.
  SET HANDLER gt_event_receiver->update_delta_tables FOR tem_grid.                          "更新内表itab
ENDFORM.                    "FM_BUTTON