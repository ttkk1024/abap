*&---------------------------------------------------------------------*
*& Report  ZMM024
*&
*&---------------------------------------------------------------------*
*&
*&发货通知单打印
*&---------------------------------------------------------------------*

REPORT zst012.
TYPE-POOLS: slis.

TABLES:vbak,vbfa,mara,kna1,t179,knvv,likp,usr02,lips.
*一、数据声明
DATA: BEGIN OF item OCCURS 0,
        mark(1),                                "选择标记

        vgbel      LIKE vbak-vbeln,             "销售订单
        vbeln      LIKE likp-vbeln,             "销售发货单
        vkorg      LIKE likp-vkorg,             "销售组织
        ernam      LIKE likp-ernam,             "制单人
        ertxt      LIKE usrefus-useralias,      "制单人描述
        kunnr      LIKE likp-kunnr,             "客户
        name1      LIKE kna1-name1,             "客户名称
        stras      LIKE kna1-stras,             "客户地址

        matnr      LIKE lips-matnr,              "物料编码
        maktx      LIKE makt-maktx,              "物料描述
        spart      LIKE mara-spart,              "产品组
        meins      LIKE mara-meins,              "单位
        mjahr      LIKE mseg-mjahr,              "物料凭证年度

        lfimg      LIKE lips-lfimg,              "发货数量
        posnn      LIKE vbfa-posnn,              "vbfa中的凭证项目
        posnr      LIKE lips-posnr,              "销售订单行项目
        erdat      LIKE lips-erdat,              "发货日期
        sdabw      LIKE likp-sdabw,              "货运方式
        bezei      LIKE tvsakt-bezei,            "货运方式描述
        vtext      LIKE tvkot-vtext,             "销售组织描述
        vtext1     LIKE tspat-vtext,             "产品组描述
        bwart      LIKE lips-bwart,              "移动类型
        objnr      LIKE vbak-objnr,              "对象号
        stat       LIKE jest-stat,               "状态
        lgort      LIKE lips-lgort,              "库存地点
        lgobe      LIKE t001l-lgobe,             "仓库地点描述
        bzirk      LIKE knvv-bzirk,              "销售地区
        bztxt      LIKE t171t-bztxt,             "销售区域名称
        mbdat      LIKE lips-mbdat,              "开票日期
        zmont(10),
        ebelx(3),                               "打印单据行项目序号
        zmont1(10),
        ztext(100),                              "售达方
        untxt      LIKE usrefus-useralias,      "打印人描述
        zdycs      LIKE zstdy-zdycs,            "打印次数
        sernr      LIKE objk-sernr,
        zxrir(6),                                "宣传品
        zpwjm(4),                                "配件
        ktext      LIKE vbak-ktext,              "抬头备注
      END OF item.
DATA: BEGIN OF itek OCCURS 0,
        vgbel      LIKE vbak-vbeln,             "销售订单
        augru      LIKE vbak-augru,             "订单原因
        bezei1     LIKE tvaut-bezei,            "订单原因描述
        vtweg      LIKE vbak-vtweg,             "销售渠道
        vtwegt     LIKE tvtwt-vtext,              "分销渠道描述
        vbeln      LIKE likp-vbeln,               "销售发货单
        vkorg      LIKE likp-vkorg,              "销售组织
        ernam      LIKE likp-ernam,
        erzet      LIKE likp-erzet,
        kunnr      LIKE likp-kunnr,              "客户
        name1      LIKE kna1-name1,               "客户名称

        matnr      LIKE lips-matnr,              "物料编码
        maktx      LIKE makt-maktx,              "物料描述
        spart      LIKE mara-spart,              "产品组
        mjahr      LIKE mseg-mjahr,              "物料凭证年度
        auart      LIKE vbak-auart,               "订单类型
        bezei2     LIKE tvakt-bezei,            "订单原因描述
        lfimg      LIKE lips-lfimg,              "发货数量
        posnn      LIKE vbfa-posnn,              "vbfa中的凭证项目
        posnr      LIKE lips-posnr,              "销售订单行项目
        erdat      LIKE lips-erdat,              "发货日期
        sdabw      LIKE likp-sdabw,                "货运方式
        bezei      LIKE tvsakt-bezei,
        vtext      LIKE tvkot-vtext,
        vtext1     LIKE tspat-vtext,
        bwart      LIKE lips-bwart,
        objnr      LIKE vbak-objnr,              "对象号
        stat       LIKE jest-stat,             "状态
        lgort      LIKE lips-lgort,
        bzirk      LIKE knvv-bzirk,
        bztxt      LIKE t171t-bztxt,              "销售区域名称
        zmont(10),

        zmont1(10),
        ebelx(10),
        ztext(100),                                "售达方
        ktext      LIKE vbak-ktext,
      END OF itek.
DATA: tmpitem LIKE item[] WITH HEADER LINE.
DATA: BEGIN OF it_ser01 OCCURS 0, "Baitz 31.12.2015 序列号
        lief_nr	TYPE vbeln_vl,
        posnr	  TYPE posnr_vl,
        obzae   LIKE objk-obzae,
        sernr   LIKE objk-sernr,
      END OF it_ser01.

DATA: gd_repid LIKE sy-repid.
DATA: i_grid_settings TYPE lvc_s_glay .
DATA: lt_h LIKE TABLE OF item WITH HEADER LINE.
DATA: lt_i LIKE TABLE OF item WITH HEADER LINE.
DATA: lft_h LIKE TABLE OF item WITH HEADER LINE.
DATA: lft_i LIKE TABLE OF item WITH HEADER LINE.
DATA: wa_lt LIKE TABLE OF item WITH HEADER LINE.
DATA: wa_item LIKE TABLE OF item WITH HEADER LINE.
DATA: lt_zstdy LIKE TABLE OF zstdy WITH HEADER LINE.

DATA: afield TYPE slis_fieldcat_alv.
DATA: fieldcat TYPE TABLE OF slis_fieldcat_alv WITH HEADER LINE.
DATA: layout TYPE slis_layout_alv.
DATA: l_pos     TYPE i VALUE 0,p_i TYPE i,l_msg(70),l_menge LIKE mseg-menge.

DATA:ebelx TYPE i VALUE 0.

DATA: BEGIN OF lt_dynpro OCCURS 10 ,
        uname(20) TYPE c ,               "打印人
        datum(10) TYPE c ,               "打印日期
        uzeit(10) TYPE c ,               "打印时间
      END OF lt_dynpro .
DATA: BEGIN OF lt_msg OCCURS 10 ,
        ernam LIKE sy-uname ,               "打印人
        erdat LIKE sy-datum ,               "打印日期
        cputm LIKE sy-uzeit ,               "打印时间
      END OF lt_msg .
DATA: f_title TYPE lvc_title.
DATA: BEGIN OF itea,
        year(10),
        mong(14),
      END OF itea.
DATA: l_ztsycs LIKE TABLE OF ztsycs WITH HEADER LINE.

DATA zys TYPE i.
"获取销售文本所需变量
DATA: t_read  LIKE TABLE OF tline WITH HEADER LINE,
      l_stxl  LIKE TABLE OF stxl WITH HEADER LINE,
      l_vbeln LIKE          thead-tdname.
DATA: wa_i LIKE item,
      wa_h LIKE item.
"自动选中同一销售出库单的单据
DATA: tem_grid TYPE REF TO cl_gui_alv_grid.
DATA: wa_modi TYPE lvc_s_modi.
DATA: stbl TYPE lvc_s_stbl.
DATA: l_events TYPE LINE OF slis_t_event.
DATA: gt_events TYPE slis_t_event.

DEFINE fill.
  afield-col_pos = l_pos.
  afield-fieldname = &1.
  afield-seltext_l = &2.
  afield-no_zero   = &3.
  afield-key       = &4.
  afield-edit      = &5.

 IF afield-fieldname = 'LFIMG'.
    afield-DECIMALS_OUT = '0'.         "去掉小数点后边0
  ENDIF.

  IF afield-fieldname = 'SERNR'.
    afield-edit_mask = '==GERNR'.
  ENDIF.

  append afield to fieldcat.
  clear afield.
  l_pos = l_pos + 1.
END-OF-DEFINITION.
SELECTION-SCREEN BEGIN OF BLOCK blc WITH FRAME TITLE text-001.
PARAMETER:       p_vkorg  LIKE vbak-vkorg DEFAULT '6000' OBLIGATORY.


SELECT-OPTIONS :  p_erdat FOR vbfa-erdat ,"DEFAULT sy-datum OBLIGATORY,
                  p_spart  FOR mara-spart DEFAULT '10' OBLIGATORY,
                  p_kuunr  FOR vbak-kunnr,
                  p_bzirk  FOR knvv-bzirk,
                  p_vbeln  FOR vbfa-vbeln,
                  s_vbeln  FOR vbak-vbeln,
                  s_sdabw  FOR likp-sdabw,
                  s_ernam FOR usr02-bname,
                  s_lgort FOR lips-lgort.
SELECTION-SCREEN SKIP.
PARAMETERS: p_c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blc.

INITIALIZATION.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM frm_get_event.
  PERFORM frm_get_data.
  IF item[] IS NOT INITIAL.
    PERFORM frm_modify.
    PERFORM show_data.
  ELSE.
    MESSAGE s001(00) WITH '没有查询到数据'.
  ENDIF.

FORM frm_get_data .
  SELECT lips~vgbel likp~kunnr  likp~vkorg lips~vbeln lips~matnr
           lips~lfimg lips~erdat  lips~posnr bwart vbak~spart knvv~bzirk vbak~ktext
          kna1~name1 sdabw likp~ernam lips~lgort mara~meins kna1~stras lips~mbdat
     INTO CORRESPONDING FIELDS OF TABLE item
     FROM lips
     INNER JOIN vbak ON lips~vgbel = vbak~vbeln
     INNER JOIN knvv ON ( knvv~kunnr = vbak~kunnr AND knvv~vkorg = vbak~vkorg  AND knvv~vtweg = vbak~vtweg )
     INNER JOIN likp ON ( likp~vbeln = lips~vbeln )
     INNER JOIN kna1 ON kna1~kunnr = likp~kunnr
     INNER JOIN mara ON mara~matnr = lips~matnr
    WHERE                        likp~vkorg = p_vkorg
                                AND vbak~spart IN p_spart
                                AND lips~vgbel IN s_vbeln
                                AND lips~vbeln IN p_vbeln
                                AND likp~kunnr IN p_kuunr
                                AND likp~erdat IN p_erdat
                                AND  likp~wadat_ist = '00000000'
                                AND  likp~sdabw IN s_sdabw
                                AND  likp~ernam  IN s_ernam
                                AND lips~lgort IN s_lgort
  AND knvv~bzirk IN p_bzirk.
  "and likp~vbeln in p_vbeln.
  SORT item BY kunnr .
  LOOP AT item.
    SELECT SINGLE maktx INTO item-maktx FROM makt WHERE matnr = item-matnr.
    SELECT SINGLE  bezei INTO item-bezei FROM tvsakt WHERE sdabw = item-sdabw.
    SELECT SINGLE vtext INTO item-vtext FROM tvkot WHERE vkorg = item-vkorg.
    SELECT SINGLE vtext INTO item-vtext1 FROM tspat WHERE  spart = item-spart.
    SELECT SINGLE bwart INTO item-bwart FROM vbfa WHERE vbelv = item-vbeln AND vbfa~vbtyp_n = 'h'.
    SELECT SINGLE objnr INTO item-objnr FROM vbak WHERE vbeln = item-vgbel .
    SELECT SINGLE stat  INTO item-stat FROM jest WHERE objnr = item-objnr AND stat LIKE 'E%'
                                         AND inact = ''..
    SELECT SINGLE bztxt INTO item-bztxt FROM t171t WHERE bzirk = item-bzirk AND spras = '1'.
    SELECT SINGLE lgobe INTO item-lgobe FROM t001l WHERE werks = '6000' AND lgort = item-lgort.
    SELECT SINGLE useralias INTO item-ertxt FROM usrefus WHERE bname = item-ernam.
    SELECT SINGLE useralias INTO item-untxt FROM usrefus WHERE bname = sy-uname.
*打印次数
    SELECT SINGLE zdycs INTO item-zdycs FROM zstdy WHERE vbeln = item-vbeln AND posnr = item-posnr.
    IF sy-subrc <> 0.
      item-zdycs = 0.
    ENDIF.
    IF item-bwart = '602' OR item-bwart = 'Z66'.
      item-zmont = '冲销'.
    ENDIF.
*获取销售文本
    CLEAR: l_vbeln.
    l_vbeln = item-vgbel && item-posnr.
    SELECT SINGLE  * INTO CORRESPONDING FIELDS OF l_stxl FROM stxl
        WHERE tdname = l_vbeln AND tdspras = '1'.
    IF sy-subrc = 0.
      READ TABLE l_stxl INDEX 1.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = l_stxl-tdid
          language = sy-langu
          name     = l_vbeln
          object   = l_stxl-tdobject
        TABLES
          lines    = t_read.

      LOOP AT t_read.
        item-ztext = item-ztext && t_read-tdline.
      ENDLOOP.
    ENDIF.
    MODIFY  item.
  ENDLOOP.
  IF p_c = 'X'.
    DELETE item WHERE zdycs <> ''.
  ENDIF.
  DELETE item WHERE bwart = '653'.
  LOOP AT item.
    wa_lt-vgbel = item-vgbel.
    COLLECT wa_lt.
  ENDLOOP.
  SORT item BY vgbel ztext DESCENDING.
  "READ TABLE item INTO wa_item INDEX 1.
  LOOP AT wa_lt.
    READ TABLE item INTO wa_item WITH  KEY vgbel = wa_lt-vgbel.
    APPEND wa_item.
  ENDLOOP.
  LOOP AT wa_item.
    LOOP AT item WHERE vgbel = wa_item-vgbel.
      item-ztext = wa_item-ztext.
      MODIFY item.
    ENDLOOP.

  ENDLOOP.
  SORT item BY kunnr vgbel posnr.
  DELETE item WHERE stat <> 'E0005'.
  IF item IS NOT INITIAL.
    LOOP AT item.
      l_pos = l_pos + 1.
      item-ebelx = l_pos.
      MODIFY item.
      lt_h-lfimg = lt_h-lfimg + item-lfimg.
      lt_h-kunnr  = '合计'.
    ENDLOOP.
    APPEND lt_h.
    "APPEND LINES OF lt_h TO item.
  ENDIF.

***根据序列号拆分 "Baitz 31.12.2015
  LOOP AT item.
    CLEAR it_ser01[].
    PERFORM get_sernr USING item-vbeln item-posnr .
    LOOP AT it_ser01.
      tmpitem = item.
      tmpitem-lfimg = 1.
      tmpitem-sernr = it_ser01-sernr.
      APPEND tmpitem.

      item-lfimg = item-lfimg - 1.
      MODIFY item.
    ENDLOOP.
  ENDLOOP.

  APPEND LINES OF tmpitem TO item.
  DELETE item WHERE lfimg <= 0.
  SORT item BY vbeln posnr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOW_DATA
*&---------------------------------------------------------------------*
FORM show_data .
  i_grid_settings-edt_cll_cb  = 'X' .
  layout-colwidth_optimize    = 'X'.
  layout-zebra             = 'X'.

  CLEAR afield.
  REFRESH fieldcat.
  fill  'MARK'  '选择'  '' 'X' 'X'.
  fieldcat-checkbox = 'X'.
  fieldcat-hotspot = ''.
  MODIFY fieldcat INDEX 1 TRANSPORTING checkbox hotspot.
  fill  'VBELN'      '发货单号'        '' '' ''.
  fill  'POSNR'      '行项目'           '' '' ''.
  fill  'NAME1'      '客户名称'       '' '' ''.
  fill  'MATNR'      '物料编码'         '' '' ''.
  fill  'MAKTX'      '物料名称'          '' '' ''.
  fill  'LFIMG'      '发货数量'          '' '' ''.
  fill  'LGOBE'      '库存地点'        '' '' ''.
  fill  'SERNR'      'VIN码'        '' '' ''.
  fill  'BEZEI'      '货运方式'          '' '' ''.
  fill  'VGBEL'      '销售订单'        '' '' ''.
  fill  'BWART'      '移动类型'        '' '' ''.
  fill  'ZDYCS'      '打印次数'        '' '' ''.
  fieldcat-hotspot = 'X'.
  MODIFY fieldcat INDEX 12 TRANSPORTING hotspot.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_settings          = i_grid_settings
      i_callback_program       = sy-cprog "sy-repid
      it_fieldcat              = fieldcat[]
      is_layout                = layout
      i_grid_title             = f_title
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
      it_events                = gt_events
    TABLES
      t_outtab                 = item.
ENDFORM.                    " SHOW_DATA

FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZCMD1'.
ENDFORM.

FORM user_command USING rf_ucomm  LIKE sy-ucomm
                        rs        TYPE slis_selfield .
  rs-refresh = 'X'.

  CASE rf_ucomm.

    WHEN '&IC1'.
      READ TABLE item INTO wa_i INDEX rs-tabindex.
      IF rs-fieldname = 'VGBEL'.
        SET PARAMETER ID 'AUN' FIELD  wa_i-vgbel .
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ELSEIF rs-fieldname = 'VBELN'.
        SET PARAMETER ID 'VL' FIELD  wa_i-vbeln .
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ELSEIF rs-fieldname = 'ZDYCS'.
        PERFORM frm_show_dymx.
      ENDIF.
    WHEN '&ZZPT'.
      "检测是否选中数据
      CLEAR: sy-subrc.
      READ TABLE item WITH KEY mark = 'X'.
      IF sy-subrc <> 0.
        MESSAGE s001(00) WITH '请选择记录' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      "打印程序
      PERFORM frm_print USING 'ZST012'.
    WHEN '&ZALL'.
      LOOP AT item WHERE mark IS INITIAL .
        item-mark = 'X'.
        MODIFY item TRANSPORTING mark.
      ENDLOOP.
    WHEN '&ZSAL'.
      LOOP AT item WHERE mark IS NOT INITIAL .
        item-mark = ''.
        MODIFY item TRANSPORTING mark.
      ENDLOOP.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_MODIFY
*&---------------------------------------------------------------------*
FORM frm_modify .
  l_ztsycs-tcode2_mkpf = sy-tcode.
  l_ztsycs-zname =  sy-uname.
  l_ztsycs-zerdat = sy-datum.
  l_ztsycs-cputm = sy-uzeit.
  APPEND l_ztsycs.
  LOOP AT l_ztsycs.
    SELECT SINGLE zsycs INTO l_ztsycs-zsycs FROM ztsycs WHERE tcode2_mkpf = l_ztsycs-tcode2_mkpf.
    l_ztsycs-zsycs = l_ztsycs-zsycs + 1.
    MODIFY l_ztsycs.
  ENDLOOP.
  MODIFY ztsycs FROM TABLE l_ztsycs[].
ENDFORM.                    " FRM_MODIFY

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      update_delta_tables
                    FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD update_delta_tables.
    CLEAR:wa_modi.
    READ TABLE et_good_cells INTO wa_modi INDEX 1.
    CHECK wa_modi-fieldname = 'MARK'.
    READ TABLE item INTO wa_h INDEX wa_modi-row_id.

    LOOP AT item INTO wa_i WHERE vbeln = wa_h-vbeln.
      IF wa_modi-value = 'X'.
        wa_i-mark = 'X'.
        MODIFY item FROM wa_i TRANSPORTING mark.
      ELSE.
        wa_i-mark = ''.
        MODIFY item FROM wa_i TRANSPORTING mark.
      ENDIF.
    ENDLOOP.

*   稳定刷新
    stbl-row = 'X'." 基于行的稳定刷新
    stbl-col = 'X'." 基于列稳定刷新
    CALL METHOD tem_grid->refresh_table_display
      EXPORTING
        is_stable = stbl.
  ENDMETHOD.                  "update_delta_tables

ENDCLASS.              "LCL_EVENT_RECEIVER IMPLEMENTATION

FORM frm_get_event .
  CLEAR l_events.
  l_events-name = 'CALLER_EXIT'.
  l_events-form = 'FM_BUTTON'.
  APPEND l_events TO gt_events.
ENDFORM.                    " FRM_GET_EVENT

*&---------------------------------------------------------------------*
*&      Form  fm_button
*&---------------------------------------------------------------------*
FORM fm_button USING e_grid TYPE slis_data_caller_exit.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = tem_grid.

*设置cell编辑响应事件
  CALL METHOD tem_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified     "mc_evt_enter(回车响应)
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  DATA: gt_event_receiver TYPE REF TO lcl_event_receiver .
  CREATE OBJECT gt_event_receiver.
  SET HANDLER gt_event_receiver->update_delta_tables FOR tem_grid.   "更新内表itab
ENDFORM.                    "FM_BUTTON

*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT
*&---------------------------------------------------------------------*
FORM frm_print  USING   p_formname.
  DATA: lf_fm_name TYPE rs38l_fnam.
  DATA: ls_control_param TYPE ssfctrlop.
  DATA: ls_composer_param TYPE ssfcompop.
  DATA: lf_formname TYPE tdsfname.

  DATA: jobinfo TYPE ssfcrescl.
  DATA  job_output_info TYPE ssfcrescl.
  DATA  job_output_options TYPE ssfcresop.
  DATA: gt_objbin LIKE solisti1 OCCURS 0 WITH HEADER LINE.

  lf_formname = p_formname.
*打印参数定义
  ls_control_param-langu     =  sy-langu.              "调用中文版的smartforms
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

*处理数据
  CLEAR: lt_h,lt_h[].
  LOOP AT item WHERE mark = 'X'.
    lt_h-kunnr = item-kunnr.
    lt_h-name1 = item-name1.
    lt_h-vgbel = item-vgbel.
    lt_h-stras = item-stras.
    lt_h-vbeln = item-vbeln.
    lt_h-bezei = item-bezei.
    "lt_h-ernam = item-ernam.
    lt_h-ertxt = item-ertxt.
    lt_h-untxt = item-untxt.
    lt_h-mbdat = item-mbdat.
    lt_h-vkorg = item-vkorg.
    lt_h-ktext = item-ktext.
    COLLECT lt_h.

  ENDLOOP.
********
  IF lt_h[] IS NOT INITIAL.
    CLEAR: itek,itek[].
    SELECT lips~vgbel likp~kunnr  likp~vkorg likp~erzet lips~vbeln lips~matnr
    lips~lfimg lips~erdat  lips~posnr bwart vbak~spart knvv~bzirk vbak~vtweg vbak~augru vbak~auart vbak~ktext
    kna1~name1 sdabw likp~ernam lips~lgort
    INTO CORRESPONDING FIELDS OF TABLE itek
    FROM lips
    INNER JOIN vbak ON lips~vgbel = vbak~vbeln
    INNER JOIN knvv ON ( knvv~kunnr = vbak~kunnr AND knvv~vkorg = vbak~vkorg  AND knvv~vtweg = vbak~vtweg )
    INNER JOIN likp ON ( likp~vbeln = lips~vbeln )
    INNER JOIN kna1 ON kna1~kunnr = likp~kunnr
    INNER JOIN mara ON mara~matnr = lips~matnr
    WHERE  likp~vkorg = p_vkorg
    AND vbak~spart = '30'
    AND lips~vgbel IN s_vbeln
    AND lips~vbeln IN p_vbeln
    AND likp~kunnr IN p_kuunr
    AND likp~erdat IN p_erdat
    AND  likp~wadat_ist = '00000000'
    AND  likp~sdabw IN s_sdabw
    AND  likp~ernam  IN s_ernam
    AND lips~lgort IN s_lgort
    AND knvv~bzirk IN p_bzirk.


    LOOP AT lt_h.
      READ TABLE itek WITH KEY kunnr = lt_h-kunnr.
      IF sy-subrc = '0'.
        lt_h-zxrir = '宣传品'.
        MODIFY lt_h.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lt_h[] IS NOT INITIAL.
    CLEAR: itek,itek[].
    SELECT lips~vgbel likp~kunnr  likp~vkorg likp~erzet lips~vbeln lips~matnr
    lips~lfimg lips~erdat  lips~posnr bwart vbak~spart knvv~bzirk vbak~vtweg vbak~augru vbak~auart vbak~ktext
    kna1~name1 sdabw likp~ernam lips~lgort
    INTO CORRESPONDING FIELDS OF TABLE itek
    FROM lips
    INNER JOIN vbak ON lips~vgbel = vbak~vbeln
    INNER JOIN knvv ON ( knvv~kunnr = vbak~kunnr AND knvv~vkorg = vbak~vkorg  AND knvv~vtweg = vbak~vtweg )
    INNER JOIN likp ON ( likp~vbeln = lips~vbeln )
    INNER JOIN kna1 ON kna1~kunnr = likp~kunnr
    INNER JOIN mara ON mara~matnr = lips~matnr
    WHERE  likp~vkorg = p_vkorg
    AND vbak~spart = '20'
    AND lips~vgbel IN s_vbeln
    AND lips~vbeln IN p_vbeln
    AND likp~kunnr IN p_kuunr
    AND likp~erdat IN p_erdat
    AND  likp~wadat_ist = '00000000'
    AND  likp~sdabw = '50'
    AND  likp~ernam  IN s_ernam
    AND lips~lgort IN s_lgort
    AND knvv~bzirk IN p_bzirk.

    LOOP AT lt_h.
      READ TABLE itek WITH KEY kunnr = lt_h-kunnr.
      IF sy-subrc = '0'.
        lt_h-zpwjm = '配件'.
        MODIFY lt_h.
      ENDIF.
    ENDLOOP.


  ENDIF.

********
  DATA: s_i TYPE i .
  LOOP AT lt_h.
    CLEAR: lft_i,lft_i[],lft_h,lft_h[],s_i.
    LOOP AT item WHERE vbeln = lt_h-vbeln AND mark = 'X' .
      MOVE-CORRESPONDING item TO lft_i.
      s_i = s_i + 1.
      lft_i-ebelx = s_i.
      APPEND lft_i.
      CLEAR: item,lft_i.
    ENDLOOP.

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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF jobinfo-outputdone = 'X'.
*  打印结束后处理数据
*    LOOP AT lt_h.
*      p_zjlst-zdbls = lt_h-zdbls.
*      APPEND p_zjlst.
*    ENDLOOP.
*
*    MODIFY ztlsdh FROM p_ztlsdh.
*    MODIFY zsjlst FROM TABLE p_zjlst.
*    MODIFY zzscdj FROM TABLE s_zzscdj.
*    COMMIT WORK.
*    MESSAGE s001(00) WITH '保存数据成功'.
*    CLEAR:s_zzscdj,s_zzscdj[],p_zjlst,p_zjlst[].
    LOOP AT lt_h.
      CLEAR: lt_zstdy,lt_zstdy[].
      LOOP AT item WHERE vbeln = lt_h-vbeln.
        lt_zstdy-vbeln = item-vbeln.
        lt_zstdy-posnr = item-posnr.
        SELECT SINGLE zdycs INTO lt_zstdy-zdycs FROM zstdy
          WHERE vbeln = item-vbeln AND posnr = item-posnr.
        lt_zstdy-zdycs = lt_zstdy-zdycs + 1.
        lt_zstdy-ernam = sy-uname.
        lt_zstdy-erdat = sy-datum.
        lt_zstdy-cputm = sy-uzeit.
        APPEND lt_zstdy.
        item-mark = ''.
        item-zdycs = lt_zstdy-zdycs.
        MODIFY item TRANSPORTING mark zdycs.
        CLEAR:lt_zstdy.
      ENDLOOP.
    ENDLOOP.
    MODIFY zstdy FROM TABLE lt_zstdy[].

  ENDIF.
ENDFORM.                    " FRM_PRINT
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DYMX
*&---------------------------------------------------------------------*
FORM frm_show_dymx .
  lt_dynpro-uname = '打印人' .
  lt_dynpro-datum = '打印日期'.
  lt_dynpro-uzeit = '打印时间'.
  COLLECT lt_dynpro .

  CONCATENATE '发货单' wa_i-vbeln '打印明细' INTO l_msg.

  CLEAR: lt_msg,lt_msg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_msg
    FROM zstdy WHERE vbeln = wa_i-vbeln AND posnr = wa_i-posnr.

  CALL FUNCTION 'G_DISPLAY_SELECTION_DYNPRO'
    EXPORTING
      dynp_title     = l_msg
      show_also_1    = 'X'
      sel_title1     = lt_dynpro
      start_column   = 25
      start_row      = 5
      number_of_rows = 15
    TABLES
      sel_table      = lt_msg
    EXCEPTIONS
      no_lines       = 1
      no_line_picked = 2
      OTHERS         = 3.

ENDFORM.                    " FRM_SHOW_DYMX

*&---------------------------------------------------------------------*
*&      Form  get_sernr
*&---------------------------------------------------------------------*
FORM get_sernr USING vbeln posnr .
  SELECT lief_nr posnr obzae sernr
    INTO CORRESPONDING FIELDS OF TABLE it_ser01
    FROM ser01 INNER JOIN objk ON ser01~obknr = objk~obknr
    WHERE ser01~lief_nr = vbeln AND
          ser01~posnr = posnr.
ENDFORM. " get_sernr