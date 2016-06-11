*----------------------------------------------------------------------*
***INCLUDE ZMM220_FRM_GET_JSKCF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_JSKC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_jskc .
  SELECT afko~aufnr
         afko~dispo
         resb~werks
         resb~rsnum
         resb~rspos
         resb~matnr
         resb~bdmng
         resb~meins
         resb~xloek
         resb~ablad
         resb~lgort       "车间库
         marc~beskz
         jest~stat
         equk~qunum
    INTO CORRESPONDING FIELDS OF TABLE it_resb
    FROM afko INNER JOIN aufk ON afko~aufnr = aufk~aufnr
              LEFT JOIN  jest ON aufk~objnr = jest~objnr AND
                                 jest~stat = 'I0045'     AND
                                 jest~inact = ''
              INNER JOIN resb ON afko~rsnum = resb~rsnum
              INNER JOIN marc ON resb~matnr = marc~matnr AND
                                 resb~werks = marc~werks
              INNER JOIN mard ON resb~matnr = mard~matnr AND
                                 resb~werks = mard~werks AND
                                 mard~lgort = s_umlgo-low
              LEFT JOIN  equk ON resb~matnr = equk~matnr AND
                                 resb~werks = equk~werks AND
                                 equk~bdatu >= sy-datum AND
                                 equk~vdatu <= sy-datum
    WHERE afko~aufnr IN s_aufnr AND
          ( marc~beskz = 'F' OR ( marc~beskz = 'X' AND resb~ablad = '外购' ) ) AND
          mard~lgpbe IN s_lgpbe
  AND marc~loggr <> 'A1'.

  DELETE it_resb WHERE stat = '' AND xloek = 'X'. "TECO状态的工单 允许组件是删除状态
  SORT it_resb BY aufnr rsnum rspos.
  READ TABLE it_resb INDEX 1.
  it_resb-aufn2 = it_resb-aufnr.
  aufn2 = it_resb-aufnr.
  MODIFY it_resb TRANSPORTING aufn2 WHERE aufn2 = ''.

***处理订单重复处理的问题和锁定
  CLEAR: it_zjsdh[],yt_aufnr[].
  LOOP AT it_resb.
    it_aufnr-aufnr = it_resb-aufnr.
    COLLECT it_aufnr.
    CASE it_resb-lgort.
      WHEN '2001'.
        it_resb-lgfsb = '2005'.
      WHEN '2002'.
        it_resb-lgfsb = '2006'.
      WHEN '2003'.
        it_resb-lgfsb = '2007'.
      WHEN '2004'.
        it_resb-lgfsb = '2008'.
      WHEN '2009'.
        it_resb-lgfsb = '2010'.
    ENDCASE.
    it_resb-umlgo = s_umlgo-low.
    SELECT SINGLE lgpbe INTO it_resb-lgpbe
    FROM mard WHERE matnr = it_resb-matnr AND werks = it_resb-werks
                                          AND lgort = it_resb-umlgo.

    MODIFY it_resb TRANSPORTING lgfsb umlgo lgpbe.        "二级库
  ENDLOOP.
  LOOP AT it_aufnr.
    CALL FUNCTION 'ENQUEUE_ESORDER'
      EXPORTING
        aufnr          = it_aufnr-aufnr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.
  ENDLOOP.

  SELECT DISTINCT zjsdh INTO TABLE it_zjsdh FROM zrmm220t2
    FOR ALL ENTRIES IN it_aufnr
    WHERE zrmm220t2~aufnr = it_aufnr-aufnr.
  IF it_zjsdh[] IS NOT INITIAL.
    SELECT DISTINCT aufnr INTO TABLE yt_aufnr FROM zrmm220t2
      FOR ALL ENTRIES IN it_zjsdh
      WHERE zjsdh = it_zjsdh-zjsdh.

    SORT it_aufnr.
    SORT yt_aufnr.
    IF it_aufnr[] <> yt_aufnr[].
      MESSAGE s000(oo) WITH '重复打印输入的工单号和上一次不一致' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      second = 'X'.
    ENDIF.
  ENDIF.

  it_resb-color = 'C300'.
  MODIFY it_resb TRANSPORTING color WHERE lgpbe = '' OR qunum = ''.
  IF sy-subrc = 0.
    MESSAGE s000(oo) WITH '黄色底纹标识的仓管员为空，或者没有维护配额'.
    disable = 'X'.
  ENDIF.

*计算寄售库存
  DATA: lt_save  LIKE it_save[] WITH HEADER LINE ,
        lt_saves LIKE it_save[] WITH HEADER LINE.
  DATA: incom      TYPE meico ,
        inpreissim TYPE meprck ,
        expreissim TYPE mepro .

  CLEAR: it_save,it_save[],it_second[],it_sec[],it_trd[].
  LOOP AT it_resb.
    it_save-matnr = it_resb-matnr.
    it_save-werks = it_resb-werks.
    it_save-umlgo = it_resb-umlgo.   "寄售库
    it_save-lgpbe = it_resb-lgpbe.
    it_save-bdmng = it_resb-bdmng.
    it_save-meins = it_resb-meins.
    it_save-aufn2 = aufn2.
    it_save-dispo = it_resb-dispo.
    COLLECT it_save.
  ENDLOOP.
  SORT it_save.
************************************************* basis1,20160309
  IF second = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_second FROM zrmm220t1
      WHERE aufn2 = aufn2 AND xloek = ''.
*****************basis,参考已过账数量
    LOOP AT it_second WHERE zcdhgz IS NOT INITIAL .
      it_sec-matnr = it_second-matnr.
      it_sec-werks = it_second-werks.
      it_sec-menge = it_second-menge.
      it_sec-meins = it_second-meins.
      COLLECT it_sec.
    ENDLOOP.
*****************basis,参考未过账数量
    LOOP AT it_second WHERE zcdhgz IS INITIAL .
      it_trd-matnr = it_second-matnr.
      it_trd-werks = it_second-werks.
      it_trd-bdmng = it_second-bdmng.
      it_trd-meins = it_second-meins.
      COLLECT it_trd.
    ENDLOOP.
    LOOP AT it_save.
      READ TABLE it_sec WITH KEY matnr = it_save-matnr werks = it_save-werks."过账数量
      IF  sy-subrc = 0.
        it_save-bdmng = it_save-bdmng - it_sec-menge.
      ENDIF.
      READ TABLE it_trd WITH KEY matnr = it_save-matnr werks = it_save-werks."未过账数量
      IF  sy-subrc = 0.
        it_save-bdmng = it_save-bdmng - it_trd-bdmng.
      ENDIF.
      MODIFY it_save.
    ENDLOOP.
  ENDIF.
  DELETE it_save WHERE bdmng <= 0.
  IF it_save[] IS INITIAL .
    MESSAGE s000(oo) WITH '寄售库存不足，无法合并.或者自有库存足够，不需要寄售库存.或者已打印' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


***获取配额数据
  SELECT matnr werks bdatu sobes lifnr quote
    INTO CORRESPONDING FIELDS OF itab_equp
    FROM equk INNER JOIN equp ON equk~qunum = equp~qunum
    FOR ALL ENTRIES IN it_save
    WHERE matnr = it_save-matnr AND
          werks = it_save-werks AND
          bdatu >= sy-datum AND
          vdatu <= sy-datum AND
          sobes IN ('0','2') AND
          beskz = 'F'  .
    IF itab_equp-sobes = '0'.
      CLEAR itab_equp-lifnr.
    ENDIF.
    CLEAR itab_equp-sobes.
    itab_equp-dispo = it_save-dispo.
    COLLECT itab_equp.

    itab_quall-matnr = itab_equp-matnr.
    itab_quall-quall = itab_equp-quote.
    COLLECT itab_quall.

    CLEAR itab_equp.
    CLEAR itab_quall.
  ENDSELECT.

***取每个供应商的配额所占百分比、寄售价格、寄售库存，并排序
  LOOP AT itab_equp.
    READ TABLE itab_quall WITH KEY matnr = itab_equp-matnr.

    itab_equp-quotp = itab_equp-quote / itab_quall-quall * 100.

    IF itab_equp-lifnr <> ''. "寄售价格和寄售库存
      incom-esokz = '2'.
      incom-ekorg = '6000'.
      incom-werks = itab_equp-werks.
      incom-lifnr = itab_equp-lifnr.
      incom-matnr = itab_equp-matnr.

      SELECT SINGLE meins INTO inpreissim-simme
      FROM mara
      WHERE matnr = itab_equp-matnr.
      inpreissim-simng = 1.
      inpreissim-sidat = sy-datum.
      inpreissim-bwsv1 = '2'.

      CLEAR expreissim.
      CALL FUNCTION 'ME_READ_INFORECORD'
        EXPORTING
          incom             = incom
          inpreissim        = inpreissim
        IMPORTING
          expreissim        = expreissim
        EXCEPTIONS
          bad_comin         = 1
          bad_material      = 2
          bad_materialclass = 3
          bad_supplier      = 4
          not_found         = 5
          OTHERS            = 6.
      IF sy-subrc = 0 AND expreissim-peinh <> 0.
        itab_equp-preis = expreissim-preis / expreissim-peinh.
      ELSE.
        itab_equp-preis = expreissim-preis .
      ENDIF.
      SELECT SINGLE slabs INTO itab_equp-slabs
      FROM mkol
      WHERE matnr = itab_equp-matnr AND
      werks = itab_equp-werks AND
      lgort = s_umlgo-low  AND
      sobkz = 'K'     AND
      lifnr = itab_equp-lifnr.
    ENDIF.

    MODIFY itab_equp.
  ENDLOOP.
  SORT itab_equp BY matnr quotp DESCENDING preis .

***分配
  DATA: menge TYPE i . "IT_SAVE的一行分配后还剩下的数量
  LOOP AT it_save.
    CLEAR lt_save[].
    lt_save = it_save.
    menge   = it_save-bdmng.
    LOOP AT itab_equp WHERE matnr = it_save-matnr AND werks = it_save-werks.
      lt_save-lifnr = itab_equp-lifnr.
*****begin*****basis1,20160611
      DATA: l_zdtyl LIKE ztbzps-zdtyl.
      DATA: l_int TYPE i.
      DATA: l_p TYPE p.
      CLEAR: l_zdtyl,l_int.
      SELECT SINGLE zdtyl INTO l_zdtyl FROM ztbzps WHERE werks = itab_equp-werks AND matnr = itab_equp-matnr.
      IF sy-subrc = 0 AND l_zdtyl IS NOT INITIAL.
        l_p = ( it_save-bdmng * itab_equp-quotp / 100 ) / l_zdtyl.
        IF frac( l_p ) GE '0.5'.
          l_int = trunc( l_p ) + 1.
        ELSE.
          l_int = trunc( l_p ).
        ENDIF.
        CLEAR: it_save-bdmng.
        it_save-bdmng = l_int * l_zdtyl.
      ELSE.
        lt_save-bdmng = trunc( it_save-bdmng * itab_equp-quotp / 100 ) . "取整
      ENDIF.
*****end*****basis1,20160611
      IF itab_equp-lifnr = ''. "非寄售，使用自有库存
        READ TABLE hs_mard WITH TABLE KEY matnr = it_save-matnr
                                          werks = it_save-werks
                                          lgort = it_save-umlgo.
        IF sy-subrc = 0.
          IF lt_save-bdmng >= hs_mard-labst. "自有库存不够或者相等
            lt_save-bdmng = hs_mard-labst.
            DELETE TABLE hs_mard.
          ELSE.
            hs_mard-labst = hs_mard-labst - lt_save-bdmng.
            MODIFY TABLE hs_mard.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSE.
        IF lt_save-bdmng > itab_equp-slabs . "如果寄售库存不够
          IF itab_equp-slabs <> 0. "并且库存不为零，BDMNG=库存
            lt_save-bdmng = itab_equp-slabs.
            DELETE itab_equp.  "库存变为零以后，删除此条目，当menge不为零的时候，不允许此条参与运算
          ELSE. "库存为零，跳过
            CONTINUE.

          ENDIF.
        ELSE.
          itab_equp-slabs = itab_equp-slabs - lt_save-bdmng. "减去这部分库存
          MODIFY itab_equp.
        ENDIF.
        APPEND lt_save.
      ENDIF.

      menge = menge - lt_save-bdmng.
    ENDLOOP.

    IF menge <> 0 . "没有分配完，找有库存的寄售供应商再分配
      lt_save = it_save.
      LOOP AT itab_equp WHERE matnr = it_save-matnr AND
                              werks = it_save-werks AND
                              lifnr <> ''           AND
                              quotp <> 100          AND  "basis1,20160317,添加配额100的限制
                              slabs <> 0 .
        lt_save-lifnr = itab_equp-lifnr.
        IF itab_equp-slabs >= menge.
          lt_save-bdmng = menge.
          COLLECT lt_save.

          itab_equp-slabs = itab_equp-slabs - menge.
          MODIFY itab_equp.

          EXIT.
        ELSE.
          lt_save-bdmng = itab_equp-slabs.
          COLLECT lt_save.

          menge = menge - itab_equp-slabs.

          itab_equp-slabs = 0 .
          MODIFY itab_equp.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lt_save[] IS NOT INITIAL.
      APPEND LINES OF lt_save TO lt_saves.
    ENDIF.
  ENDLOOP.

  IF lt_saves[] IS INITIAL.
    MESSAGE s000(oo) WITH '寄售库存不足，无法合并。或者自有库存足够，不需要寄售库存' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    it_save[] = lt_saves[].
  ENDIF.
  DELETE it_save WHERE bdmng = 0.
*  LOOP AT it_save.
*    SELECT SINGLE maktx INTO it_save-makts FROM makt WHERE matnr = it_save-matnr.
*    SELECT SINGLE name1 INTO it_save-name1 FROM lfa1 WHERE lifnr = it_save-lifnr.
*    MODIFY it_save.
*  ENDLOOP.
ENDFORM.                    " FRM_GET_JSKC