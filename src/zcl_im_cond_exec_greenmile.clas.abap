class ZCL_IM_COND_EXEC_GREENMILE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_STARTCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COND_EXEC_GREENMILE IMPLEMENTATION.


  METHOD if_ex_eval_startcond_ppf~evaluate_start_condition.
*    check 1 = 2.

*** WMDO - INICIO -  22-08-22 - Inclusão de validação de NF
* Variáveis
    DATA : lv_param_fo TYPE ze_param_low.

* Objetos
    DATA : lo_container TYPE REF TO /bofu/cl_ppf_container.

* Types
    TYPES : BEGIN OF ty_notas,
              remessa TYPE c LENGTH 10,
              fatura  TYPE c LENGTH 10,
              docnum  TYPE j_1bdocnum,
              cfop    TYPE j_1bnflin-cfop,
              status  TYPE j_1bstatuscode,
              nfe_key TYPE c LENGTH 44,
              nftype  TYPE j_1bnfdoc-nftype,
              nfenum  TYPE j_1bnfdoc-nfenum,
              series  TYPE j_1bnfdoc-series,
              docdat  TYPE j_1bnfdoc-docdat,
              partyp  TYPE j_1bnfdoc-partyp,
              branch  TYPE j_1bnfdoc-branch,
              parid   TYPE j_1bnfdoc-parid,
              inco1   TYPE j_1bnfdoc-inco1,
            END OF ty_notas .

    TYPES: BEGIN OF ty_taxanf,
             docnum TYPE j_1bdocnum,
             itmnum TYPE j_1bitmnum,
             taxtyp TYPE j_1btaxtyp,
             base   TYPE j_1bbase,
             rate   TYPE j_1btxrate,
             taxval TYPE j_1btaxval,
             excbas TYPE j_1bexcbas,
             othbas TYPE j_1bothbas,
             stattx TYPE j_1bstattx,
           END OF ty_taxanf .

    TYPES: ty_taxnf_t TYPE SORTED TABLE OF ty_taxanf WITH UNIQUE KEY docnum itmnum taxtyp .

*   Tabela de notas fiscais eletrônicas
    TYPES: ty_notas_t TYPE SORTED TABLE OF ty_notas WITH NON-UNIQUE KEY remessa
                                               WITH NON-UNIQUE SORTED KEY status COMPONENTS status .

    TYPES : BEGIN OF ty_vbeln,
              vbeln TYPE likp-vbeln,
            END OF ty_vbeln.

    TYPES: ty_lr_param_fo TYPE RANGE OF char2.

* Tabelas interna
    DATA : lt_remessas TYPE ty_notas_t,
           lt_items    TYPE /scmtms/t_tor_item_tr_k,
           lt_messages TYPE /scmtms/t_symsg,
           lt_taxanf   TYPE ty_taxnf_t,
           lt_root     TYPE /scmtms/t_tor_root_k,
           lt_vbeln    TYPE TABLE OF ty_vbeln,
           lt_msg      TYPE /scmtms/t_symsg,
           lr_param_fo TYPE ty_lr_param_fo.

    TRY.
        IF flt_val = 'ZCOND_EXEC_GREENMILE'.
          lo_container ?= io_context->appl.

          DATA(lo_config) = NEW zclca_tabela_parametros( ).

          TRY.
              lo_config->m_get_range(
                EXPORTING
                  iv_modulo = 'TM'
                  iv_chave1 = 'COND_EXPED_GREENMILE'
                IMPORTING
                  et_range  = lr_param_fo ).
            CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
          ENDTRY.


*          lo_config->m_get_single(
*            EXPORTING
*              iv_modulo = 'TM'
*              iv_chave1 = 'COND_EXPED_GREENMILE'
*            IMPORTING
*              ev_param  = lv_param_fo ).

*          IF lv_param_fo IS NOT INITIAL.
          IF lr_param_fo IS NOT INITIAL.
            /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->retrieve( EXPORTING iv_node_key  = /scmtms/if_tor_c=>sc_node-root
                                            it_key       = VALUE #( ( key = lo_container->get_bo_root_key( ) ) )
                                            iv_edit_mode = /bobf/if_conf_c=>sc_edit_read_only
                                            iv_fill_data = abap_true
                                  IMPORTING eo_change    = DATA(lo_change)
                                            et_data      = lt_root ).

            IF lt_root IS NOT INITIAL.

              "Instanciar classe para envio da ordem de frete ao GKO
              DATA(lo_gko) = NEW zcltm_interface_fo_gko( ).
              DATA(lv_ok) = lo_gko->check_all_ok( EXPORTING iv_tor_key   = lt_root[ 1 ]-key " NodeID
                                                            iv_interface = '02'             " Interface GreenMile
                                                  IMPORTING et_messages  = lt_msg ).        " System Messages

              IF lv_ok EQ abap_false.
                ep_rc = 4.
                RETURN.
              ENDIF.

              IF NOT lt_root[ 1 ]-zz1_cond_exped IN lr_param_fo.
                ep_rc = 4.
                RETURN.
              ENDIF.

              IF NOT line_exists( lt_root[ blk_exec = abap_false ] ). "#EC CI_SORTSEQ
                ep_rc = 4.
                RETURN.
              ENDIF.

*              /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->retrieve_by_association(
*                EXPORTING
*                  iv_node_key             = /scmtms/if_tor_c=>sc_node-root
*                  it_key                  = VALUE #( ( key = lo_container->get_bo_root_key( ) ) )
*                  iv_association          = /scmtms/if_tor_c=>sc_association-root-item_tr
*                  iv_fill_data            = abap_true
*                IMPORTING
*                  et_data                 = lt_items ).
*
*              IF lt_items IS NOT INITIAL.
*                LOOP AT lt_items INTO DATA(ls_items).
*                  IF ls_items-base_btd_id IS INITIAL.
*                    CONTINUE.
*                  ENDIF.
*                  APPEND VALUE #( vbeln = ls_items-base_btd_id+25(10) ) TO lt_vbeln.
*                ENDLOOP.
*
*                DELETE ADJACENT DUPLICATES FROM lt_vbeln.
*
*                IF lt_vbeln IS NOT INITIAL.
*                  SELECT *
*                    FROM likp
*                    INTO TABLE @DATA(lt_likp)
*                    FOR ALL ENTRIES IN @lt_vbeln
*                  WHERE vbeln = @lt_vbeln-vbeln
*                    AND fkstk <> 'C'.
*
*                  IF sy-subrc IS INITIAL.
*                    ep_rc = 4.
*                    RETURN.
*                  ENDIF.
*                ENDIF.
*              ENDIF.

            ENDIF.
          ENDIF.

          "Validar se todas as notas fiscais estão autorizadas na SEFAZ
*          ep_rc = abap_false.
*          CLEAR : lt_remessas.
*
*          SELECT DISTINCT right( items~base_btd_id, 10 ) AS remessa, fluxo~vbeln AS fatura, nfe~docnum AS doc_nfe, nfe~cfop AS cfop, status~code AS status,
*                          concat( status~regio, concat( status~nfyear, concat( status~nfmonth, concat( status~stcd1, concat( status~model,
*                          concat( status~serie, concat( status~nfnum9, concat( status~docnum9, status~cdv ) ) ) ) ) ) ) ) AS nfe_key,
*                          hd~nftype, hd~nfenum, hd~series, hd~docdat, hd~partyp, hd~branch, hd~parid, hd~inco1
*              FROM @lt_items AS items
*              LEFT JOIN vbfa AS fluxo ON fluxo~vbelv = right( items~base_btd_id, 10 ) AND
*                                         fluxo~vbtyp_v = 'J'   AND
*                                         fluxo~vbtyp_n = 'M'
*              LEFT JOIN j_1bnflin AS nfe ON nfe~reftyp = 'BI' AND
*                                            nfe~refkey = fluxo~vbeln
*              LEFT JOIN j_1bnfe_active AS status ON status~docnum = nfe~docnum
*              LEFT JOIN j_1bnfdoc AS hd ON hd~docnum = nfe~docnum
*              WHERE items~base_btd_tco = '73'
*                AND status~cancel = @abap_false
*              INTO TABLE @lt_remessas.

*          LOOP AT lt_remessas ASSIGNING FIELD-SYMBOL(<fs_remessa_invalida>)
*                              WHERE status <> '100'.
*            ep_rc = 4.
*            lt_messages = VALUE #( BASE lt_messages ( msgno = '006' msgty = 'E' msgv1 = <fs_remessa_invalida>-remessa ) ).
*          ENDLOOP.

*          IF lt_remessas IS NOT INITIAL AND ep_rc = abap_false.
*            CLEAR lt_taxanf.
*
*            SELECT  docnum , itmnum , taxtyp , base   , rate   , taxval , excbas , othbas , stattx
*              FROM j_1bnfstx FOR ALL ENTRIES IN @lt_remessas
*            WHERE docnum = @lt_remessas-docnum
*              INTO TABLE @lt_taxanf.
*
*            IF sy-subrc = 0.
**        SORT gt_taxanf BY docnum itmnum taxtyp.
*            ENDIF.
*          ENDIF.
        ENDIF.
      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
    ENDTRY.
*** WMDO - FIM -  22-08-22 - Inclusão de validação de NF

  ENDMETHOD.
ENDCLASS.
