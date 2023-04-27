class ZCL_IM_COND_EXEC_GKO definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_STARTCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COND_EXEC_GKO IMPLEMENTATION.


  method IF_EX_EVAL_STARTCOND_PPF~EVALUATE_START_CONDITION.
    ep_rc = 4.
  endmethod.
ENDCLASS.
