class lcl_202101 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.
endclass.

class lcl_202101 implementation.
  method lif_solver~part1.
    data: lv_result type i,
          lv_prev type i,
          lv_curr type i.

    field-symbols: <lv_line> type string.
    loop at me->t_input assigning <lv_line>.
      if sy-tabix > 1.
        lv_prev = lv_curr.
      endif.
      lv_curr = <lv_line>.

      if sy-tabix > 1 and lv_curr > lv_prev.
        lv_result = lv_result + 1.
      endif.
    endloop.

    rv_return = lv_result.
  endmethod.

  method lif_solver~part2.
    data: lv_window0 type i,
          lv_window1 type i,
          lv_window2 type i,
          lv_window3 type i,
          lv_result type i.

    field-symbols: <lv_line> type string.
    loop at me->t_input assigning <lv_line>.
      lv_window0 = lv_window1.
      lv_window1 = lv_window2 + <lv_line>.
      lv_window2 = lv_window3 + <lv_line>.
      lv_window3 = <lv_line>.

      if sy-tabix > 3 and lv_window1 > lv_window0.
        lv_result = lv_result + 1.
      endif.
    endloop.

    rv_return = lv_result.
  endmethod.
endclass.
