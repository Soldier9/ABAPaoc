class lcl_202201 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    data: t_elves type standard table of i.
endclass.

class lcl_202201 implementation.
  method lif_solver~part1.
    field-symbols: <lv_line> type string,
                   <lv_elf> type i.

    append initial line to me->t_elves assigning <lv_elf>.
    loop at me->t_input assigning <lv_line>.
      if <lv_line> is initial.
        append initial line to me->t_elves assigning <lv_elf>.
      else.
        <lv_elf> = <lv_elf> + <lv_line>.
      endif.
    endloop.

    sort me->t_elves descending.
    read table me->t_elves index 1 assigning <lv_elf>.
    rv_return = <lv_elf>.
  endmethod.

  method lif_solver~part2.
    field-symbols: <lv_elf> type i.

    loop at me->t_elves from 1 to 3 assigning <lv_elf>.
      rv_return = rv_return + <lv_elf>.
    endloop.
  endmethod.
endclass.
