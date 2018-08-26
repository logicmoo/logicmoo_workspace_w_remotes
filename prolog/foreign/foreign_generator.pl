/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(foreign_generator, [generate_library/4,
                              gen_foreign_library/2]).

:- use_module(library(lists)).
:- use_module(library(foldnl)).
:- use_module(library(assertions)).
:- use_module(library(extend_args)).
:- use_module(library(extra_messages)).
:- use_module(library(assrt_lib)).
:- use_module(library(call_ref)).
:- use_module(library(foreign/foreign_props)).
:- use_module(library(metaprops)).
:- use_module(library(apply)).
:- use_module(library(atomics_atom)).
:- use_module(library(camel_snake)).
:- use_module(library(key_value)).
:- use_module(library(transpose)).
:- use_module(library(implementation_module)).

:- multifile
    gen_foreign_library/2,
    use_foreign_source/2,
    use_foreign_header/2,
    include_foreign_dir/2,
    library_foreign_dir/2,
    extra_compiler_opts/2,
    link_foreign_library/2,
    pkg_foreign_config/2.

:- dynamic
    gen_foreign_library/2,
    use_foreign_source/2,
    use_foreign_header/2,
    include_foreign_dir/2,
    extra_compiler_opts/2,
    link_foreign_library/2,
    pkg_foreign_config/2.

:- meta_predicate current_foreign_prop(1,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?).

command_to_atom(Command, Args, Atom) :-
    process_create(path(Command), Args, [stdout(pipe(Out))]),
    read_stream_to_codes(Out, String),
    string_to_atom(String, Atom).

fortran_command(M, path(gfortran), ValueL, ValueT) :-
    command_to_atom(swipl, ['--dump-runtime-variables'], Atom),
    atomic_list_concat(AtomL, ';\n', Atom),
    findall(Value, ( ( member(NameValue, AtomL),
                       member(NameEq, ['PLCFLAGS="', 'PLLDFLAGS="']),
                       atomics_atom([NameEq, Values, '"'], NameValue)
                     ; extra_compiler_opts(M, Values)
                     ),
                     atomic_args(Values, ValueL1),
                     member(Value, ValueL1)
                   ),
            ValueL, ValueT).

intermediate_obj(M, DirSO, Source, Object) -->
    {intermediate_obj(M, DirSO, Source, Object, Command)}, !,
    [Command].
intermediate_obj(_, _, Source, Source) --> [].

intermediate_obj(M, DirSO, Source, Object, Fortran-Args) :-
    file_name_extension(Base, for, Source),
    file_base_name(Base, Name),
    file_name_extension(Name, o, NameO),
    directory_file_path(DirSO, NameO, Object),
    fortran_command(M, Fortran, Args, ['-c', Source, '-o', Object]).

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

generate_library(M, AliasSO, AliasSOPl, File) :-
    absolute_file_name(AliasSO, FileSO, [file_type(executable),
                                         relative_to(File)]),
    findall(FSource, ( ( use_foreign_source(M, FAlias)
                       ; FAlias = library('foreign/foreign_interface.c')
                       ; FAlias = library('foreign/foreign_swipl.c')
                       ),
                       absolute_file_name(FAlias, FSource,
                                          [extensions(['.c', '']),
                                           access(read),
                                           relative_to(File)])
                     ), FSourceL),
    ( forall(( member(Dep, [File|FSourceL])
             ; ( use_foreign_header(M, HAlias)
               ; HAlias = library('foreign/foreign_interface.h')
               ; HAlias = library('foreign/foreign_swipl.h')
               ),
               absolute_file_name(HAlias, Dep,
                                  [extensions(['.h','']),
                                   access(read),
                                   relative_to(File)])
             ; member(Alias, [library(foreign/foreign_generator),
                              library(foreign/foreign_props),
                              library(foreign/foreign_interface)
                             ]),
               absolute_file_name(Alias, Dep, [file_type(prolog),
                                               access(read),
                                               relative_to(File)])
             ),
             is_newer(FileSO, Dep))
    ->print_message(informational,
                    format('Skipping build of ~w: is up to date', [FileSO]))
    ; do_generate_library(M, FileSO, File, FSourceL),
      do_generate_wrapper(M, AliasSO, AliasSOPl, File)
    ).

do_generate_wrapper(M, AliasSO, AliasSOPl, File) :-
    findall(F/A, ( current_foreign_prop(_, Head, M, _, _, _, _, _, _, _, _, _, _),
                   \+ ( predicate_property(M:Head, number_of_clauses(X)),
                        X>0
                      ),
                   functor(Head, F, A)
                 ), IntfPIU),
    sort(IntfPIU, IntfPIL),
    atom_concat(M, '$impl', IModule),
    absolute_file_name(AliasSOPl, FileSOPl, [file_type(prolog),
                                             relative_to(File)]),
    % assertion(file_name_extension(_, pl, FileSOPl)),
    save_to_file(FileSOPl,
                 phrase(( add_autogen_note(M),
                          [(:- module(IModule, IntfPIL))],
                          generate_aux_clauses(M),
                          ['',
                           (:- use_foreign_library(AliasSO))]
                        ))).

atomic_args(String, ArgL) :-
    atomic_list_concat(ArgL1, ' ', String),
    subtract(ArgL1, [''], ArgL).

do_generate_library(M, FileSO, File, FSourceL) :-
    file_name_extension(BaseFile, _, FileSO),
    generate_foreign_interface(M, File, BaseFile),
    absolute_file_name(library(foreign/foreign_interface),
                       IntfPl,
                       [file_type(prolog), access(read), relative_to(File)]),
    directory_file_path(DirIntf, _, IntfPl),
    directory_file_path(DirSO,   _, FileSO),
    atom_concat(BaseFile, '_intf.c', IntfFile),
    foldl(intermediate_obj(M, DirSO), FSourceL, FTargetL, Commands, CommandsT),
    append(FTargetL, [IntfFile|CLibL], FArgsT),
    findall(CLib, ( link_foreign_library(M, Lib),
                    atom_concat('-l', Lib, CLib)
                  ; pkg_foreign_config(M, Package),
                    command_to_atom('pkg-config', ['--libs', Package], CLib1),
                    atom_concat(CLibs, '\n', CLib1),
                    atomic_args(CLibs, CLibL1),
                    member(CLib, CLibL1)
                  ), CLibL, ['-o', FileSO]),
    findall(COpt, ( ( extra_compiler_opts(M, COpts)
                    ; pkg_foreign_config(M, Package),
                      command_to_atom('pkg-config', ['--cflags', Package], COpt1),
                      atom_concat(COpts, '\n', COpt1)
                    ),
                    atomic_args(COpts, COptL1),
                    member(COpt, COptL1)
                  ), COptL, IDirL),
    findall(IDir, ( ( Dir = DirSO
                    ; Dir = DirIntf
                    ; include_foreign_dir(M, DAlias),
                      absolute_file_name(DAlias, Dir, [file_type(directory),
                                                       relative_to(File)])
                    ),
                    atom_concat('-I', Dir, IDir)
                  ),
            IDirL, LDirL),
    findall(LDir, ( library_foreign_dir(M, DAlias),
                    absolute_file_name(DAlias, Dir, [file_type(directory),
                                                     relative_to(File)]),
                    atom_concat('-L', Dir, LDir)
                  ),
            LDirL, FArgsT),
    CommandsT = [path('swipl-ld')-['-shared'|COptL]],
    forall(member(Command-ArgL, Commands),
           compile_1(Command, ArgL)).

compile_1(Command, ArgL) :-
    process_create(Command, ArgL, [stdout(pipe(Out)),
                                   stderr(pipe(Err))]),
    read_string(Err, _, SErr),
    read_string(Out, _, SOut),
    close(Err),
    command_to_string(Command, ArgL, CommandS),
    catch(( close(Out),
            print_message(informational, format('~s', [CommandS]))
          ),
          Error,
          ( print_message(error, Error),
            print_message(error, format("~s~s~nCommand: ~s", [SOut, SErr, CommandS]))
          )).

command_to_string(Command, ArgL, CommandS) :-
    ( Command = path(RCommand)
    ->true
    ; RCommand = Command
    ),
    atomic_list_concat([RCommand|ArgL], ' ', CommandS).

:- meta_predicate with_output_to_file(+,0 ).

with_output_to_file(File, Goal) :- setup_call_cleanup(tell(File), Goal, told).

write_lines([]) :- !.
write_lines([E|L]) :- !,
    write_lines(E),
    write_lines(L).
write_lines(Line) :-
    write_line(Line).

write_line(Line) :-
    ( nonvar(Line),
      do_write_line_2(Line)
    ->true
    ; writeln(Line)
    ).

write_line_1(Line) :-
    ( nonvar(Line),
      do_write_line_1(Line)
    ->true
    ; write(Line)
    ).

do_write_line_1(F-A) :- format(F, A).
do_write_line_1(A+B) :-
    write_line_1(A),
    write_line_1(B).

do_write_line_2((:- A))    :- portray_clause((:- A)).
do_write_line_2((A :- B))  :- portray_clause((A :- B)).
do_write_line_2((A --> B)) :- portray_clause((A --> B)).
do_write_line_2(Line) :- write_line_1(Line), nl.

:- meta_predicate save_to_file(+,2).

save_to_file(File, Goal) :-
    call(Goal, Lines, []),
    with_output_to_file(File, write_lines(Lines)).

generate_foreign_interface(Module, FilePl, BaseFile) :-
    atom_concat(BaseFile, '_impl', BaseFileImpl),
    file_name_extension(BaseFileImpl, h, FileImpl_h),
    atom_concat(BaseFile, '_intf', BaseFileIntf),
    file_name_extension(BaseFileIntf, h, FileIntf_h),
    file_name_extension(BaseFileIntf, c, FileIntf_c),
    directory_file_path(_, Base, BaseFile),
    save_to_file(FileImpl_h, generate_foreign_impl_h(Module)),
    save_to_file(FileIntf_h, generate_foreign_intf_h(Module, FileImpl_h)),
    save_to_file(FileIntf_c, generate_foreign_c(Module, Base, FilePl, FileIntf_h)).

c_var_name(Arg, CArg) :-
    format(atom(CArg), '_c_~w', [Arg]).

generate_foreign_intf_h(Module, FileImpl_h) -->
    add_autogen_note(Module),
    ["#ifndef __"+Module+"_INTF_H",
     "#define __"+Module+"_INTF_H",
     '',
     '',
     '#include <foreign_swipl.h>',
     "#include \""+FileImpl_h+"\"",
     '',
     "extern module_t __"+Module+"_impl;"],
    findall_tp(Module, type_props_nf, declare_type_getter_unifier),
    findall('extern '+Decl+';',
            ( current_foreign_prop(_, Head, _, Module, _, _, _, _, Dict, _, _, BindName, _, Type),
              apply_dict(Head, Dict),
              declare_intf_head(Type, BindName, Head, Decl)
            )),
    ['',
     "#endif /* __"+Module+"_INTF_H */"].

declare_intf_head(fimport(_), BindName, _, Decl) :-
    !,
    declare_intf_fimp_head(BindName, Decl).
declare_intf_head(fimport(_, _), BindName, _, Decl) :-
    !,
    declare_intf_fimp_head(BindName, Decl).
declare_intf_head(_, BindName, Head, Decl) :-
    declare_intf_head(BindName, Head, Decl).

declare_intf_fimp_head(BindName, "predicate_t "+BindName+"").

generate_foreign_impl_h(Module) -->
    add_autogen_note(Module),
    ["#ifndef __"+Module+"_IMPL_H~n#define __"+Module+"_IMPL_H",
     '',
     '#include <foreign_interface.h>'],
    findall_tp(Module, type_props, declare_struct),
    declare_foreign_bind(Module),
    ["#endif /* __"+Module+"_IMPL_H */"].

add_autogen_note(Module) -->
    ["/* NOTE: File generated automatically from "+Module+" */",
     ''].

generate_foreign_c(Module, Base, FilePl, FileIntf_h) -->
    add_autogen_note(Module),
    findall("#include \""+File_h+"\"",
            ( use_foreign_header(Module, HAlias),
              absolute_file_name(HAlias, File_h, [extensions(['.h', '']),
                                                  access(read),
                                                  relative_to(FilePl)])
            )),
    ["#include \""+FileIntf_h+"\"",
     '',
     "module_t __"+Module+";",
     "module_t __"+Module+"_impl;"
    ],
    findall_tp(Module, type_props_nf, implement_type_getter),
    findall_tp(Module, type_props_nf, implement_type_unifier),
    generate_foreign_register(Module, Base),
    generate_foreign_intf(Module).

generate_foreign_register(Module, Base) -->
    ["install_t install_"+Base+"() {",
     '    __system_dict_create        =PL_predicate("dict_create", 3, "system");',
     '    __system_get_dict           =PL_predicate("get_dict",    3, "system");',
     '    __system_put_dict           =PL_predicate("put_dict",    4, "system");',
     '    __foreign_generator_call_idx=PL_predicate("call_idx",    2, "foreign_generator");',
     "    __"+Module+"     =PL_new_module(PL_new_atom(\""+Module+"\"));",
     "    __"+Module+"_impl=PL_new_module(PL_new_atom(\""+Module+"$impl\"));"],
    findall_tp(Module, type_props_nf, define_aux_variables),
    findall(Line,
            ( current_foreign_prop(_, _, M, Module, _, _, _, _, _, _, PredName, BindName, Arity, Type),
              write_register_sentence(Type, M, PredName, Arity, BindName, Line))),
    ["} /* install_"+Base+" */",
    ''].

write_register_sentence(fimport(_),    M, PredName, Arity, BindName, Line) :- !,
    write_init_fimport_binding(M, PredName, Arity, BindName, Line).
write_register_sentence(fimport(_, _), M, PredName, Arity, BindName, Line) :- !,
    write_init_fimport_binding(M, PredName, Arity, BindName, Line).
write_register_sentence(_, _, PredName, Arity, BindName,
                        '    PL_register_foreign(\"~w\", ~w, ~w, 0);'
                        -[PredName, Arity, BindName]).

write_init_fimport_binding(M, PN, A, BN,
                           '    ~w = PL_predicate("~w", ~w, "~w");'-[BN, PN, A, M]).

:- meta_predicate findall_tp(+,5,5,?,?).

findall_tp(Module, TypeProps, Call) -->
    findall(List,
            ( call(TypeProps, Module, Type, TypePropLDictL, Pos, _Asr),
              maplist(apply_dict_tp, TypePropLDictL),
              phrase(type_components(Module, Type, TypePropLDictL, Call, Pos), List)
            )).

apply_dict_tp(t(Type, PropL, Dict)) :- apply_dict(Type-PropL, Dict).

type_props(M, Type, TypePropLDictL, Pos, Asr) :-
    type_props_(M, Type, TDict, Pos, Asr),
    collect_prop(Asr, M, comp, TPropL),
    ( TPropL \= []
    ->TypePropLDictL = [t(Type, TPropL, TDict)]
    ; bind_type_names(M:Type, TypePropLDictL)
    ->true
    ; TypePropLDictL = [t(Type, [], TDict)]
    ).

type_props_(CM, Type, Dict, Pos, Asr) :-
    % We use asr_head_prop instead of prop_asr since the auto-generated types
    % should be only those defined in the current module, but not others that
    % could be imported in CM --EMM
    asr_head_prop(Asr, CM, Type, check, prop, Dict, Pos),
    once(prop_asr(glob, type(_), _, Asr)).

type_props_nf(Module, Type, TypePropLDictL, Pos, Asr) :-
    type_props(Module, Type, TypePropLDictL, Pos, Asr),
    % Don't create getters and unifiers for
    % typedefs, they are just casts:
    \+ type_is_tdef(Module, Type, _, _),
    \+ prop_asr(glob, foreign(_, _), _, Asr),
    \+ prop_asr(glob, native(_, _), _, Asr).

define_aux_variables(dict_ini(Name, M, _, _), _, _) -->
    !,
    ['    __rtcwarn((__~w_aux_keyid_index_~w=PL_pred(PL_new_functor(PL_new_atom("__aux_keyid_index_~w"), 2), __~w_impl))!=NULL);'-[M, Name, Name, M]].
define_aux_variables(dict_key_value(_, _, _, _), _, _) --> !, {fail}.
define_aux_variables(_, _, _) --> [].

implement_type_getter_ini(PName, CName, Spec, Name) -->
    {ctype_decl(Spec, Decl, [])},
    ['int FI_get_~w(root_t __root, term_t ~w, ~s *~w) {'-[Name, PName, Decl, CName]].

c_get_argument_getter(Spec, CNameArg, PNameArg, GetArg) :-
    c_get_argument(Spec, in, CNameArg, PNameArg, GetArg).

implement_type_getter(union_ini(Spec, L), Term, Name) -->
    ( {L = [_, _|_]}
    ->{term_pcname(Term, Name, PName, CName)},
      implement_type_getter_ini(PName, CName, Spec, Name),
      ['    term_t __args = PL_new_term_refs(2);',
       '    int __utype;',
       "    PL_put_term(__args, "+PName+");",
       '    __rtcheck(__rtctype(PL_call_predicate(NULL, PL_Q_NORMAL,',
       '                                          __foreign_generator_call_idx, __args),',
       "                        __args, \"Not a valid "+Name+"\"));",
       '    __rtcheck(PL_get_integer(__args + 1, &__utype));',
       "    "+CName+"->utype=__utype;",
       "    switch ("+CName+"->utype) {"]
    ; []
    ).
implement_type_getter(union_end(L), _, _) -->
    ( ( {L = [_, _|_]}
      ->['    default:',
         '        return FALSE;',
         '    };']
      ; {L \= [t(_, [], _)]}
      )
    ->implement_type_end
    ; []
    ).
implement_type_getter(func_ini(Spec, L), Term, Name) -->
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ['    case ~s_~s:'-[Name, TName],
       '    {']
    ; {func_pcname(Name, PName, CName)},
      implement_type_getter_ini(PName, CName, Spec, Name)
    ).
implement_type_getter(func_rec(N, Term, Name, L), Spec, Arg) -->
    { L = [_, _|_]
    ->functor(Term, TName, _),
      format(atom(CRecordName), '~w.~w', [TName, Arg]),
      format(atom(TNameArg), '~w_~w', [TName, Arg]),
      camel_snake(PRecordName, TNameArg),
      Indent = '        '
    ; CRecordName = Arg,
      camel_snake(PRecordName, Arg),
      Indent = '    '
    },
    { func_pcname(Name, PName, CName),
      format(atom(CNameArg), '&~w->~w', [CName, CRecordName]),
      format(atom(PNameArg), '~w_~w',   [PName, PRecordName])
    },
    [Indent+'term_t '+PNameArg+'=PL_new_term_ref();',
     Indent+'__rtcheck(PL_get_arg('+N+','+PName+','+PNameArg+'));'],
    {c_get_argument_getter(Spec, CNameArg, PNameArg, GetArg)},
    [Indent+GetArg+';'].
implement_type_getter(func_end(L), _, _) -->
    ( {L = [_, _|_]}
    ->['        break;',
       '    }']
    ; []
    ).
implement_type_getter(atom(Name, L), Spec, Term) -->
    {functor(Term, TName, _)},
    ( {L = [_, _|_]}
    ->{ func_pcname(Name, PName, CName1),
        format(atom(CName), '~w->~w', [CName1, TName]),
        Indent = '        '
      },
      ['    case ~s_~s:'-[Name, TName]]
    ; { func_pcname(Name, PName, CName),
        Indent = '    '
      },
      implement_type_getter_ini(PName, CName, Spec, TName)
    ),
    { (\+is_type(Spec)->atom_concat('&', CName, CArg);CArg=CName),
      c_get_argument_getter(Spec, CArg, PName, GetArg)
    },
    [Indent+GetArg+';'],
    ( {L = [_, _|_]}
    ->[Indent+'break;']
    ; []
    ).
implement_type_getter(dict_ini(Name, M, _, L), Spec, Term) -->
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ['    case ~s_~s:'-[Name, TName],
       '    {']
    ; ["predicate_t __"+M+"_aux_keyid_index_"+Name+";"],
      {term_pcname(Term, Name, PName, CName)},
      %% TBD: This will fail for structures with dict_t
      implement_type_getter_dict_ini(M, PName, CName, Spec, Name)
    ).
implement_type_getter(dict_key_value(Dict, _, N, _), Key, Value) -->
    {key_value_from_dict(Dict, N, Key, Value)}.
implement_type_getter(dict_rec(_, Term, N, Name, L), Spec, Arg) -->
    { ( L = [_, _|_]
      ->functor(Term, TName, _),
        format(atom(CRecordName), '~w.~w', [TName, Arg]),
        Indent = '        '
      ; CRecordName = Arg,
        Indent = '    '
      ),
      term_pcname(Term, Name, PName, CName),
      format(atom(CNameArg), '&~w->~w', [CName, CRecordName]),
      c_get_argument_getter(Spec, CNameArg, PName, GetArg)
    },
    [Indent+'    case '+N+': '+GetArg+'; break;'].
implement_type_getter(dict_end(_, _, L), _, _) -->
    ['        }'],
    ( {L = [_, _|_]}
    ->['        break;',
       '    }']
    ; []
    ).

implement_type_getter_dict_ini(Module, PName, CName, Spec, Name) -->
    {ctype_decl(Spec, Decl, [])},
    ['static int get_pair_~w(root_t, term_t, term_t, ~s *);'-[Name, Decl],
     ''],
    implement_type_getter_ini(PName, CName, Spec, Name),
    ['    memset(~w, 0, sizeof(~s));'-[CName, Decl],
     '    FI_get_dict_t(~w, ~w, ~w);'-[Name, PName, CName]
    ],
    implement_type_end,
    ['static int',
     'get_pair_~w(root_t __root, term_t __keyid, term_t ~w, ~s *~w) {'
     -[Name, PName, Decl, CName],
     '    int __index;',
     "    FI_get_keyid_index(__"+Module+"_aux_keyid_index_"+Name
     +", __keyid, __index);",
     '    switch (__index) {'].

implement_type_end -->
    ['    return TRUE;',
     '}',
     ''].

term_pcname(Term, NameL, PName, CName) :-
    ( compound(Term)
    ->functor(Term, Func, _)
    ; Func = Term
    ),
    ( valid_csym(Func)
    ->Name = Func
    ; Name = NameL
    ),
    func_pcname(Name, PName, CName).

func_pcname(NameL, PName, CName) :-
    ( is_list(NameL)
    ->atomic_list_concat(NameL, Name)
    ; Name = NameL
    ),
    camel_snake(PName, Name),
    c_var_name(Name, CName).

type_char(Type, Char) :- char_type(Char, Type).

valid_csym(Func) :-
    atom_codes(Func, Codes),
    maplist(type_char(csym), Codes).

implement_type_unifier(atom(Name, _), Spec, Term) -->
    {functor(Term, TName, _)},
    ( {L = [_, _|_]}
    ->{ func_pcname(Name, PName, CName1),
        format(atom(CName), '~w->~w', [CName1, TName]),
        Indent = '        '
      },
      ['    case ~s_~s:'-[Name, TName]]
    ; { func_pcname(Name, PName, CName),
        Indent = '    '
      },
      implement_type_unifier_ini(PName, CName, Name, Spec)
    ),
    {c_set_argument(Spec, inout, CName, PName, SetArg)},
    [Indent+SetArg+';'],
    ( {L = [_, _|_]}
    ->[Indent+'break;']
    ; []
    ).
implement_type_unifier(union_ini(Spec, TPDL), Term, Name) -->
    {term_pcname(Term, Name, PName, CName)},
    ( {TPDL = [_, _|_]}
    ->implement_type_unifier_ini(PName, CName, Name, Spec),
      ["    switch ("+CName+"->utype) {"]
    ; []
    ).
implement_type_unifier(union_end(TPDL), _, _) -->
    ( ( {TPDL = [_, _|_]}
      ->['    default:',
         '        return FALSE;',
         '    };']
      ; {TPDL \= [t(_, [], _)]}
      )
    ->implement_type_end
    ; []
    ).
implement_type_unifier(func_ini(Spec, L), Term, Name) -->
    {func_pcname(Name, PName, CName)},
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ['    case ~s_~s:'-[Name, TName],
       '    {']
    ; implement_type_unifier_ini(PName, CName, Name, Spec)
    ),
    {functor(Term, Func, Arity)},
    ['    __rtcheck(PL_unify_functor(~w, PL_new_functor(PL_new_atom("~w"), ~d)));'
     -[PName, Func, Arity]].
implement_type_unifier(func_rec(N, Term, Name, L), Spec, Arg) -->
    { func_pcname(Name, PName, CName),
      ( L = [_, _|_]
      ->functor(Term, TName, _),
        format(atom(CRecordName), '~w.~w', [TName, Arg]),
        format(atom(TNameArg), '~w_~w', [TName, Arg]),
        camel_snake(PRecordName, TNameArg),
        Indent = '        '
      ; CRecordName = Arg,
        camel_snake(PRecordName, Arg),
        Indent = '    '
      ),
      format(atom(CNameArg), '~w->~w', [CName, CRecordName]),
      format(atom(PNameArg), '~w_~w',  [PName, PRecordName])
    },
    [Indent+'term_t '+PNameArg+'=PL_new_term_ref();',
     Indent+'__rtcheck(PL_get_arg('+N+','+PName+','+PNameArg+'));'],
    {c_set_argument(Spec, out, CNameArg, PNameArg, SetArg)},
    [Indent+SetArg+';'].
implement_type_unifier(func_end(L), _, _) -->
    ( {L = [_, _|_]}
    ->['        break;',
       '    }']
    ; []
    ).
implement_type_unifier(dict_ini(Name, _, _, L), Spec, Term) -->
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ['    case ~s_~s:'-[Name, TName],
       '    {']
    ; {func_pcname(Term, PName, CName)},
      implement_type_unifier_ini(PName, CName, Name, Spec)
    ),
    ['    term_t __desc=PL_new_term_ref();',
     '    term_t __tail=PL_copy_term_ref(__desc);'].
implement_type_unifier(dict_key_value(Dict, _, N, _), Key, Value) -->
    {key_value_from_dict(Dict, N, Key, Value)}. % Placed in 'dict' order
implement_type_unifier(dict_rec(_, Term, _N, Name, L), Spec, Arg) -->
    { term_pcname(Term, Name, PName, CName),
      ( L = [_, _|_]
      ->functor(Term, TName, _),
        format(atom(CRecordName), '~w.~w', [TName, Arg]),
        format(atom(TNameArg), '~w_~w', [TName, Arg]),
        camel_snake(PRecordName, TNameArg),
        Indent = '        '
      ; CRecordName = Arg,
        camel_snake(PRecordName, Arg),
        Indent = '    '
      ),
      format(atom(CNameArg), '~w->~w', [CName, CRecordName]),
      format(atom(PNameArg), '~w_~w',  [PName, PRecordName])
    },
    ( {spec_pointer(Spec)}
    ->[Indent+'if('+CNameArg+') {']
    ; [Indent+'{']
    ),
    [Indent+'    term_t '+PNameArg+'=PL_new_term_ref();'],
    {c_set_argument(Spec, out, CNameArg, PNameArg, SetArg)},
    [Indent+'    '+SetArg+';',
     Indent+'    FI_put_desc(__tail, "'+Arg+'", '+PNameArg+');',
     '    }'].
implement_type_unifier(dict_end(_, Tag, L), Term, _) -->
    {func_pcname(Term, PName, _)},
    ['    __rtcheck(PL_unify_nil(__tail));',
     "    FI_dict_create("+PName+", \""+Tag+"\", __desc);"],
    ( {L = [_, _|_]}
    ->['        break;',
       '    }']
    ; []
    ).

spec_pointer(chrs(_)).
spec_pointer(ptr(_)).
spec_pointer(pointer-_).
spec_pointer(list(_)).
spec_pointer(tdef(_, Spec)) :- spec_pointer(Spec).
% spec_pointer(type(_)).

implement_type_unifier_ini(PName, CName, Term, Spec) -->
    {ctype_decl(Spec, Decl, [])},
    ['int FI_unify_~w(term_t ~w, ~s* const ~w) {'
     -[Term, PName, Decl, CName]].

apply_name(Name=Value) :-
    camel_snake(Name, Arg),
    ignore(Value=Arg).

apply_dict(Head, Dict) :-
    maplist(apply_name, Dict),
    term_variables(Head, Vars),
    fg_numbervars(Vars, 1, Dict).

fg_numbervars([], _, _).
fg_numbervars([V|Vs], N, Dict) :-
    format(atom(T), 'var_~d', [N]),
    succ(N, N1),
    ( memberchk(_=T, Dict)
    ->fg_numbervars([V|Vs], N1, Dict)
    ; V=T,
      fg_numbervars(Vs, N1, Dict)
    ).

bind_type_names(MType, TypeMPropLDictL) :-
    predicate_property(MType, interpreted),
    strip_module(MType, _, Type),
    findall(t(Type, MPropL, Dict),
            bind_tn_clause(MType, MPropL, Dict),
            TypeMPropLDictL).

:- meta_predicate
    bind_tn_clause(0, -, -).

bind_tn_clause(MType, MPropL, Dict) :-
    strip_module(MType, M, Type),
    catch(clause(MType, Body, Ref), _, fail),
    ( clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line)),
      get_dictionary(Type :- Body, File, Line, M, Dict)
    ->true
    ; Dict = []
    ),
    clause_property(Ref, module(CM)),
    sequence_list(Body, PropL, []),
    maplist(cond_qualify_with(CM), PropL, MPropL).

ds_union_ini_1(Name, Idx, t(Type, _, _)) -->
    { functor(Type, _, N),
      arg(N, Type, Term),
      functor(Term, TName, _)
    },
    ['    ~s_~s = ~d,'-[Name, TName, Idx]].

declare_struct(union_ini(_, TPDL), _, Name) -->
    ( {TPDL = [_, _|_]}
    ->['typedef enum {'],
      foldnl(ds_union_ini_1(Name), 1, TPDL),
      ['} ~s_utype;'-[Name],
       'struct ~s {'-[Name],
       '  ~s_utype utype;'-[Name],
       '  union {'
      ]
    ; []
    ).
declare_struct(union_end(TPDL), _, _) -->
    ( {TPDL = [_, _|_]}
    ->['  };',
       '};'
      ]
    ; []
    ).
declare_struct(atom(Name, L), Spec, Term) -->
    {ctype_decl(Spec, Decl, [])},
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ['    ~s ~w;'-[Decl, TName]]
    ; ['typedef ~s ~w;'-[Decl, Name]]
    ).
declare_struct(func_ini(Spec, L), _, _) -->
    ( {L = [_, _|_]}
    ->{Decl = "  struct"}
    ; {ctype_decl(Spec, Decl, [])}
    ),
    ['~s {'-[Decl]].
declare_struct(func_end(L), Term, _) -->
    ( {L = [_, _|_]}
    ->{functor(Term, TName, _)},
      ["    } "+TName+";"]
    ; ['};']
    ).
declare_struct(func_rec(_, _, _, _), Spec, Name) -->
    {ctype_decl(Spec, Decl, [])},
    ['    ~s ~w;'-[Decl, Name]].
%%
declare_struct(dict_ini(_, _, _, _), Spec, _) -->
    {ctype_decl(Spec, Decl, [])},
    ['',
     '~s {'-[Decl]].
declare_struct(dict_key_value(Dict, Desc, N, _), Key, Value) -->
    {key_value_from_desc(Dict, Desc, N, Key, Value)}.
declare_struct(dict_rec(_, _, _, _, _), Spec, Name) -->
    {ctype_decl(Spec, Decl, [])},
    ['    ~s ~w;'-[Decl, Name]].
declare_struct(dict_end(_, _, _), _, _) --> ['};'].

declare_type_getter_unifier(atom(_, _), _, _) --> [].
    % declare_type_getter_unifier(Name, Spec).
declare_type_getter_unifier(union_ini(Spec, L), _, Name) -->
    ( {L = [_, _|_]}
    ->declare_type_getter_unifier(Name, Spec)
    ; []
    ).
declare_type_getter_unifier(union_end(_), _, _) --> [].
declare_type_getter_unifier(func_ini(Spec, L), _, Name) -->
    ( {L = [_, _|_]}
    ->[]
    ; declare_type_getter_unifier(Name, Spec)
    ).
declare_type_getter_unifier(func_end(_), _, _) --> [].
declare_type_getter_unifier(func_rec(_, _, _, _), _, _) --> [].
declare_type_getter_unifier(dict_ini(Name, M, _, _), _, _) -->
    ["predicate_t __"+M+"_aux_keyid_index_"+Name+";"].
declare_type_getter_unifier(dict_end(_, _, _), _, _) --> [].
declare_type_getter_unifier(dict_rec(_, _, _, _, _), _, _) --> [].

declare_type_getter_unifier(Name, Spec) -->
    {ctype_decl(Spec, Decl, [])},
    ['int FI_get_~w(root_t __root, term_t, ~s*);'-[Name, Decl],
     'int FI_unify_~w(term_t, ~s* const);'-[Name, Decl],
     ''].

generate_aux_clauses(Module) -->
    findall_tp(Module, type_props, generate_aux_clauses).

% This will create an efficient method to convert keys to indexes in the C side,
% avoiding string comparisons.
generate_aux_clauses(dict_ini(Name, _, _, _), _, _) -->
    !,
    {atom_concat('__aux_keyid_index_', Name, F)},
    [(:- public F/2)].
generate_aux_clauses(dict_key_value(Dict, _, N, _), Key, Value) -->
    !,
    {key_value_from_dict(Dict, N, Key, Value)}.
generate_aux_clauses(dict_rec(_, _, N, Name, _), _, Key) -->
    !,
    { atom_concat('__aux_keyid_index_', Name, F),
      Pred =.. [F, Key, N]
    },
    [(Pred :- true)].
generate_aux_clauses(_, _, _) --> [].

:- multifile
    prolog:message//1.

prolog:message(ignored_type(Name, Arg)) -->
    [""+Name+"->"+Arg+" ignored"].

prolog:message(failed_binding(TypeComponents)) -->
    ['~w failed'-[TypeComponents]].

:- meta_predicate type_components(+,+,+,5,+,?,?).

type_components(M, Type, TypePropLDictL, Call, Loc) -->
    {functor(Type, Name, _)},
    call(Call, union_ini(type(Name), TypePropLDictL), Type, Name),
    foldl(type_components_one(M, Name, Call, TypePropLDictL, Loc),
          TypePropLDictL),
    call(Call, union_end(TypePropLDictL), Type, _).

type_components_one(M, Name, Call, TPLDL, Loc, t(Type, PropL, _)) -->
    { functor(Type, _, Arity),
      arg(Arity, Type, Term)
    },
    ( {compound(Term)}
    ->call(Call, func_ini(type(Name), TPLDL), Term, Name),
      findall(Lines,
              ( arg(N, Term, Arg),
                phrase(( { member(Prop, PropL),
                           match_known_type_(Prop, M, Name, Spec, Arg)
                         },
                         call(Call, func_rec(N, Term, Name, TPLDL), Spec, Arg)
                       ->[]
                       ; {print_message(warning, at_location(Loc, ignored_type(Name, Arg)))}
                       ), Lines)
              )),
      call(Call, func_end(TPLDL), Term, Name)
    ; { select(dict_t(Desc, Term), PropL, PropL1)
      ; select(dict_t(Tag, Desc, Term), PropL, PropL1)
      ; select(dict_join_t(Tag, Type1, Type2, Term), PropL, PropL1),
        join_dict_types(Type1, M, Type2, M, Tag, Desc)
      ; select(dict_extend_t(Term, Type, Tag, Desc2), PropL, PropL1),
        join_type_desc(M:Type, Tag, Desc2, Desc)
      }
    ->{ is_dict(Desc, Tag)
      ->Dict=Desc
      ; dict_create(Dict, Tag, Desc)
      },
      {ignore(Tag = Name)},
      call(Call, dict_ini(Name, M, Dict, TPLDL), type(Name), Term),
      findall(Lines,
              phrase(( call(Call, dict_key_value(Dict, Desc, N, Name), Arg, Value),
                       ( { fetch_kv_prop_arg(Arg,  M, Value, PropL1, Prop),
                           match_known_type_(Prop, M, Name, Spec, Arg)
                         },
                         call(Call, dict_rec(M, Term, N, Name, TPLDL), Spec, Arg)
                       ->[]
                       ; {print_message(warning, at_location(Loc, ignored_type(Name, Arg)))}
                       )), Lines)),
      call(Call, dict_end(M, Tag, TPLDL), Term, Name)
    ; { member(Prop, PropL),
        match_known_type_(Prop, M, Name, Spec, Term)
      }
    ->call(Call, atom(Name, TPLDL), Spec, Term)
    ; {PropL = []}
    ->[]
    ),
    !.
type_components_one(M, N, G, TPLDL, Loc, TPLD) -->
    {print_message(
         error,
         at_location(
             Loc,
             failed_binding(type_components_one(M, N, G, TPLDL,
                                                Loc, TPLD))))}.

key_value_from_dict(Dict, N, Key, Value) :-
    S = s(0),
    Value=Dict.Key,
    S = s(N),
    succ(N, N2),
    nb_setarg(1, S, N2).

key_value_from_list(Desc, N, Key, Value) :-
    nth0(N, Desc, KeyValue),
    key_value(KeyValue, Key, Value).

key_value_from_desc(_, Desc, N, Key, Value) :-
    is_list(Desc), !,
    key_value_from_list(Desc, N, Key, Value).
key_value_from_desc(Dict, _, N, Key, Value) :-
    key_value_from_dict(Dict, N, Key, Value).

fetch_kv_prop_arg(Key, CM, Value, PropL, M:Prop) :-
    ( member(MProp, PropL),
      strip_module(CM:MProp, M, Prop),
      functor(Prop, _, N),
      arg(N, Prop, Key)
    ; extend_args(Value, [Key], Prop),
      M=CM
    ).

declare_intf_head(PCN, Head, ("foreign_t "+PCN+"(")+ArgS+')') :-
    ( compound(Head)
    ->findall(Txt, ( arg(_, Head, Arg),
                     format(atom(Txt), 'term_t ~w', [Arg])
                   ), TxtL),
      atomic_list_concat(TxtL, ', ', ArgS)
    ; true
    ).

declare_foreign_bind(CM) -->
    findall(Line+';',
            ( read_foreign_properties(Head, M, CM, Comp, Call, Succ, Glob, Bind, _),
              declare_impl_head(Head, M, CM, Comp, Call, Succ, Glob, Bind, Line)
           )).

declare_impl_head(Head, M, CM, Comp, Call, Succ, Glob, Bind, Type+FHD) :-
    ( member(RS, [returns_state(_), type(_)]),
      memberchk(RS, Glob)
    ->Type = 'int ',       % int to avoid SWI-Prolog.h dependency at this level
      CHead = Head
    ; member(returns(Var, _), Glob)
    ->bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Var, Spec, Mode),
      ctype_arg_decl(Spec, Mode, Decl, []),
      Type = '~s '-[Decl],
      Head =.. Args,
      once(select(Var, Args, CArgs)),
      CHead =.. CArgs
    ; Type = 'void ',
      CHead = Head
    ),
    declare_foreign_head(CHead, M, CM, Comp, Call, Succ, Glob, Bind, FHD),
    !.

declare_foreign_head(Head, M, CM, Comp, Call, Succ, Glob, (CN/_ as _ + _),
                     CN+'('+Args+')') :-
    phrase(( ( {memberchk(memory_root(_), Glob)}
             ->['root_t __root']
             ; []
             ),
             ( {compound(Head)}
             ->declare_foreign_bind_(1, M, CM, Head, Comp, Call, Succ, Glob)
             ; []
             )
           ), ArgL, []),
    atomic_list_concat(ArgL, ', ', Args).

declare_foreign_bind_(N, M, CM, Head, Comp, Call, Succ, Glob) -->
    {arg(N, Head, Arg)},
    declare_foreign_bind_arg(Head, M, CM, Comp, Call, Succ, Glob, Arg),
    {succ(N, N1)},
    !,
    declare_foreign_bind_(N1, M, CM, Head, Comp, Call, Succ, Glob).
declare_foreign_bind_(_, _, _, _, _, _, _, _) --> [].

declare_foreign_bind_arg(Head, M, CM, Comp, Call, Succ, Glob, Arg) -->
    {bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
     ctype_barg_decl(Spec, Mode, Decl, []),
     format(atom(A), '~s ~w', [Decl, Arg])
    },
    [A].

ctype_barg_decl(Spec, Mode) -->
    ctype_arg_decl(Spec, Mode),
    ({Mode = in, \+ is_type(Spec)} -> [] ; "*"),
    ({Mode = in} -> " const" ; []). % Ensure const correctness

ctype_arg_decl(Spec, Mode) -->
    ctype_decl(Spec),
    ({is_ref(Spec, Mode)} -> [] ; "*").

is_ref(term,    _) :- !.
is_ref(list(_), _) :- !.        % Always ref
is_ref(ptr(_),  _) :- !.        % Always ref
is_ref(chrs(_), _) :- !.
is_ref(_, in).
is_ref(_, out).
% is_ref(inout, _) :- fail.
% Allow pointer to NULL, the equivalent to free variables in imperative
% languages --EMM

is_type(type(_)).
is_type(tdef(_, Spec)) :- is_type(Spec).

ctype_decl(list(Spec))    --> ctype_decl(Spec), "*".
ctype_decl(ptr(Spec))     --> ctype_decl(Spec), "*".
ctype_decl(chrs(Name))    --> acodes(Name).
ctype_decl(type(Name))    --> "struct ", acodes(Name).
ctype_decl(term)          --> "term_t".
ctype_decl(tdef(Name, _)) --> acodes(Name).
ctype_decl(cdef(Name))    --> acodes(Name).
ctype_decl(_-CType)       --> acodes(CType).

acodes(Atom, List, Tail) :-
    atom_codes(Atom, Codes),
    append(Codes, Tail, List).

cond_qualify_with(CM, MProp, MProp) :-
    strip_module(CM:MProp, M, Prop),
    ( CM = M
    ->MProp = Prop
    ; MProp = M:Prop
    ).

foreign_native(foreign(_)).
foreign_native(foreign(_, _)).
foreign_native(native(_)).
foreign_native(native(_, _)).

current_foreign_prop(Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL, DictL,
                     FuncName, PredName, BindName, Arity) :-
    current_foreign_prop(foreign_native, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                         DictL, FuncName, PredName, BindName, Arity, _).

current_foreign_prop(Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL, DictL,
                     FuncName, PredName, BindName, Arity, Type) :-
    current_foreign_prop(foreign_native_fimport, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                         DictL, FuncName, PredName, BindName, Arity, Type).

:- meta_predicate collect(?,^,-).
collect(Tmpl, Goal, List) :-
    (bagof(Tmpl, Goal, List) *-> true ; List = []).

collect_props(Asr, CM, CompL, CallL, SuccL, GlobL) :-
    maplist(collect_prop(Asr, CM),
            [comp, call, succ, glob],
            [CompL, CallL, SuccL, GlobL]).

collect_prop(Asr, CM, Part, PropL) :-
    collect(MProp,
            (M, Prop, From)^( curr_prop_asr(Part, M:Prop, From, Asr),
                              ( M \= CM
                              ->MProp = M:Prop
                              ; MProp = Prop
                              )
                            ), PropL).

assertion_db(Asr, Head, M, CM, Status, Type, Comp, Call, Succ, Glob, Comm, Dict, Loc) :-
    asr_head_prop(Asr, CM, Head, Status, Type, Dict, Loc),
    ( curr_prop_asr(comm, Comm, _, Asr)
    ->true
    ; Comm = ""
    ),
    implementation_module(CM:Head, M),
    collect_props(Asr, CM, Comp, Call, Succ, Glob).

current_foreign_prop(GenKeyProp, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                     DictL, FuncName, PredName, BindName, Arity, KeyProp) :-
    asr_head_prop(Asr, Context, Head, check, Type, _, _),
    memberchk(Type, [pred, prop]),
    implementation_module(Context:Head, Module),
    once(( call(GenKeyProp, KeyProp),
           prop_asr(glob, KeyProp, _, Asr)
           % implementation_module(KM:KeyProp+1_extra_argument, KI)
         )),
    findall(Head-[MComp, MCall, MSucc, MGlob, Dict],
            ( assertion_db(_, Head, Module, CM, check, Type, Comp, Call, Succ,
                           Glob, _, Dict, _),
              maplist(maplist(cond_qualify_with(CM)),
                      [ Comp,  Call,  Succ,  Glob],
                      [MComp, MCall, MSucc, MGlob])
            ), KPropLL),
    maplist(=(Head-_), KPropLL),
    pairs_values(KPropLL, PropLL),
    transpose(PropLL, PropTL),
    maplist(append, PropTL, [CompU, CallU, SuccU, GlobU, DictL]),
    maplist(sort, [CompU, CallU, SuccU, GlobU], [CompL, CallL, SuccL, GlobL]),
    ( ( prop_asr(glob, native(_),    _, Asr)
      ; prop_asr(glob, native(_, _), _, Asr)
      )
    -> % Already considered
      \+ ( member(KeyProp2, [foreign(_), foreign(_, _)]),
           memberchk(KeyProp2, GlobL)
         )
    ; true
    ),
    functor(Head, PredName, Arity),
    ( memberchk(foreign(_), GlobL)
    ->FuncName = PredName
    ; memberchk(foreign(FuncName, _), GlobL)
    ->true
    ; memberchk(fimport(_), GlobL)
    ->FuncName = PredName
    ; memberchk(fimport(FuncName, _), GlobL)
    ->true
    ; true
    ),
    ( memberchk(native(_), GlobL)
    ->BindName = PredName
    ; memberchk(native(BindName, _), GlobL)
    ->true
    ; nonvar(FuncName)
    ->atom_concat('pl_', FuncName, BindName)
    ).

foreign_native_fimport(H) :- foreign_native(H).
foreign_native_fimport(fimport(_)).
foreign_native_fimport(fimport(_, _)).

read_foreign_properties(Head, M, CM, Comp, Call, Succ, Glob, CN/A as PN/BN + CheckMode, T) :-
    current_foreign_prop(_Asr, Head, M, CM, Comp, Call, Succ, Glob, Dict, CN, PN, BN, A, T),
    nonvar(CN),
    ( memberchk(type(_), Glob)
    ->CheckMode=(type)
    ; CheckMode=pred
    ),
    apply_dict(Head, Dict).

generate_foreign_intf(Module) -->
    findall(Lines,
            ( read_foreign_properties(Head, M, Module, Comp, Call, Succ, Glob, Bind, Type),
              phrase(declare_intf_impl(Type, Head, M, Module, Comp, Call, Succ, Glob, Bind),
                     Lines))).

declare_intf_impl(fimport(_), Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    !,
    declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).
declare_intf_impl(fimport(_, _), Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    !,
    declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).
declare_intf_impl(_, Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    declare_forg_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).

declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    { Bind = (_/A as PN/BN + _),
      declare_intf_fimp_head(BN, BNHead)
    },
    [BNHead+'=NULL;'],
    {declare_impl_head(Head, M, Module, Comp, Call, Succ, Glob, Bind, ImplHead)},
    [ImplHead+' {',
     "    term_t "+BN+"_args = PL_new_term_refs("+A+");"],
    ( {memberchk(parent(Var, _), Glob)}
    ->["    __leaf_t *__root = LF_ROOT(LF_PTR(FI_array_ptr("+Var+")));"]
    ; []
    ),
    bind_outs_arguments(Head, M, Module, Comp, Call, Succ, Glob, Bind),
    ["} /* "+PN/A+" */",
     ''].

declare_forg_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    { Bind = (PI as _/PCN + CheckMode),
      declare_intf_head(PCN, Head, PCNH)
    },
    [PCNH+' {'],
    % If is variable then succeed (because is compatible)
    findall("    if(PL_is_variable("+Arg+")) return TRUE;",
            ( CheckMode==(type),
              arg(_, Head, Arg)
            )),
    ['    __mkroot(__root);'],
    bind_arguments(Head, M, Module, Comp, Call, Succ, Glob, Bind, Return),
    ['    __delroot(__root);',
     "    return "+Return+";",
     "} /* "+PI+" */",
     ''].

c_set_argument(list(S),    _, C, A, L) :- c_set_argument_rec(list, S, C, A, L).
c_set_argument(ptr( S),    _, C, A, L) :- c_set_argument_rec(ptr,  S, C, A, L).
c_set_argument(type(T),    M, C, A, L) :- c_set_argument_type(M, T, C, A, L).
c_set_argument(cdef(T),    M, C, A, L) :- c_set_argument_one(M, T, C, A, L).
c_set_argument(T-_,        M, C, A, L) :- c_set_argument_one(M, T, C, A, L).
c_set_argument(chrs(_),    M, C, A, L) :- c_set_argument_chrs(M, C, A, L).
c_set_argument(tdef(_, S), M, C, A, L) :- c_set_argument(S, M, C, A, L).
c_set_argument(term,       _, C, A, "__rtcheck(PL_unify("+A+", "+C+"))").

c_set_argument_one(out,   Type, CArg, Arg, '__rtc_FI_unify(~w, ~w, ~w)'-[Type, Arg, CArg]).
c_set_argument_one(inout, Type, CArg, Arg, 'FI_unify_inout(~w, ~w, ~w)'-[Type, Arg, CArg]).

c_set_argument_type(out,   Type, CArg, Arg, '__rtc_FI_unify(~w, ~w, &~w)'-[Type, Arg, CArg]).
c_set_argument_type(inout, Type, CArg, Arg, 'FI_unify_inout_type(~w, ~w, ~w)'-[Type, Arg, CArg]).

c_set_argument_chrs(out,   CArg, Arg, "__rtc_FI_unify(chrs, "+Arg+", "+CArg+")").
c_set_argument_chrs(inout, CArg, Arg, "FI_unify_inout_chrs("+Arg+", "+CArg+")").

c_set_argument_rec(Type, Spec, CArg, Arg, ("FI_unify_"+Type+"(")+L+(", "+Arg+", "+CArg+")")) :-
    format(atom(Arg_), '~w_', [Arg]),
    c_var_name(Arg_, CArg_),
    c_set_argument(Spec, out, CArg_, Arg_, L).

c_get_argument(list(S),    M, C, A, L) :- c_get_argument_rec(M, list, S, C, A, L).
c_get_argument(ptr(S),     M, C, A, L) :- c_get_argument_rec(M, ptr,  S, C, A, L).
c_get_argument(type(T),    M, C, A, L) :- c_get_argument_type(M, T, C, A, L).
c_get_argument(cdef(T),    M, C, A, L) :- c_get_argument_one(M, T, C, A, L).
c_get_argument(T-_,        M, C, A, L) :- c_get_argument_one(M, T, C, A, L).
c_get_argument(chrs(_),    M, C, A, L) :- c_get_argument_chrs(M, C, A, L).
c_get_argument(tdef(_, S), M, C, A, L) :- c_get_argument(S, M, C, A, L).
c_get_argument(term, _, C, A, "*"+C+"=PL_copy_term_ref("+A+")").

c_get_argument_one(in, Type, CArg, Arg, '__rtc_FI_get(~w, ~w, ~w)'-[Type, Arg, CArg]).
c_get_argument_one(inout, Type, CArg, Arg, 'FI_get_inout(~w, ~w, ~w)'-[Type, Arg, CArg]).

c_get_argument_type(in, Type, CArg, Arg, '__rtc_FI_get(~w, ~w, ~w)'-[Type, Arg, CArg]).
c_get_argument_type(inout, Type, CArg, Arg, 'FI_get_inout(~w, ~w, ~w)'-[Type, Arg, CArg]).

c_get_argument_chrs(in, CArg, Arg, '__rtc_FI_get(~w, ~w, ~w)'-[chrs, Arg, CArg]).
c_get_argument_chrs(inout, CArg, Arg, "FI_get_inout_chrs("+Arg+", "+CArg+")").

c_get_argument_rec(Mode, Type, Spec, CArg, Arg,
                   ("FI_get_"+Mode+"_"+Type+"(")+L+(", "+Arg+", "+CArg+")")) :-
    format(atom(Arg_), '~w_',   [Arg]),
    c_var_name(Arg_, CArg_),
    c_get_argument(Spec, in, CArg_, Arg_, L).

bind_arguments(Head, M, CM, Comp, Call, Succ, Glob, Bind, Return) -->
    ( {compound(Head)}
    ->findall('    '+("~s"-[Decl])+DN,
              ( arg(_, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                ctype_arg_decl(Spec, Mode, Decl, []),
                c_var_name(Arg, CArg),
                ( Spec = term
                ->DN=" "+CArg+"=PL_new_term_ref();"
                ; DN=" "+CArg+";"
                )
             )),
      findall('    '+GetArg+';',
              ( arg(_, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [in, inout]),
                c_var_name(Arg, CArg1),
                atom_concat('&', CArg1, CArg),
                c_get_argument(Spec, Mode, CArg, Arg, GetArg)
             ))
    ; []
    ),
    {generate_foreign_call(Bind-Head, M, CM, Comp, Call, Succ, Glob, Return, ForeignCall)},
    [ForeignCall],
    ( {compound(Head)}
    ->findall('    '+SetArg+';',
              ( arg(_, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [out, inout]),
                c_var_name(Arg, CArg),
                c_set_argument(Spec, Mode, CArg, Arg, SetArg)
              ))
    ; []
    ).

invert_mode(in, out).
invert_mode(out, in).
invert_mode(inout, inout).

bind_outs_arguments(Head, M, CM, Comp, Call, Succ, Glob, (_ as _/BN +_)) -->
    findall('    '+("~s"-Decl)+Line,
            ( memberchk(returns(Arg, _), Glob)
            ->bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
              memberchk(Mode, [out, inout]),
              ctype_arg_decl(Spec, Mode, Decl, []),
              ( Spec = term
              ->Line=" "+Arg+"=PL_new_term_ref();"
              ; Line=" "+Arg+";"
              )
            )),
    ( {compound(Head)}
    ->findall(['    term_t ~w=~w_args + ~d;'-[PArg, BN, Idx-1],
               '    '+SetArg+';'],
              ( arg(Idx, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [in, inout]),
                invert_mode(Mode, InvM),
                ( Mode = inout
                ->atom_concat('*', Arg, CArg)
                ; CArg = Arg
                ),
                format(atom(PArg), '_p_~w', [Arg]),
                c_set_argument(Spec, InvM, CArg, PArg, SetArg)
              ))
    ; []
    ),
    {CallPred = 'PL_call_predicate(__~w, PL_Q_NORMAL, ~w, ~w_args)'-[CM, BN, BN]},
    ( {memberchk(returns_state(_), Glob)}
    ->['    int __result = '+CallPred+';']
    ; ['    __rtcwarn('+CallPred+');']
    ),
    ( {compound(Head)}
    ->findall(['    term_t ~w=~w_args + ~d;'-[PArg, BN, Idx-1],
               '    '+SetArg+';'],
              ( arg(Idx, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [out, inout]),
                invert_mode(Mode, InvM),
                ( memberchk(returns(Arg, _), Glob)
                ->atom_concat('&', Arg, CArg)
                ; CArg = Arg
                ),
                format(atom(PArg), '_p_~w', [Arg]),
                c_get_argument(Spec, InvM, CArg, PArg, SetArg)
              )),
      ( { memberchk(returns(Arg, _), Glob)
        ; memberchk(returns_state(_), Glob),
          Arg = '__result'
        }
      ->["    return "+Arg+";"]
      ; []
      )
    ; []
    ).

generate_foreign_call((CN/_A as _ + _)-Head, M, CM, Comp, Call, Succ, Glob, Return,
                      '    '+Line+CN+'('+MR+FC+');') :-
    ( member(RS, [returns_state(_), type(_)]),
      memberchk(RS, Glob)
    ->Line='foreign_t __result=',
      CHead = Head,
      Return = '__result'
    ; ( member(returns(Var, _), Glob)
      ->c_var_name(Var, CVar),
        Line='~w='-[CVar],
        Head =.. Args,
        once(select(Var, Args, CArgs)),
        CHead =.. CArgs
      ; CHead = Head,
        Line=''
      ),
      ( member(no_exception, Glob)
      ->Return = 'TRUE'
      ; Return = '!PL_exception(0)'
      )
    ),
    ( memberchk(memory_root(_), Glob)
    ->MR='__root, '
    ; MR=''
    ),
    ( compound(CHead)
    ->generate_foreign_call_(1, CHead, M, CM, Comp, Call, Succ, Glob, FC)
    ; FC=''
    ).

generate_foreign_call_(N, Head, M, CM, Comp, Call, Succ, Glob, Sep+Deref+CArg+FC) :-
    arg(N, Head, Arg),
    (N \= 1 -> Sep = ', ' ; Sep = ''),
    bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
    c_var_name(Arg, CArg),
    ( Mode = in,
      \+ is_type(Spec)
    ->Deref = ''
    ; Deref = '&'
    ),
    N1 is N + 1,
    !,
    generate_foreign_call_(N1, Head, M, CM, Comp, Call, Succ, Glob, FC).
generate_foreign_call_(_, _, _, _, _, _, _, _, '').

:- use_module(library(sequence_list)).
:- use_module(library(prolog_clause), []).

get_dictionary(Term, File, Line, M, Dict) :-
    ( prolog_clause:read_term_at_line(File, Line, M, RawTerm1, _TermPos, Dict),
      ( RawTerm1 \= (_ :- _)
      ->RawTerm = (RawTerm1 :- true)
      ; RawTerm1 = RawTerm
      ),
      subsumes(RawTerm, Term) -> true
    ; Dict = []
    ).

match_known_type(Prop, M, Name, Spec, Arg) :-
    match_known_type_(Prop, M, Name, Spec, Arg), !.

match_known_type_(M:Prop,       _, Name, Spec, Arg) :-
    match_known_type(Prop, M, Name, Spec, Arg).
match_known_type_(atm(A),       _, _, chrs('char*'), A).
match_known_type_(atom(A),      _, _, chrs('char*'), A).
match_known_type_(ptr(Type, A), M, N, ptr(Spec), A) :-
    nonvar(Type),
    Type =.. [F|Args],
    Prop =.. [F, E|Args],
    match_known_type_(Prop, M, N, Spec, E).
% match_known_type_(string(A),        _, _, string_chars-'char*', A).
match_known_type_(ptr(A),            _, _, pointer-'void*', A).
match_known_type_(long(A),           _, _, long-long,       A).
match_known_type_(int(A),            _, _, integer-int,     A).
match_known_type_(nnegint(A),        _, _, integer-'unsigned int', A).
match_known_type_(integer(A),        _, _, integer-int,     A).
match_known_type_(character_code(A), _, _, char_code-char,  A).
match_known_type_(char(A),           _, _, char-char,       A).
match_known_type_(num(A),            _, _, float-double,    A).
match_known_type_(float_t(A),        _, _, float_t-float,   A).
match_known_type_(number(A),         _, _, float-double,    A).
match_known_type_(term(A),           _, _, term,            A).
match_known_type_(list(Type, A),     M, N, list(Spec),      A) :-
    nonvar(Type),
    extend_args(Type, [E], Prop),
    match_known_type_(Prop, M, N, Spec, E).
match_known_type_(dict_t(Desc, A), _, Name, type(Type), A) :-
    is_dict(Desc, Tag),
    !,
    atomic_list_concat([Name, '_', Tag], Type).
match_known_type_(Type, M, _, tdef(Name, Spec), A) :-
    type_is_tdef(M, Type, Spec, A),
    functor(Type, Name, _),
    !.
match_known_type_(Type, M, _, Spec, A) :-
    compound(Type),
    functor(Type, Name, Arity),
    arg(Arity, Type, A),
    functor(Head, Name, Arity),
    type_props(M, Head, TypePropLDictL, _, _),
    ( TypePropLDictL = [t(_, [], _)]
    ->Spec=cdef(Name)
    ; Spec=type(Name)
    ),
    !.

type_is_tdef(M, Type, Spec, A) :-
    compound(Type),
    functor(Type, TName, Arity),
    arg(Arity, Type, A),
    functor(Head, TName, Arity),
    type_props_(M, Head, _, _, Asr),
    \+ curr_prop_asr(comp, _, _, Asr),
    bind_type_names(M:Head, TypeMPropLDictL),
    TypeMPropLDictL = [t(Head, [Prop], _)],
    arg(Arity, Head, A),
    arg(Arity, Prop, B),
    A==B,
    match_known_type_(Prop, M, TName, Spec, A),
    !.

bind_argument(Head, M, CM, CompL, CallL, SuccL, GlobL, Arg, Spec, Mode) :-
    functor(Head, Name, _),
    ( member(Comp, CompL),
      match_known_type(Comp, CM, Name, Spec, Arg1),
      Arg1 == Arg
    ->true
    ; true
    ),
    ( member(Call, CallL),
      match_known_type(Call, CM, Name, Spec, Arg1),
      Arg1 == Arg
    ->Mode = in
    ; true
    ),
    ( member(Succ, SuccL),
      match_known_type(Succ, CM, Name, Spec, Arg1),
      Arg1 == Arg
    ->Mode = out
    ; true
    ),
    ( memberchk(type(_), GlobL),
      match_known_type(Head, M, Name, Spec, Arg1),
      Arg1 == Arg
    ->Mode = in
    ; true
    ),
    ignore(Mode = inout),
    ignore(Spec = term).

:- public call_idx/2.
:- meta_predicate call_idx(0, -).
call_idx(Call, Idx) :-
    findall(Ref, once(call_ref(Call, Ref)), [Ref]), % avoid unifications
    nth_clause(_, Idx, Ref).
