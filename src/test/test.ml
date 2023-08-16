open Base
open Stdio
open Python_libgen

type enum_type = A | B [@@deriving python, python_export_type]

type simple_alias = int * (string * float)
[@@deriving python, python_export_type]

type sum_type =
  | C of bool * string
  | D of enum_type
  | E of { x : int; y : bool }
[@@deriving python, python_export_type]

type type_with_lists = L of int option list
[@@deriving python, python_export_type]

type record_type = { x : int; y : float option }
[@@deriving python, python_export_type]

type record_type_alias = record_type = { x : int; y : float option }
[@@deriving python, python_export_type]

type ('a, 'b) polymorphic = { x : 'a; y : 'b }
[@@deriving python, python_export_type]

module M = struct
  type loc = int * int * int * int [@@deriving python, python_export_type]

  type 'a with_loc = { data : 'a; loc : loc }
  [@@deriving python, python_export_type]
end

type located_name = string M.with_loc [@@deriving python, python_export_type]

let%python_export f (x : int) : int = x + 1

let%python_export rec fact (n : int) : int =
  if n <= 0 then 1 else n * fact (n - 1)

let%python_export two_poly (a : int) (b : string) :
    (int, string) polymorphic * (string, int) polymorphic =
  ({ x = a; y = b }, { x = b; y = a })

let%python_export list_poly (a : int) (b : string) :
    (int, string) polymorphic list =
  [ { x = a; y = b }; { x = a; y = b } ]

let%python_docstring fact =
  {|
      Compute the factorial of an integer number.
      Return 1 on negative inputs.
  |}

let%python_export sum (l : int list) : int = List.fold_left ~f:( + ) ~init:0 l
let%python_export make_record (x : int) : record_type = { x; y = None }

let%test_unit "functions preserved" =
  assert (f 5 = 6);
  assert (fact 3 = 6);
  assert (sum [ 1; 2; 3 ] = 6)

let%expect_test "registered types" =
  let open Python_libgen.Repr in
  List.iter (registered_python_types ()) ~f:(fun v ->
      printf !"%{show_type_declaration}\n\n" v);
  [%expect
    {|
    { Repr.type_name = "EnumType"; type_vars = [];
      definition = (Repr.Enum ["A"; "B"]) }

    { Repr.type_name = "SimpleAlias"; type_vars = [];
      definition =
      (Repr.Alias
         (Repr.Tuple
            [(Repr.App (Repr.Int, []));
              (Repr.Tuple
                 [(Repr.App (Repr.String, [])); (Repr.App (Repr.Float, []))])
              ]))
      }

    { Repr.type_name = "SumType"; type_vars = [];
      definition =
      (Repr.Variant
         [("C",
           (Repr.Anonymous
              [(Repr.App (Repr.Bool, [])); (Repr.App (Repr.String, []))]));
           ("D", (Repr.Anonymous [(Repr.App ((Repr.Custom "EnumType"), []))]));
           ("E",
            (Repr.Labeled
               [("x", (Repr.App (Repr.Int, [])));
                 ("y", (Repr.App (Repr.Bool, [])))]))
           ])
      }

    { Repr.type_name = "TypeWithLists"; type_vars = [];
      definition =
      (Repr.Variant
         [("L",
           (Repr.Anonymous [(Repr.List (Repr.Option (Repr.App (Repr.Int, []))))]))
           ])
      }

    { Repr.type_name = "RecordType"; type_vars = [];
      definition =
      (Repr.Record
         [("x", (Repr.App (Repr.Int, [])));
           ("y", (Repr.Option (Repr.App (Repr.Float, []))))])
      }

    { Repr.type_name = "RecordTypeAlias"; type_vars = [];
      definition =
      (Repr.Record
         [("x", (Repr.App (Repr.Int, [])));
           ("y", (Repr.Option (Repr.App (Repr.Float, []))))])
      }

    { Repr.type_name = "Polymorphic"; type_vars = ["A"; "B"];
      definition = (Repr.Record [("x", (Repr.Tvar "A")); ("y", (Repr.Tvar "B"))])
      }

    { Repr.type_name = "Loc"; type_vars = [];
      definition =
      (Repr.Alias
         (Repr.Tuple
            [(Repr.App (Repr.Int, [])); (Repr.App (Repr.Int, []));
              (Repr.App (Repr.Int, [])); (Repr.App (Repr.Int, []))]))
      }

    { Repr.type_name = "WithLoc"; type_vars = ["A"];
      definition =
      (Repr.Record
         [("data", (Repr.Tvar "A"));
           ("loc", (Repr.App ((Repr.Custom "Loc"), [])))])
      }

    { Repr.type_name = "LocatedName"; type_vars = [];
      definition =
      (Repr.Alias
         (Repr.App ((Repr.Custom "WithLoc"), [(Repr.App (Repr.String, []))])))
      } |}]

let test ~use_dataclasses =
  let values = registered_python_values () in
  let types = registered_python_types () in
  let settings = Stubs.{ use_dataclasses } in
  print_endline
    (Stubs.generate_py_stub ~interface_only:false ~settings ~generated:"core"
       ~lib_name:"mylib" ~values ~types)

let%expect_test "python stub without dataclasses" =
  test ~use_dataclasses:false;
  [%expect
    {|
    # Autogenerated by python-libgen

    from ctypes import RTLD_LOCAL, PyDLL, c_char_p

    from importlib.resources import files, as_file

    DLL_NAME = "core.so"

    dll_resource = files("mylib.bin").joinpath(DLL_NAME)
    with as_file(dll_resource) as dll_file:
        dll = PyDLL(str(dll_file), RTLD_LOCAL)

    argv_t = c_char_p * 3
    argv = argv_t(DLL_NAME.encode("utf-8"), b"register", None)
    dll.caml_startup(argv)

    import _core_internals  # type: ignore


    from typing import Generic, Literal, TypeAlias, TypeVar, TypedDict, Union

    A = TypeVar("A")

    B = TypeVar("B")

    EnumType: TypeAlias = Union[tuple[Literal["A"], None], tuple[Literal["B"], None]]

    SimpleAlias: TypeAlias = tuple[int, tuple[str, float]]

    SumType: TypeAlias = Union[tuple[Literal["C"], tuple[bool, str]], tuple[Literal["D"], tuple[EnumType]], tuple[Literal["E"], tuple[int, bool]]]

    TypeWithLists: TypeAlias = tuple[Literal["L"], tuple[list[int | None]]]

    class RecordType(TypedDict, total=True):
        x: int
        y: float | None

    class RecordTypeAlias(TypedDict, total=True):
        x: int
        y: float | None

    class Polymorphic(TypedDict, Generic[A, B], total=True):
        x: A
        y: B

    Loc: TypeAlias = tuple[int, int, int, int]

    class WithLoc(TypedDict, Generic[A], total=True):
        data: A
        loc: Loc

    LocatedName: TypeAlias = WithLoc[str]

    def f(x: int) -> int:
        return _core_internals.f(x)

    def fact(n: int) -> int:
        """
        Compute the factorial of an integer number.
        Return 1 on negative inputs.
        """
        return _core_internals.fact(n)

    def two_poly(a: int, b: str) -> tuple[Polymorphic[int, str], Polymorphic[str, int]]:
        return _core_internals.two_poly(a, b)

    def list_poly(a: int, b: str) -> list[Polymorphic[int, str]]:
        return _core_internals.list_poly(a, b)

    def sum(l: list[int]) -> int:
        return _core_internals.sum(l)

    def make_record(x: int) -> RecordType:
        return _core_internals.make_record(x) |}]

let%expect_test "python stub with dataclasses" =
  test ~use_dataclasses:true;
  [%expect
    {|
    # Autogenerated by python-libgen

    from ctypes import RTLD_LOCAL, PyDLL, c_char_p

    from importlib.resources import files, as_file

    DLL_NAME = "core.so"

    dll_resource = files("mylib.bin").joinpath(DLL_NAME)
    with as_file(dll_resource) as dll_file:
        dll = PyDLL(str(dll_file), RTLD_LOCAL)

    argv_t = c_char_p * 3
    argv = argv_t(DLL_NAME.encode("utf-8"), b"register", None)
    dll.caml_startup(argv)

    import _core_internals  # type: ignore


    from dataclasses import dataclass

    from enum import Enum

    from typing import Generic, TypeAlias, TypeVar, Union

    A = TypeVar("A")

    B = TypeVar("B")

    class EnumType(Enum):
        A = "A"
        B = "B"

    def _ocaml_of_EnumType(x):
        return x.value

    def _EnumType_of_ocaml(x):
        return EnumType.A if x == "A" else EnumType.B if x == "B" else NotImplemented

    SimpleAlias: TypeAlias = tuple[int, tuple[str, float]]

    def _ocaml_of_SimpleAlias(x):
        return x

    def _SimpleAlias_of_ocaml(x):
        return x

    @dataclass
    class C:
        arg1: bool
        arg2: str

    @dataclass
    class D:
        arg: EnumType

    @dataclass
    class E:
        x: int
        y: bool

    SumType: TypeAlias = Union[C, D, E]

    def _ocaml_of_SumType(x):
        return ("C", (x.arg1, x.arg2)) if isinstance(x, C) else ("D", (_ocaml_of_EnumType(x.arg),)) if isinstance(x, D) else ("E", x.__dict__) if isinstance(x, E) else NotImplemented

    def _SumType_of_ocaml(x):
        return C(arg1=x[1][0], arg2=x[1][1]) if x[0] == "C" else D(arg=_EnumType_of_ocaml(x[1][0])) if x[0] == "D" else E(**x[1]) if x[0] == "E" else NotImplemented

    @dataclass
    class L:
        arg: list[int | None]

    TypeWithLists: TypeAlias = L

    def _ocaml_of_TypeWithLists(x):
        return ("L", (x.arg,)) if isinstance(x, L) else NotImplemented

    def _TypeWithLists_of_ocaml(x):
        return L(arg=x[1][0]) if x[0] == "L" else NotImplemented

    @dataclass
    class RecordType:
        x: int
        y: float | None

    def _ocaml_of_RecordType(x):
        return x.__dict__

    def _RecordType_of_ocaml(x):
        return RecordType(**x)

    @dataclass
    class RecordTypeAlias:
        x: int
        y: float | None

    def _ocaml_of_RecordTypeAlias(x):
        return x.__dict__

    def _RecordTypeAlias_of_ocaml(x):
        return RecordTypeAlias(**x)

    @dataclass
    class Polymorphic(Generic[A, B]):
        x: A
        y: B

    def _ocaml_of_Polymorphic(x, _ocaml_of_A, _ocaml_of_B):
        return {"x": _ocaml_of_A(x.x), "y": _ocaml_of_B(x.y)}

    def _Polymorphic_of_ocaml(x, _A_of_ocaml, _B_of_ocaml):
        return Polymorphic(x=_A_of_ocaml(x["x"]), y=_B_of_ocaml(x["y"]))

    Loc: TypeAlias = tuple[int, int, int, int]

    def _ocaml_of_Loc(x):
        return x

    def _Loc_of_ocaml(x):
        return x

    @dataclass
    class WithLoc(Generic[A]):
        data: A
        loc: Loc

    def _ocaml_of_WithLoc(x, _ocaml_of_A):
        return {"data": _ocaml_of_A(x.data), "loc": _ocaml_of_Loc(x.loc)}

    def _WithLoc_of_ocaml(x, _A_of_ocaml):
        return WithLoc(data=_A_of_ocaml(x["data"]), loc=_Loc_of_ocaml(x["loc"]))

    LocatedName: TypeAlias = WithLoc[str]

    def _ocaml_of_LocatedName(x):
        return _ocaml_of_WithLoc(x, (lambda _x: _x))

    def _LocatedName_of_ocaml(x):
        return _WithLoc_of_ocaml(x, (lambda _x: _x))

    def f(x: int) -> int:
        return _core_internals.f(x)

    def fact(n: int) -> int:
        """
        Compute the factorial of an integer number.
        Return 1 on negative inputs.
        """
        return _core_internals.fact(n)

    def two_poly(a: int, b: str) -> tuple[Polymorphic[int, str], Polymorphic[str, int]]:
        _ret = _core_internals.two_poly(a, b)
        return (_Polymorphic_of_ocaml(_ret[0], (lambda _x: _x), (lambda _x: _x)), _Polymorphic_of_ocaml(_ret[1], (lambda _x: _x), (lambda _x: _x)))

    def list_poly(a: int, b: str) -> list[Polymorphic[int, str]]:
        _ret = _core_internals.list_poly(a, b)
        return [_Polymorphic_of_ocaml(_x, (lambda _x: _x), (lambda _x: _x)) for _x in _ret]

    def sum(l: list[int]) -> int:
        return _core_internals.sum(l)

    def make_record(x: int) -> RecordType:
        _ret = _core_internals.make_record(x)
        return _RecordType_of_ocaml(_ret) |}]
