# Autogenerated by python_library_in_ocaml

from typing import TypeAlias, Literal

Expr: TypeAlias = (
    tuple[Literal["Constant"], tuple[int]]
    | tuple[Literal["Var"], tuple[str]]
    | tuple[Literal["Add"], tuple["Expr", "Expr"]]
)

def eval(valuation: list[tuple[str, int]], expr: "Expr") -> int | None:
    """
    Evaluate an expression given a valuation that maps variables
    to values. Return None if a variable does not appear in the valuation.
    """
    ...

def fact(n: int) -> int: ...
def f(x: int) -> int: ...
