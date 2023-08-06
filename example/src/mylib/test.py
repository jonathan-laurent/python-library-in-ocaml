import mylib.ocaml_module as mod

print(f"{mod.fact(5)=}")

valuation = [("x", 5), ("y", 3)]
expr = ("Add", (("Var", ("x",)), ("Var", ("y",))))
print(f"{mod.eval(valuation, expr)=}")
