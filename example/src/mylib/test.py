import mylib.ocaml_module as mod

print(mod.fact(5))

valuation = [("x", 5), ("y", 3)]
expr = ("Add", (("Var", ("x",)), ("Var", ("y",))))
print(mod.eval(valuation, expr))

print(mod.custommer_data("Homer Simpson"))
print(mod.custommer_data("Flanders"))