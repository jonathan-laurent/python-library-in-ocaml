def test_normal():
    import mylib.ocaml_module as mod

    print(mod.fact(5))

    valuation = [("x", 5), ("y", 3)]
    expr = ("Add", (("Var", ("x",)), ("Var", ("y",))))
    print(mod.eval(valuation, expr))
    print(mod.example_expr)

    print(mod.custommer_data("Homer Simpson"))
    print(mod.custommer_data("Flanders"))

    print(mod.log(10))
    print(mod.log(2, base=2))


def test_dataclasses():
    import mylib.ocaml_module_dataclasses as mod

    print(mod.fact(5))

    valuation = [("x", 5), ("y", 3)]
    expr = mod.Add(mod.Var("x"), mod.Var("y"))
    print(mod.eval(valuation, expr))
    print(mod.example_expr)

    print(mod.custommer_data("Homer Simpson"))
    print(mod.custommer_data("Flanders"))


if __name__ == "__main__":
    test_normal()
    print()
    test_dataclasses()
