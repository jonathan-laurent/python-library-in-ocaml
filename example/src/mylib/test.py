def test_normal():
    import mylib.ocaml_module as mod

    print(mod.fact(5))

    valuation = [("x", 5), ("y", 3)]
    expr = ("Add", (("Var", ("x",)), ("Var", ("y",))))
    print(mod.eval(valuation, expr))

    print(mod.custommer_data("Homer Simpson"))
    print(mod.custommer_data("Flanders"))


def test_dataclasses():
    from mylib.ocaml_module_dataclasses import fact, Add, Var, eval, custommer_data

    print(fact(5))

    valuation = [("x", 5), ("y", 3)]
    expr = Add((Var("x"), Var("y")))
    print(eval(valuation, expr))

    print(custommer_data("Homer Simpson"))
    print(custommer_data("Flanders"))


if __name__ == '__main__':
    test_normal()
    print()
    test_dataclasses()