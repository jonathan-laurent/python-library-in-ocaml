import timeit


def profile(f, n=100_000):
    t0 = timeit.default_timer()
    for i in range(n):
        f()
    t1 = timeit.default_timer()
    return (t1 - t0) / n


def profile_normal():
    import mylib.ocaml_module as mod

    valuation = [("x", 5), ("y", 3)]
    expr = ("Add", (("Var", ("x",)), ("Var", ("y",))))
    time = profile(lambda: mod.eval(valuation, expr))
    print(f"Normal: {1e6*time:.2f} us")


def profile_dataclasses():
    import mylib.ocaml_module_dataclasses as mod

    valuation = [("x", 5), ("y", 3)]
    expr = mod.Add(mod.Var("x"), mod.Var("y"))
    time = profile(lambda: mod.eval(valuation, expr))
    print(f"Dataclasses: {1e6*time:.2f} us")


if __name__ == "__main__":
    profile_normal()
    profile_dataclasses()
