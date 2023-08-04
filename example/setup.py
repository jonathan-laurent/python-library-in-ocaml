import os
import shutil

from setuptools import setup
from setuptools.command.build_py import build_py
import subprocess


class build(build_py):
    def run(self):
        # We use dune to build the OCaml library and copy it in the src/bin directory.
        proc = subprocess.run(["dune", "build", "--root", "ocaml"])
        if proc.returncode != 0:
            print("Error building OCaml library")
            exit(1)
        bin_path = os.path.join("src", "mylib", "bin")
        dune_so = os.path.join("ocaml", "_build", "default", "mylib_ocaml.so")
        os.makedirs("mylib/bin", exist_ok=True)
        shutil.copy(dune_so, bin_path)
        os.chmod(os.path.join(bin_path, "mylib_ocaml.so"), 0o666)


setup(cmdclass={"build_py": build})
