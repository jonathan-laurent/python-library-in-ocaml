import os
import shutil

from setuptools import setup
from os.path import join
from setuptools.command.build_py import build_py
import subprocess
import re


def build_library(generated_module: str, use_dataclasses: bool):
    # Figure out the library name (avoiding a toml dependency)
    project_file = open("pyproject.toml", "r").read()
    if (m := re.search(r'name\s*=\s*"([^"]+)"', project_file)) is None:
        print("Unable to infer the library name.")
        exit(1)
    lib_name = m.group(1)
    # Use dune to build the OCaml library and copy it in src/bin
    proc = subprocess.run(["dune", "build", "--root", "ocaml"])
    assert proc.returncode == 0, "Error building the OCaml library."
    dune_build = join("ocaml", "_build", "default")
    # Copy the shared library binary
    dll = f"ocaml_module.so"
    bin_path = join("src", lib_name, "bin")
    os.makedirs(bin_path, exist_ok=True)
    shutil.copy(join(dune_build, dll), bin_path)
    os.chmod(join(bin_path, dll), 0o666)
    # Generate the Python stubs
    for ext in ["py", "pyi"]:
        generator = join(dune_build, f"ocaml_module.exe")
        cmd = [generator, f"generate-{ext}", "--lib-name", lib_name]
        if use_dataclasses:
            cmd.append("--use-dataclasses")
        proc = subprocess.run(cmd, text=True, stdout=subprocess.PIPE)
        if proc.returncode != 0:
            print(proc.stdout)
            print("Error generating Python stubs.")
        file = join("src", lib_name, f"{generated_module}.{ext}")
        with open(file, "w") as f:
            f.write(proc.stdout)
        # Format the stubs with black
        if shutil.which("black") is not None:
            subprocess.run(["black", file])



class build(build_py):
    def run(self):
        build_library("ocaml_module", use_dataclasses=False)
        build_library("ocaml_module_dataclasses", use_dataclasses=True)


setup(cmdclass={"build_py": build}, setup_requires=["black"])
