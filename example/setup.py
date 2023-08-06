import os
import shutil

from setuptools import setup
from os.path import join
from setuptools.command.build_py import build_py
import subprocess
import re


GENERATED_MODULE = "ocaml_module"


class build(build_py):
    def run(self):
        # Figure out the library name (avoiding a toml dependency)
        project_file = open("pyproject.toml", "r").read()
        lib_name = re.search(r'name\s*=\s*"([^"]+)"', project_file).group(1)
        # Use dune to build the OCaml library and copy it in the src/bin directory.
        proc = subprocess.run(["dune", "build", "--root", "ocaml"])
        assert proc.returncode == 0, "Error building the OCaml library."
        dune_build = join("ocaml", "_build", "default")
        # Copy the shared library binary
        dll = f"{GENERATED_MODULE}.so"
        bin_path = join("src", lib_name, "bin")
        os.makedirs(join(lib_name, "bin"), exist_ok=True)
        shutil.copy(join(dune_build, dll), bin_path)
        os.chmod(join(bin_path, dll), 0o666)
        # Generate stubs
        for ext in ["py", "pyi"]:
            generator = join(dune_build, f"{GENERATED_MODULE}.exe")
            cmd = [generator, f"generate-{ext}", "--lib-name", lib_name]
            proc = subprocess.run(cmd, text=True, stdout=subprocess.PIPE)
            if proc.returncode != 0:
                print(proc.stdout)
                print("Error generating Python stubs.")
            file = join("src", lib_name, f"{GENERATED_MODULE}.{ext}")
            with open(file, "w") as f:
                f.write(proc.stdout)
            if shutil.which("black") is not None:
                subprocess.run(["black", file])


setup(cmdclass={"build_py": build}, setup_requires=["black"])
